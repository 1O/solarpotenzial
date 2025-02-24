
## behält nur Pixelsammlungen über `minsize` Mindestausdehnung:
clean_raster <- \(r){
  im <- matrix(r, dim(r)) |> as.cimg() 
  values(r) <- as.pixset(!is.na(im)) |> clean(constants$minsize) |> as.matrix() |> t()
  r <- subst(r, FALSE, NA)
  r
}


## braucht knapp 7 Sekunden; stattdessen `clean_raster` verwenden
keep_large_blocks <- \(r){
  x <- ncol(r); y <- nrow(r)
  im <- matrix(r, ncol = x, nrow = y) |> 
    as.cimg() |> label()
  sizes <- table(im)
  is_large <- im %in% as.integer(names(sizes)[sizes >= constants$minsize])
  values(r) <- values(r) * c(NA, 1)[1 + is_large]
  r
}


## prüft ob mindestens ein Gebäude (OBJECTID) in der Kachel zu liegen kommt.
## Falls ja, wird der gesamte Gebäudelayer als Raster zurückgegeben, 
## andernfalls FALSE zum Abbruch weiterer Berechnungen für diese Kachel
get_building_raster_or_skip <- \(r){
  v_buildings <- 
    ## Gebäudepolygone innerhalb des DOM-Rasters:
    query(v_buildings_austria, extent = r) 
  ifelse(length(v_buildings),
         return(v_buildings |>
                  buffer(constants$buffer) |> 
                  rasterize(y = r, field = 'OBJECTID', touches = FALSE)
         )
         ,
         FALSE
  )
}



## liest Raster ein, stellt zusätzliche Raster her,
## dazu gehört auch das kachelweise Einlesen und Rastern der Gebäude-Polygone
prepare_rasters <- \(dir_root = '.', tile_code, keep = FALSE){
  rasters <- list()
  
  ## DOM einlesen, vorerst nur, um den tile extent zu bestimmen
  rasters$dom_full <- rast(file.path(dir_root, sprintf('input/%s/%s_DOM.tif',
                                                       tile_code, tile_code)
  )
  )
  
  
  ## mit gepuffertem, rasterisierten Gebäudevektor starten, alle weiteren
  ## Raster werden gleich damit maskiert:
  v_buildings <- 
    ## Gebäudepolygone innerhalb des DOM-Rasters:
    query(v_buildings_austria, extent = rasters$dom_full) |> 
    buffer(constants$buffer)
  
  ## Gebäudepolygone als Raster "buildings", Auflösung und Extent vom DOM:
  rasters$buildings <- v_buildings |>
    rasterize(y = rasters$dom, field = 'OBJECTID', touches = FALSE)
  
  ## mit Fehler abbrechen, wenn weniger als `constants$minbuildings` Gebäude
  ## (OBJECTIDS)
  
  n_buildings <- rasters$buildings |> values() |> unique() |> length()
  enough_buildings <- n_buildings >= constants$minbuildings
  stopifnot("too few buildings in tile" = enough_buildings) |>  try()
  

  
  
  ## Oberflächen-Höhenmodell:
  rasters$dom <- rasters$dom_full
  set.names(rasters$dom, 'dom')
  rasters$dom_full <- NULL
  ## Globalstrahlung:
  rasters$glo <- rast(
    file.path(dir_root,
              sprintf('input/%s/GLO_real/%s_GLO_real_Jahressumme.tif',
                      tile_code, tile_code))
  )
  
  ## 8 Himmelsrichtungen, von Nord (0) bis Nordwest (7) im UZS
  rasters$aspect <- rasters$dom |> terrain('aspect', neighbors = 4) |>
    classify(rcl = cbind(c(0:8 * 45) - 22.5,
                         c(0:8 * 45) + 22.5,
                         c(0, 1:7, 0)
    )
    )
  set.names(rasters$aspect, 'aspect')
  
  rasters$slope <- rasters$dom |> terrain('slope', neighbors = 4)
  
  # set.values(rasters$slope, as.integer(values(rasters$slope)))
  set.names(rasters$slope, 'deg')
  
  ## Dachtyp (flach oder geneigt)
  rasters$rooftype <- rasters$slope |>
    classify(rcl = cbind(c(-Inf, constants$flat),
                         c(constants$flat, Inf),
                         0:1)
    )
  set.names(rasters$rooftype, 'inclined')
  
  ## Flächenkorrektur für geneigte Flächen: tatsächliche Dachfläche steigt
  ## mit der Steilheit
  rasters$a <- 1/cos(pi/180 * rasters$slope)
  set.names(rasters$a, 'area')

  ## PV-Ertrag aus Globalstrahlung real und Fläche:
  rasters$harvest_pv <- rasters$a * constants$pv_e_f(rasters$glo)
  set.names(rasters$harvest_pv, 'ertrag_PV')
  
  ## PV-Ertrag aus Globalstrahlung real und Fläche:
  rasters$harvest_st <- rasters$a * constants$st_e * rasters$glo
  set.names(rasters$harvest_st, 'ertrag_ST')
  
  ## Raster mit eignung_solar hinzufügen und Kategorielabels setzen:
  rasters$suit <- rasters$glo |> classify(cbind(
    head(constants$intervals_solar, -1), ## von
    tail(constants$intervals_solar, -1), ## bis
    0:5 ## Klassenindex (0 = ungeeignet, 5 = sehr gut geeignet)
  )
  )
  
  set.names(rasters$suit, 'suit')
  
  speckle_mask <- clean_raster(rasters$buildings)
  rasters <- Map(rasters, f = \(r) mask(r, speckle_mask))
  rasters
}


## Mit `zonal` können mehrere Statistiken pro Zone (hier: Gebäude) berechnet
## und als Liste zurückgegeben werden. Diese Funktion macht aus der Liste
## separate Spalten im dataframe. Zeitfresser.
get_zonal_wide <- \(r_in, r_of, slug = NULL){
  if(!is.null(slug)) set.names(r_in, slug)
  values(r_in) <- as.character(values(r_in))
  (zonal(r_in, r_of, table) |> as.matrix())[, -1]
}

## Rasterwerte nach zwei kombinierten Zonenrastern summieren und umformen.
## die Kategorien aus `zones_1` ergeben die
get_areas_wide <- \(r, zones_1, zones_2, labels_wide = NULL){
  ## kombiniert Zonen; der Multiplikator für die Rasterwerte der 2. Zone sorgt
  ## für eindeutige Werte, die später wieder durch Mod-division und Mod den
  ## jeweiligen Kategorien von Raster 1 und 2 zugeordnet werden können.
  zones <- 100 * zones_1 + zones_2
  tmp <- zonal(r, zones, sum)
  tmp <- cbind(tmp, tmp[1] %/% 100, tmp[1] %% 100) |> 
    setNames(c(names(tmp), names(zones_2), names(zones_1))) |>  
    select(-1) |>
    pivot_wider(names_from = names(zones_1),
                values_from = names(r)
    )
  names(tmp)[1] <- names(zones_1)
  names(tmp) <- paste(names(zones_2), names(tmp), sep='_')
  tmp
}


## Rasterwerte extrahieren und als dataframe zurückgeben:
extract_rasters <- \(rasters, iqr_mult = 2){
    ## Spalten mit OBJECTID und DOM-Ausreißer:
  outliers <- zonal(rasters$dom, rasters$buildings,
                    fun = \(xs){lb = quantile(xs, .25, na.rm = TRUE)
                    ub = quantile(xs, .75, na.rm = TRUE)
                    bw = iqr_mult * (ub - lb)
                    sum((xs < lb - bw) | (xs > ub + bw), na.rm = TRUE)
                    }
  ) |> setNames(c('OBJECTID', 'n_outliers'))
  
  ## Statistiken für DOM:
  dom_stats <- zonal(rasters$dom, rasters$buildings,
                     fun = \(xs){sapply(c(min = min, mean = mean, sd = sd, max = max),
                                        \(fn) do.call(fn, list(xs, na.rm = TRUE)))
                     }
  ) |> as.matrix() |> as.data.frame()
  ## Ausrichtung
  aspects <- get_areas_wide(rasters$a, rasters$buildings, rasters$aspect)
  names(aspects)[1] <- 'OBJECTID'
  ## Dachneigung (flat = 0, inclined = 1)
  rooftypes <- get_areas_wide(rasters$a, rasters$buildings, rasters$rooftype) |> 
    setNames(nm = c('OBJECTID', 'flat', 'inclined'))
  ## Eignungsklassen
  suitabilities <- get_areas_wide(rasters$a, rasters$buildings, rasters$suit)
  names(suitabilities)[1] <- 'OBJECTID'
  
  ## Ertrag PV
  harvest_pv <- zonal(rasters$harvest_pv, rasters$buildings)
  ## Ertrag Solarthermie
  harvest_st <- zonal(rasters$harvest_st, rasters$buildings)
  
  ## alle zu data.frame joinen:
    Reduce(f = \(a, b) left_join(a, b, by = 'OBJECTID'),
         list(dom_stats, outliers, aspects, rooftypes,
              suitabilities, harvest_pv, harvest_st)
         )
}


## ergänzt die tabellierte Rasterinformation von `extract_rasters` um (auf Gebäudeebene)
## errechnete Werte:
enrich_extract <- \(d){ # d ist ein data.table
  tmp <- as.data.table(d)
  setnames(tmp, paste0('aspect_', 0:7), paste0('aspect.', constants$labels$aspect))
  setnames(tmp, paste0('suit_', 0:5), paste0('eign.', constants$labels$eignung_solar))
  setnames(tmp, names(tmp), gsub('NaN', 'unb', names(tmp)))
  
  # # ## possible module count
  # tmp[, `:=` (n_poss_modules = rowSums(.SD) * constants$a_usable / constants$modul_m2),
  #     #### diese Spalten für Berechnung:
  #     .SDcols = paste0('eign.', c('wenig_2020', 'geeignet', 'gut', 'sehr_gut'))
  # ]
  # ## high scenario 2020
  # tmp[, `:=` (e_pv_2020_high = rowSums(.SD) * constants$a_usable * constants$pv_e,
  #             e_st_2020_high = rowSums(.SD) * constants$a_usable * constants$st_e
  # ),
  # .SDcols = paste0('eign.', c('wenig_2020', 'geeignet', 'gut', 'sehr_gut'))
  # ]
  # 
  # ## high scenario 2040
  # tmp[, `:=` (e_pv_2040_high = rowSums(.SD) * constants$a_usable * constants$pv_e,
  #             e_st_2040_high = rowSums(.SD) * constants$a_usable * constants$st_e
  # ),
  # .SDcols = paste0('eign.', c('wenig_2020', 'wenig_2040', 'geeignet', 'gut', 'sehr_gut'))
  # ]
  # 
  # ## low scenarios 2020 & 2040 (identisch)
  # tmp[, `:=` (e_pv_2020_low = rowSums(.SD) * constants$a_usable * constants$pv_e,
  #             e_st_2020_low = rowSums(.SD) * constants$a_usable * constants$st_e,
  #             e_pv_2040_low = rowSums(.SD) * constants$a_usable * constants$pv_e,
  #             e_st_2040_low = rowSums(.SD) * constants$a_usable * constants$st_e
  # ),
  # .SDcols = paste0('eign.', c('gut', 'sehr_gut'))
  # ]
  
  tmp[, OBJECTID:=NULL]
  tmp |> as.data.frame()
}


### Berechnung und Speicherung pro Kachel:
calc_and_save <- \(dir_root = '.', tile_code){
  cat('preparing rasters...')
  rasters <- prepare_rasters(dir_root, tile_code) ## Arbeitsraster anlegen

  tryCatch(
    d <- rasters |> 
      extract_rasters() ##|> ## Rasterwerte als data.frame extrahieren
    ## enrich_extract() ## zusätzliche Tabellenkalkulationen
    , error = \(e) cat('...can\'t extract data')
  )
  
  cat("...trying to write CSV...")
  ## Ergebnistabelle als CSV speichern:
  tryCatch( d |> write.csv2(file.path(dir_root, sprintf('output/data_%s.csv', tile_code))),
            error = \(e) cat('...writing failed')
  )
  cat("...trying to save tiffs...")
  ## Raster als Tiffs speichern:
  tryCatch({
    Map(names(rasters),
        f = \(n) writeRaster(rasters[[n]], 
                             sprintf('./output/%s_%s_%s.tiff', tile_code, n,
                                     as.character(constants$minsize)
                                     ),
                             overwrite = TRUE))
  },
  error = \(e) cat('...saving tiff failed')
  )
}


count_dom_outliers <- \(tile_code, buffer_size = -1, iqr_mult = 2){
  
  r_in <- rast(file.path(dir_root, sprintf('input/%s/%s_DOM.tif',
                                           tile_code, tile_code)
  )
  )
  
  v_buildings <- 
    query(v_buildings_austria, extent = ext(r_in)) |> 
    buffer(buffer_size)
  
  r_buildings <- v_buildings |> rasterize(y = r_in, field = 'OBJECTID')
  
  zonal(r_in, r_buildings,
        fun = \(xs){lb = quantile(xs, .25, na.rm = TRUE)
        ub = quantile(xs, .75, na.rm = TRUE)
        bw = iqr_mult * (ub - lb)
        sum((xs < lb - bw) | (xs > ub + bw), na.rm = TRUE)
        }
  )[, 2] |> sum()
}


show_dom_outliers <- \(r_in, buffer_size = -1, iqr_mult = 2){
  ## zeigt Ausreißer im DOM pro Gebäude an; dauert 10 s. oder länger
  v_buildings <- 
    query(v_buildings_austria, extent = ext(r_in)) |> 
    buffer(buffer_size)
  
  r_buildings <- v_buildings |> rasterize(y = r_in, field = 'OBJECTID')
  r_outliers <- zonal(r_in, r_buildings,
                      fun = \(xs){lb = quantile(xs, .25, na.rm = TRUE)
                      ub = quantile(xs, .75, na.rm = TRUE)
                      bw = iqr_mult * (ub - lb)
                      c(lb - bw, ub + bw)
                      },
                      as.raster = TRUE
                      
  ) 
  set.names(r_outliers, c('low', 'high'))
  (r_in < r_outliers$low | r_in > r_outliers$high) |>  
    clamp(1, 1, values = FALSE)  
}








