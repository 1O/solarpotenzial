get_mask_without_speckles <- \(r){
  ## entfernt alle Pixelhaufen < `min_cells`
  mat <- as.matrix(r, nrow(r), ncol(r))
  img <- as.cimg(t(mat), dim(r)[2], dim(r)[1])
  components <- label(img)
  sizes <- tabulate(components)
  r_clean <- as.cimg(components %in% 
                       which(sizes >= constants$min_cluster_size)) |> 
    as.matrix() |> t() |> rast()
  r_clean[r_clean == 0] <- NA
  crs(r_clean) <- crs(r)
  ext(r_clean) <- ext(r)
  r_clean
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
  tile_extent <- ext(rasters$dom_full)
  v_buildings <- 
    ## Gebäudepolygone innerhalb des DOM-Rasters:
    query(v_buildings_austria, extent = tile_extent) |> 
    buffer(constants$buffer)
  
  ## Gebäudepolygone als Raster "buildings", Auflösung und Extent vom DOM:
  rasters$buildings <- v_buildings |>
    rasterize(y = rasters$dom, field = 'OBJECTID')
  
  speckle_mask <- get_mask_without_speckles(rasters$buildings)
  rasters$buildings <- mask(rasters$buildings, speckle_mask)
  
  ## Oberflächen-Höhenmodell:
  rasters$dom <- rasters$dom_full |> mask(speckle_mask)
  set.names(rasters$dom, 'dom')
  rasters$dom_full <- NULL
  ## Globalstrahlung:
  rasters$glo <- rast(
    file.path(dir_root,
              sprintf('input/%s/GLO_real/%s_GLO_real_Jahressumme.tif',
                      tile_code, tile_code))
  ) |> 
    mask(rasters$buildings)
  
  ## 8 Himmelsrichtungen, von Nord (0) bis Nordwest (7) im UZS
  rasters$aspect <- rasters$dom |> terrain('aspect') |>
    classify(rcl = cbind(c(0:8 * 45) - 22.5,
                         c(0:8 * 45) + 22.5,
                         c(0, 1:7, 0)
    )
    )
  set.names(rasters$aspect, 'aspect')
  
  
  rasters$rooftype <- rasters$dom |> terrain('slope') |> 
    classify(rcl = cbind(c(-Inf, constants$flat),#
                         c(constants$flat, Inf),
                         0:1)
             )
  set.names(rasters$rooftype, 'inclined')
  
  
  ## Raster mit eignung_solar hinzufügen und Kategorielabels setzen:
  rasters$suit <- rasters$glo |> classify(cbind(
    head(constants$intervals_solar, -1), ## von
    tail(constants$intervals_solar, -1), ## bis
    0:5 ## Klassenindex (0 = ungeeignet, 5 = sehr gut geeignet)
  )
  )
  
  set.names(rasters$suit, 'suit')

  rasters
  
}

get_zonal_wide <- \(r_in, r_of, slug = NULL){
  if(!is.null(slug)) set.names(r_in, slug)
  values(r_in) <- as.character(values(r_in))
  (zonal(r_in, r_of, table) |> as.matrix())[, -1]
}

extract_rasters <- \(rasters, iqr_mult = 2){
  object_ids <- rasters$buildings |> values() |> unique() |> na.omit()
  res <- matrix(NA, length(object_ids),
                2 + 8,
                dimnames = list(NULL, NULL)
  )
  
    cbind(
      ## Spalten mit OBJECTID und DOM-Ausreißer:
    zonal(rasters$dom, rasters$buildings,
          fun = \(xs){lb = quantile(xs, .25, na.rm = TRUE)
          ub = quantile(xs, .75, na.rm = TRUE)
          bw = iqr_mult * (ub - lb)
          sum((xs < lb - bw) | (xs > ub + bw), na.rm = TRUE)
          }
    ) |> setNames(c('objectid', 'n_outliers')) |> as.matrix(),
    ## Statistiken für DOM:
    zonal(rasters$dom, rasters$buildings,
                  fun = \(xs){sapply(c(min = min, mean = mean, sd = sd, max = max),
                                     \(fn) do.call(fn, list(xs, na.rm = TRUE)))
                  }
    ),
    ## Ausrichtung
    get_zonal_wide(rasters$aspect, rasters$buildings),
    ## Dachneigung (flat = 0, inclined = 1)
    get_zonal_wide(rasters$rooftype, rasters$buildings),
    ## solare Eignungsklassen:
    get_zonal_wide(rasters$suit, rasters$buildings)
    )
  
  
  
}

## ergänzt die tabellierte Rasterinformation von `extract_rasters` um (auf Gebäudeebene)
## errechnete Werte:
enrich_extract <- \(d){ # d ist ein data.table
  tmp <- as.data.table(d)
  setnames(tmp, paste0('aspect.', 0:7), paste0('aspect.', constants$labels$aspect))
  setnames(tmp, names(tmp), gsub('suit', 'eign', names(tmp)))
  setnames(tmp, paste0('eign.', 0:5), paste0('eign.', constants$labels$eignung_solar))
  setnames(tmp, paste0('inclined.', 0:1), c('neig_flach', 'neig_schraeg'))
  setnames(tmp, names(tmp), gsub('NaN', 'unb', names(tmp)))

  # ## possible module count
  tmp[, `:=` (n_poss_modules = rowSums(.SD) * constants$a_usable / constants$modul_m2),
      #### diese Spalten für Berechnung:
      .SDcols = paste0('eign.', c('wenig_2020', 'geeignet', 'gut', 'sehr_gut'))
  ]
  ## high scenario 2020
  tmp[, `:=` (e_pv_2020_high = rowSums(.SD) * constants$a_usable * constants$pv_e,
              e_st_2020_high = rowSums(.SD) * constants$a_usable * constants$st_e
  ),
  .SDcols = paste0('eign.', c('wenig_2020', 'geeignet', 'gut', 'sehr_gut'))
  ]

  ## high scenario 2040
  tmp[, `:=` (e_pv_2040_high = rowSums(.SD) * constants$a_usable * constants$pv_e,
              e_st_2040_high = rowSums(.SD) * constants$a_usable * constants$st_e
  ),
  .SDcols = paste0('eign.', c('wenig_2020', 'wenig_2040', 'geeignet', 'gut', 'sehr_gut'))
  ]

  ## low scenarios 2020 & 2040 (identisch)
  tmp[, `:=` (e_pv_2020_low = rowSums(.SD) * constants$a_usable * constants$pv_e,
              e_st_2020_low = rowSums(.SD) * constants$a_usable * constants$st_e,
              e_pv_2040_low = rowSums(.SD) * constants$a_usable * constants$pv_e,
              e_st_2040_low = rowSums(.SD) * constants$a_usable * constants$st_e
  ),
  .SDcols = paste0('eign.', c('gut', 'sehr_gut'))
  ]
  
  tmp[, OBJECTID:=NULL]
  tmp |> as.data.frame()
}


### Berechnung und Speicherung pro Kachel:
calc_and_save <- \(dir_root = '.', tile_code){
  rasters <- prepare_rasters(dir_root, tile_code) ## Arbeitsraster anlegen
  d <- rasters |> 
    extract_rasters() |> ## Rasterwerte als data.frame extrahieren
    enrich_extract() ## zusätzliche Tabellenkalkulationen
  
  cat("...trying to write CSV...")
  ## Ergebnistabelle als CSV speichern:  
  d |> write.csv2(file.path(dir_root, sprintf('output/data_%s.csv', tile_code)))
  cat("...trying to save tiffs...")
  ## Raster als Tiffs speichern:
  Map(names(rasters), f = \(n) writeRaster(rasters[[n]], sprintf('./output/%s_%s.tiff', tile_code, n), overwrite = TRUE))
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








