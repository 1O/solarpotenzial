## liefert die benötigten Konstanten:
get_constants <- \() {
  list(
    flat = 9,  # Schwellenwert (°), unter dem Dach als flach angenommen wird
    steep = 70, # Schwellenwert (°), oberhalb dessen Steilflächen ausgenommen werden
    minsize = 3, # erforderliche Mindestausdehnung zusammenhängender Flächen (Pixel = m²)
    # zusammenhängender Dachfläche
    minbuildings = 3, ## Mindestanzahl an Gebäuden, ab der die Kachel berechnet wird
    a_usable = .7,  # Anteil der für PV nutzbaren Dachfläche (0-1)
    modul_m2 = 2.1,  # Fläche pro Modul [m2]
    pv_e = .18,  # PV efficiency (0-1)
    pv_e_f = \(irr_global) .1898 * irr_global - 3.9931, ## Regression statt Konstante
    st_e = .4,  # ST efficiency (0-1)
    buffer = 0,  # Puffer um Gebäudepolygone [m]; nicht puffern, die Berechnung von Neigung/Aspekt 
    ## entfernt sowieso schon den Zellsaum;
    intervals_solar = c(0, 550, 700, 850, 1000, 1150, Inf), ## Klassen solar
    labels = list(
      aspect = c('N', 'NO', 'O', 'SO', 'S', 'SW', 'W', 'NW'),
      eignung_solar = c('nicht', 'wenig_2040', 'wenig_2020', 'geeignet', 'gut', 'sehr_gut')
    )
  )
}




get_tile_codes <- \(file_paths){
  tile_codes <- list.files(file_paths$filepath_DOM,
                           pattern = '\\.tif[f]?$'
  ) |> 
    gsub(pattern = '(.*)_.*', replacement = '\\1') |> 
    sort()
  cat(sprintf("%.0f DOM tiles found", length(tile_codes)))
  tile_codes
}


## behält nur Pixelsammlungen über `minsize` Mindestausdehnung:
clean_raster <- \(r_buildings, r_slope){
  r <- r_buildings
  im_buildings <- matrix(r_buildings, dim(r_buildings)) |> as.cimg()
  im_slope <- matrix(r_slope, dim(r_slope)) |> as.cimg()
  ## Pixel auf Gebäudeflächen mit < 70 
  values(r) <- as.pixset(!is.na(im_buildings) & im_slope < constants$steep) |> 
    ## Inseln < minsize entfernen:
    clean(constants$minsize) |> as.matrix() |> t()
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


## liest Raster ein, stellt zusätzliche Raster her,
## dazu gehört auch das kachelweise Einlesen und Rastern der Gebäude-Polygone
prepare_rasters <- \(file_paths, ## Pfade zu Eingaberastern/-vektoren
                     tile_code, ## aktuell bearbeitete Kachel
                     keep = FALSE ## Raster aus Zwischenschritten behalten?
                     ){
  rasters <- list()
  
  ## DOM einlesen, vorerst nur, um den tile extent zu bestimmen
  rasters$dom_full <- rast(file.path(file_paths$filepath_DOM,
                                     sprintf('%s_DOM.tif', tile_code)
  )
  )
  
  
  v_buildings <- query(v_buildings_austria, extent = rasters$dom_full)
  ## mit NULL abbrechen, falls keine Gebäude in Kachel:
  if(all(is.na(values(v_buildings)))) return(NULL)
  
  rasters$buildings <-
    v_buildings |>
    buffer(constants$buffer) |> 
    rasterize(y = rasters$dom_full, field = 'OBJECTID', touches = FALSE)

  ## Gemeindepolygone abfragen und rastern:
  rasters$communities <-
    v_communities_austria |>
    query(ext = rasters$dom_full) |>
    rasterize(y = rasters$dom, field = 'id')
  
  ## Oberflächen-Höhenmodell:
  rasters$dom <- rasters$dom_full
  set.names(rasters$dom, 'dom')
  rasters$dom_full <- NULL
  
  ## Globalstrahlung:
  rasters$glo <- 
    rast(file.path(file_paths$filepath_GLO,
              sprintf('%s_GLO_real_Jahressumme.tif', tile_code)
              )
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
  
  general_mask <- clean_raster(rasters$buildings, rasters$slope)
  rasters <- Map(rasters, f = \(r) mask(r, general_mask))
  rasters
}


## Rasterwerte nach zwei kombinierten Zonenrastern summieren und umformen.
get_areas_wide <- \(r, zones_1, zones_2, labels_wide = NULL){
  ## kombiniert Zonen; der Multiplikator für die Rasterwerte der 2. Zone sorgt
  ## für eindeutige Werte, die später wieder durch Mod-division und Mod den
  ## jeweiligen Kategorien von Raster 1 und 2 zugeordnet werden können.
  
  ## der Modul muss höher als das Maximum eindeutiger Rasterwerte beider
  ## Gruppierungszonen sein:
  the_mod <- 1 + max(length(unique(values(zones_1))),
                     length(unique(values(zones_2)))
  ) 
  
  zones <- the_mod * zones_1 + zones_2
  tmp <- zonal(r, zones, sum, na.rm = FALSE)
  
  tmp <- cbind(tmp, tmp[1] %/% the_mod, tmp[1] %% the_mod) |> 
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
  
  ## Gemeinde-IDs der Gebäude:
  community_ids <- zonal(rasters$communities, rasters$buildings, mean) |>
    mutate(id = (cats(rasters$communities)[[1]])$id[id + 1]) |>
    rename(GEMEINDE_ID = id)
  
  
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
    rename(OBJECTID = 'inclined_OBJECTID',
           inclined = 'inclined_1',
           flat = 'inclined_0'
    )
 
  ## Eignungsklassen
  suitabilities <- get_areas_wide(rasters$a, rasters$buildings, rasters$suit)
  names(suitabilities)[1] <- 'OBJECTID'
  
  ## Jahreseinstrahlung pro Eignungsklasse:
  glo_per_suit <- get_areas_wide(rasters$a * rasters$glo, rasters$buildings, rasters$suit) |> 
    rename_with(.fn = ~ gsub('^', 'glo_', .x)) |>
    rename(OBJECTID = glo_suit_OBJECTID)

  
  ## Jahreseinstrahlung auf Flachdächer:
  glo_per_rooftype <- get_areas_wide(rasters$a * rasters$glo, rasters$buildings, rasters$rooftype) |>
    rename(glo_flat = "inclined_0", glo_inclined = "inclined_1",
           OBJECTID = inclined_OBJECTID
           )
  
  ## Ertrag PV
  harvest_pv <- zonal(rasters$harvest_pv, rasters$buildings)
  ## Ertrag Solarthermie
  harvest_st <- zonal(rasters$harvest_st, rasters$buildings)
  
  ## alle zu data.frame joinen:
  Reduce(f = \(a, b) left_join(a, b, by = 'OBJECTID'),
         list(community_ids,
              dom_stats, outliers,
              aspects, rooftypes,
              suitabilities, 
              glo_per_suit,
              glo_per_rooftype,
              harvest_pv, harvest_st
              )
  ) ##|> 
    ##relocate(glo_flat, .before = glo_inclined)
}


## ergänzt die tabellierte Rasterinformation von `extract_rasters` um (auf Gebäudeebene)
## errechnete Werte:
enrich_extract <- \(d){ # d ist ein data.table
  tmp <- as.data.table(d)
  setnames(tmp, paste0('aspect_', 0:7), paste0('aspect_', constants$labels$aspect), skip_absent=TRUE)
  setnames(tmp, paste0('suit_', 0:5), paste0('eign_', constants$labels$eignung_solar), skip_absent=TRUE)
  setnames(tmp, paste0('glo_suit_', 0:5), paste0('glo_eign_', constants$labels$eignung_solar), skip_absent=TRUE)
  setnames(tmp, names(tmp), gsub('NaN', 'unb', names(tmp)), skip_absent=TRUE)
  setnames(tmp, names(tmp), gsub('\\.', '_', names(tmp)))
  
  tmp |> as.data.frame()
}


prepare_db_output_table <- \(conn, table_name = 'raw'){
  dbExecute(conn, sprintf(
    "CREATE TABLE IF NOT EXISTS %s (
    GEMEINDE_ID text, OBJECTID text,  dom_min double, dom_mean double, 
    dom_sd double, dom_max double, n_outliers integer, aspect_N double,
    aspect_NO double, aspect_O double, aspect_SO double, aspect_S double,
    aspect_SW double, aspect_W double, aspect_NW double, flat double, 
    inclined double, eign_nicht double, eign_wenig_2040 double, 
    eign_wenig_2020 double, eign_geeignet double, eign_gut double, 
    eign_sehr_gut double, glo_eign_nicht double, glo_eign_wenig_2040 double, 
    glo_eign_wenig_2020 double, glo_eign_geeignet double, glo_eign_gut double, 
    glo_eign_sehr_gut double, glo_rooftype_0 double, glo_rooftype_1 double,
    ertrag_PV double, ertrag_ST double,
    
    PRIMARY KEY(GEMEINDE_ID, OBJECTID)
          )",
    table_name
  )
  )
  

}



## schreibt data.frame in SQLite-DB
write_to_db <- \(d, table_name = 'raw', conn){ ## data.frame
  dbWriteTable(conn = conn,
               name = table_name,
               value = d,
               append = TRUE
               )
}

### Berechnung und Speicherung pro Kachel:
calc_and_save <- \(dir_root = '.', tile_code, export_images = FALSE, 
                   save_excels = FALSE, conn, i
                   ){
  cat(paste('\n', i, Sys.time(), ': '))
  cat(sprintf('working on tile %s ...', tile_code))
  cat('preparing rasters...')
  rasters <- prepare_rasters(file_paths, tile_code) ## Arbeitsraster anlegen
  
  tryCatch(
    d <- rasters |> 
      extract_rasters() |> ## Rasterwerte als data.frame extrahieren
      enrich_extract() ## zusätzliche Tabellenkalkulationen
    , error = \(e) cat(paste('...can\'t extract data: ', e))
  )
  
  
  cat("...trying to write to database ...")
  tryCatch({
    prepare_db_output_table(conn)
    write_to_db(d, table_name = 'raw', conn)},
           error = \(e) cat(paste('can\'t write to database:', e))
  )
  
  if(save_excels){
    cat("...trying to write CSV...")
    ## Ergebnistabelle als CSV speichern:
    tryCatch( d |> write.csv2(file.path(dir_root, sprintf('output/data_%s.csv', tile_code))),
              error = \(e) cat(paste('...writing failed: ', e))
    )
  }
  
  if(export_images){
    ## falls export_images == TRUE, Raster als GeoTIFFs speichern:
    cat("...trying to save tiffs...")
    tryCatch({
      Map(names(rasters),
          f = \(n) writeRaster(rasters[[n]], 
                               sprintf('./output/%s_%s_%spx.tiff', tile_code, n,
                                       as.character(constants$minsize)
                               ),
                               overwrite = TRUE))
    },
    error = \(e) cat('...saving tiff failed')
    )
  }
}




count_dom_outliers <- \(tile_code, buffer_size = -1, iqr_mult = 2){
  
  r_in <- rast(sprintf(file_paths$filepath_DOM, sprintf('%s_DOM.tif', tile_code)))
  
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


## führt Plausibilitätstests durch:
validate <- \(d){
  ## Gleichheit Dachflächen nach Neigung bzw. Aspekt:
  expect_equal(
    sum(d$flat, d$inclined, na.rm = TRUE),
    d |>
      summarise(across(starts_with("aspect"), ~ sum(.x, na.rm = TRUE))) |>
      rowSums()
    )
  ## Gleichheit Dachflächen nach Neigung bzw. Eignung:
  expect_equal(
    sum(d$flat, d$inclined, na.rm = TRUE),
    d |>
      summarise(across(starts_with("eign_"), ~ sum(.x, na.rm = TRUE))) |>
      rowSums()
  )
  ## Gleichheit Solarstrahlung nach Neigung bzw. Eignung:
  expect_equal(
    d |> summarise(across(c(glo_flat, glo_inclined), ~ sum(.x, na.rm = TRUE))) |>
      rowSums(),
    d |> summarise(across(starts_with("glo_eign_"), ~ sum(.x, na.rm = TRUE))) |>
      rowSums()
  )
  ## die Summe der geneigten Dachfläche sollte die der Flachdächer übersteigen: 
  expect_gt(
    sum(d$inclined, na.rm = TRUE),
    sum(d$flat, na.rm = TRUE)
  )
  
  
}




