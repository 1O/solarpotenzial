
## liest Raster ein, stellt zusätzliche Raster her,
## dazu gehört auch das kachelweise Einlesen und Rastern der Gebäude-Polygone
prepare_rasters <- \(dir_root = '.', tile_code, keep = FALSE){
  path_dom_file <- file.path(dir_root, sprintf('input/DOM/%s_DOM.tif', tile_code))  
  path_glo_file <- file.path(dir_root,
                             sprintf('input/GLO/%s_GLO_real_Jahressumme.tif', tile_code)
  )
  
  ## Raster für DOM und Globalstrahlung:
  rasters <- list(dom = rast(path_dom_file) |> setNames(nm = 'elev'),
                  glo = rast(path_glo_file)
  )
  
  
  vectors$buildings <- with(c(rasters, vectors), {
    ## Gebäudepolygone innerhalb des DOM-Rasters:
    query(buildings_austria,
          ## extent des DOM rasters auf CRS der Gebäude-DB umprojizieren:
          extent = project(ext(dom), crs(dom), crs(buildings_austria))
    ) |> 
      ## umprojizieren auf CRS des DOM-Rasters, Puffern:
      project(crs(rasters$dom)) |> buffer(constants$buffer)
  })
  
  
  ## Gebäudepolygone als Raster "buildings", Auflösung und Extent vom DOM:
  rasters$buildings <- vectors$buildings |> rasterize(y = rasters$dom, field = 'OBJECTID')
  
  
  ## Raster-Liste um Raster mit slope und aspect ergänzen,
  ## zur Beschleunigung werden nur die Zellenwerte innerhalb eines 20m-Puffers
  ## um die Gebäude berücksichtigt:
  
  rasters <- append(rasters,
                    local({
                      tmp <- rasters$dom |> mask(buffer(vectors$buildings, 20))
                      rasters_new <- Map(c('slope', 'aspect'), f = \(param) terrain(tmp, param))
                      rm(tmp)
                      rasters_new
                    })
  )
  
  
  ## alle Rasterkacheln mit gepufferten Gebäudepolygonen maskieren
  ## (senkt Rechenaufwand)
  rasters <- Map(rasters, f = \(r) mask(r, vectors$buildings))
  
  
  #### die folgenden abgeleiteten Raster werden erst hier,
  #### aus den maskierten Eingangsrastern erzeugt (Rechenaufwand):
  
  ## Raster mit kategorisierten Himmelsrichtungen:
  rasters$aspect_classed <- rasters$aspect |> classify(-45/2 + 0:6 * 45)
  
  ## Raster mit rooftype (0 = flat, 1 = inclined):
  rasters$rooftype <- rasters$slope |>
    classify(rbind(c(-Inf, constants$flat, 0), c(constants$flat, Inf, 1))) |> 
    setNames(nm = 'inclined')
  
  ## Raster mit eignung_solar hinzufügen und Kategorielabels setzen:
  rasters$eignung_solar <- rasters$glo |> classify(cbind(
    head(constants$intervals_solar, -1), ## von
    tail(constants$intervals_solar, -1), ## bis
    0:5 ## Klassenindex (0 = ungeeignet, 5 = sehr gut geeignet)
  )
  )
  set.cats(rasters$eignung_solar, ## Klassenindex (0-5) labeln:
           value = data.frame(a = 0:5, eignung = constants$labels$eignung_solar)
  )
  
  
  ## nicht mehr benötigte Raster löschen, falls keep = FALSE:
  if(!keep) rasters[c(
    # 'glo',
    # 'slope',
    # 'aspect'
  )] <- NULL
  
  rasters
  
}


## ergänzt die tabellierte Rasterinformation von `extract_rasters` um (auf Gebäudeebene)
## errechnete Werte:
enrich_extract <- \(d){ # d ist ein data.frame
  d |> 
    mutate(
      suit_total = sum(c_across(starts_with('solareignung_'))) - solareignung_nicht,
      suit_low = sum(c_across(paste0('solareignung_', c('sehr gut', 'gut')))),
      suit_high_20 = suit_low + sum(c_across(paste0('solareignung_', c('geeignet', 'wenig (2020)')))),
      suit_high_40 = suit_low + sum(c_across(paste0('solareignung_', c('geeignet', 'wenig (2040)')))),
      n_possible_modules = suit_total / constants$a_usable,
      mutate(across(starts_with('suit_'),
                    .fns = list(
                      e_pv = ~ prod(.x, constants$pv_e, constants$a_usable),
                      e_st = ~ prod(.x, constants$pv_e, constants$a_usable)
                    )
      )
      ),
      .by = OBJECTID
    )
}



extract_rasters <- \(the_rasters){
  ## extrahiert Rasterwerte und aggregiert sie pro OBJECTID zu data.frames
  ## konvertiert data.frames zu data.tables mit OBJECTID als Index
  ## joined data.tables
  tables <- with(the_rasters, {
    list(
      ids = data.table(unique(values(buildings))),
      dom = zonal(dom, buildings,
                  fun = \(xs) c(count = length(xs),
                                min = min(xs, na.rm = TRUE),
                                mean = mean(xs, na.rm = TRUE),
                                sd = sd(xs, na.rm = TRUE),
                                max = max(xs, na.rm = TRUE),
                                outlier_count = boxplot(xs,
                                                        range = 2, ## doppelter IQR von unterer/oberer Boxkante
                                                        plot = FALSE
                                )$out |>
                                  length() |> as.integer()
                  )
      ),
      aspect = zonal(aspect_classed, buildings,
                     fun = \(x) factor(x, levels = 0:7, labels = constants$labels$aspect) |> table()
      ),
      ## die Prozentberechnung über Summe und Länge funktioniert, weil rooftype 'flat' numerisch Null ist
      rooftype = zonal(rooftype, buildings, \(x) 100 * sum(x) / length(x)) |> rename(perc_inclined = 'inclined'),
      eignung_solar = zonal(eignung_solar, buildings,
                            fun = \(x) factor(x, levels = 1:6, labels = constants$labels$eignung_solar) |> table()
      ),
      ## geneigte Dachfläche pro Himmelsrichtung:
      inclined_per_aspect = zonal(rasters$rooftype * rasters$aspect_classed, rasters$buildings,
                                  fun = \(x) factor(x, levels = 0:7, labels = constants$labels$aspect) |> table()
      ),
      ## geneigte Dachfläche pro Einstrahlungs-Eignung:
      inclined_per_eignung <-  zonal(rasters$rooftype * rasters$eignung_solar, rasters$buildings,
                                     fun = \(x) factor(x, levels = 0:5, labels = constants$labels$eignung_solar) |> table()
      )
    ) |> 
      ## in data.table umwandeln und indizieren (Performance)
      Map(f = \(table) as.data.table(table) |> setkeyv('OBJECTID'))
  }
  )
  
  ## data.tables aus `tables` über OBJECTID joinen:
  tables |> Reduce(f = \(a, b) a[b]) |> head()
}


## ergänzt die tabellierte Rasterinformation von `extract_rasters` um (auf Gebäudeebene)
## errechnete Werte:
enrich_extract <- \(d){ # d ist ein data.table
  tmp <- copy(d)
  ## SOLLEN NUR GENEIGTE DACHFLÄCHEN INKLUDIERT WERDEN???
  
  ## possible module count
  tmp[, `:=` (n_poss_modules = rowSums(.SD) * constants$a_usable / constants$modul_m2),
    #### diese Spalten für Berechnung:
    .SDcols = paste('inclined', c('wenig_2020', 'geeignet', 'gut', 'sehr_gut'), sep = '.')
  ]
  
  ## high scenario 2020
  tmp[, `:=` (e_pv_2020_high = rowSums(.SD) * constants$a_usable * constants$pv_e,
            e_st_2020_high = rowSums(.SD) * constants$a_usable * constants$st_e
  ),
  .SDcols = paste('inclined', c('wenig_2020', 'geeignet', 'gut', 'sehr_gut'), sep = '.')
  ]
  
  ## high scenario 2040
  tmp[, `:=` (e_pv_2040_high = rowSums(.SD) * constants$a_usable * constants$pv_e,
            e_st_2040_high = rowSums(.SD) * constants$a_usable * constants$st_e
  ),
  .SDcols = paste('inclined', c('wenig_2020', 'wenig_2040', 'geeignet', 'gut', 'sehr_gut'), sep = '.')
  ]
  
  ## low scenarios 2020 & 2040 (identisch)
  tmp[, `:=` (e_pv_2020_low = rowSums(.SD) * constants$a_usable * constants$pv_e,
            e_st_2020_low = rowSums(.SD) * constants$a_usable * constants$st_e,
            e_pv_2040_low = rowSums(.SD) * constants$a_usable * constants$pv_e,
            e_st_2040_low = rowSums(.SD) * constants$a_usable * constants$st_e
  ),
  .SDcols = paste('inclined', c('gut', 'sehr_gut'), sep = '.')
  ]  
  
  tmp
  
}



### Berechnung und Speicherung pro Kachel:
calc_and_save <- \(dir_root = '.', tile_code){
  
  rasters <- prepare_rasters(dir_root, tile_code) ## Arbeitsraster anlegen
  d <- rasters |> 
    extract_rasters() |> ## Rasterwerte als data.frame extrahieren
    enrich_extract() ## zusätzliche Tabellenkalkulationen
  
  d |> write.csv2(file.path(dir_root, sprintf('output/data_%s.csv', tile_code)))
  rasters$eignung_solar |> writeRaster(file.path(dir_root, sprintf('output/%s.tiff', tile_code)),
                                       overwrite = TRUE
  )
  
}



  
