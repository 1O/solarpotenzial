## Vorbereitung ----------------------------------------------------------------
### Bibliotheken laden:
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(RSQLite)
library(imager)


### Arbeitsumgebung setzen
## Stammverzeichnis (mit `main.R` und `helpers.R`):
dir_root <- '~/Dokumente/fremd/Christine Brendle/Solarpotenzial/R/'

setwd(dir_root) ## Stammverzeichnis als Arbeitsverzeichnis
source('./helpers.R') ## Hilfsfunktionen laden



### Konstanten festlegen:
constants <- list(
  flat = 10,  # Schwellenwert (°), unter dem Dach als flach angenommen wird
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

#### DB-Verbindung zu Geopackage
#### mit Gesamtdatensatz der Gebäudepolygone herstellen:
#### die Gebäudedaten sind bereits auf EPSG3035 (LAEA) wie die Rasterdaten
## Gebäudelayer muss in EPSG 3035 (LAEA) sein:
filepath_buildings <- file.path(dir_root, 'input/DEM/DLM_EPSG3035.gpkg')
v_buildings_austria <-  vect(filepath_buildings, proxy = TRUE) ## nur Verbindung, nicht einlesen
## dasselbe für die Gemeindepolygone:
filepath_communities <- file.path(dir_root, './input/GEM_W23_3035.gpkg')
v_communities_austria <- vect(filepath_communities, proxy = TRUE)


## Berechnungen ----------------------------------------------------------------
### Anwendungsbsp:
tile_codes <- c("26850-47525", "99999-99999", "27475-45475")


#### Raster aus GeoTIFFs einlesen und abgeleitete Raster berechnen:
## Dauer: 1.8 s / Kachel
## Beschleunigung durch Maskierung von Beginn weg und wschl. Umprojektion des
## Gebäudevektors außerhalb von R

source('./helpers.R')
rasters <- prepare_rasters(dir_root, tile_codes[1])

rasters$slope


prepare_rasters(dir_root, tile_codes[1]) |> microbenchmark::microbenchmark(times = 5)


#### Rasterinformationen in Tabelle (data.table) zusammenführen:
## Dauer: 1.69 s pro Kachel
summary_table <- extract_rasters(rasters) |> enrich_extract()



#### Durchschleifen mehrerer Kacheln:

### Datenbankverbindung öffnen; falls nicht vorhanden, wird Datenbank
### dieses Namens angelegt:
conn <- dbConnect(
  drv = SQLite(),
  dbname = './output/solarpotenzial.db'
)

#### tile codes durchschleifen:
seq_along(tile_codes) |> 
  Map(f = \(i){
    calc_and_save(dir_root, tile_code = tile_codes[i],
                  conn = conn, i = i, export_images = TRUE)
  }) ##|> microbenchmark::microbenchmark(times = 1)


### Datenbankverbindung schließen:
dbDisconnect(sqlite_conn)



## DB plausen

d <- dbReadTable(conn, "raw")

summary(d)



## Sandbox ---------------------------------------------------------------------

source('./helpers.R')




## Jahreseinstrahlung auf Flachdächer:
get_areas_wide(rasters$a * rasters$glo, rasters$buildings, rasters$rooftype) |> 
  rename_with(.fn = ~ gsub('inclined', 'glo_rooftype', .x)) |> 
  rename(OBJECTID = glo_rooftype_OBJECTID)


#   ## Ausreißerzahl vs. verschiedene Puffergrößen als dataframe:
# tile_code <- tile_codes[1]
# res2 <- data.frame(buffer_size = 3 - .2 * 0:30) |>
#   rowwise() |>
#   mutate(outlier_count = count_dom_outliers(tile_code, buffer_size = buffer_size))
# 
# write.csv(res, file.path("output", sprintf("%s_outliers.csv", tile_code)))
# 
# library(ggplot2)
# left_join(res, res2, by = 'buffer_size') |>
#   as.data.frame() |> 
#   tidyr::pivot_longer(2:3) |> 
#   arrange(name) |>
#   mutate(value = (value-min(value))/(max(value)-min(value)), .by = name) |> 
#   ggplot(aes(buffer_size, value, col = name)) + geom_point() + geom_line() +
#   geom_vline(aes(xintercept = constants$buffer), lty='dashed') +
#   labs(title = 'Ausreißer DOM vs. Pufferdistanz', x = 'Pufferdistanz [m]', y = 'Anzahl Ausreißerpixel (=Fläche in m2)',
#                    caption = sprintf(
#                      'Beispielkacheln %s; \npositive / negative Pufferdistanz: Gebäudeumriss wird erweitert / beschnitten',
#                      paste(tile_codes, collapse = ', ')
#                      )
#   )
# 
# 
# ggsave('./output/outliers_vs_buffer.png')
# 
# 
# hist_slope <- rasters$slope |> values() |> hist()
# 
# hist_slope[c('mids', 'counts')] |> as.data.frame() |> 
#   mutate(coverage = cumsum(counts),
#          rel_coverage = 100 * coverage / max(coverage)
#          ) |> 
#   ggplot(aes(mids, rel_coverage)) + geom_line() + geom_point() +
#   labs(x = 'Dachneigung', y = 'erfasste Dachfläche [%]',
#        title = '27475-45475 (Salzburg)')
# 
# 
# 
# ggsave('./output/dom_coverage_vs_slope_salzburg.png')
#