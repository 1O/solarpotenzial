## Vorbereitung ----------------------------------------------------------------
### Bibliotheken laden:
library(terra)
library(sf)
library(dplyr)
library(data.table)
library(RSQLite)
library(imager)




### Arbeitsumgebung setzen
## Stammverzeichnis (mit `main.R` und `helpers.R`):
dir_root <- '~/Dokumente/fremd/Christine Brendle/Solarpotenzial/R/'

setwd(dir_root) ## Stammverzeichnis als Arbeitsverzeichnis
source('./helpers.R') ## Hilfsfunktionen laden


# S4 method for class 'SQLiteDriver' (für späteres Auslesen direkt in DB)
# sqlite_conn <- dbConnect(
#   drv = SQLite(),
#   dbname = './output/solarpotenzial.db'
# )

### Konstanten festlegen:
constants <- list(
  # crs = 31287, ## Lambert Austria
  flat = 10,  # Schwellenwert (°), unter dem Dach als flach angenommen wird
  min_cluster_size = 9, # erforderliche Mindestgröße (Pixel = m²)
  # zusammenhängender Dachfläche
  a_usable = .7,  # Anteil der für PV nutzbaren Dachfläche (0-1)
  modul_m2 = 2.1,  # Fläche pro Modul [m2]
  pv_e = .18,  # PV efficiency (0-1)
  st_e = .4,  # ST efficiency (0-1)
  buffer = -1,  # Puffer um Gebäudepolygone [m]
  intervals_solar = c(0, 550, 700, 850, 1000, 1150, Inf), ## Klassen solar
  labels = list(
    aspect = c('N', 'NO', 'O', 'SO', 'S', 'SW', 'W', 'NW'),
    eignung_solar = c('nicht', 'wenig_2040', 'wenig_2020', 'geeignet', 'gut', 'sehr_gut')
    )
)

#### DB-Verbindung zu Geopackage
#### mit Gesamtdatensatz der Gebäudepolygone herstellen:
#### die Gebäudedaten sind bereits auf EPSG3035 (LAEA) wie die Rasterdaten
filepath_buildings <- file.path(dir_root, 'input/DEM/DLM_EPSG3035.gpkg')
v_buildings_austria = vect(filepath_buildings, proxy = TRUE) ## nur Verbindung, nicht einlesen


## Berechnungen ----------------------------------------------------------------
### Anwendungsbsp:

#### tile codes :
tile_code <- '26850-47525'

#### Raster aus GeoTIFFs einlesen und abgeleitete Raster berechnen:
## Dauer: 0.54 s/Kachel (war: 1.656)
## Beschleunigung durch Maskierung von Beginn weg und wschl. Umprojektion des
## Gebäudevektors außerhalb von R
## microbenchmark::microbenchmark(prepare_rasters(dir_root, tile_code), times = 10)


source("helpers.R")

#### Rasterinformationen in Tabelle (data.table) zusammenführen:
## Dauer: 2.285 s pro Kachel



summary_table <- extract_rasters(rasters)
#### summary_table ergänzen (wird by reference direkt in summary_table geändert)
enrich_extract(summary_table)


#### gesamter Prozess pro Kachel:
calc_and_save(dir_root, tile_code)

#### Durchschleifen mehrerer Kacheln:

tile_codes <- c("26850-47525", "27475-45475")

tile_codes |> 
  Map(f = \(tile_code){
    cat(sprintf('\nworking on tile %s ...', tile_code))
    tryCatch(calc_and_save(dir_root, tile_code),
             error = \(e) cat(sprintf('processing for tile %s failed', tile_code))
    )
  })



## Ausreißerzahl vs. verschiedene Puffergrößen als dataframe:
tile_code <- tile_codes[2]
d_outliers <- data.frame(buffer_size = 3 - .2 * 0:30) |> 
  rowwise() |>
  mutate(outlier_count = count_dom_outliers(tile_code, buffer_size = buffer_size))

write.csv(d_outliers, file.path("output", sprintf("%s_outliers.csv", tile_code)))


## Ausreißer als Raster speichern (dauert mehrere Sekunden):
tile_codes <- c("26850-47525", "27475-45475")
tile_code <- tile_codes[2]

rasters <- prepare_rasters(dir_root, tile_code)

show_dom_outliers(rasters$dom) |> 
  writeRaster(sprintf('output/%s_outliers.tiff', tile_code), overwrite = TRUE)
                                              

