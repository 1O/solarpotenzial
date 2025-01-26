## Vorbereitung ----------------------------------------------------------------
### Bibliotheken laden:
library(terra)
library(sf)
library(dplyr)
library(data.table)

### Arbeitsumgebung setzen
## Stammverzeichnis (mit `main.R` und `helpers.R`):
dir_root <- 'Pfad/zum/Stammverzeichnis'
setwd(dir_root) ## Stammverzeichnis als Arbeitsverzeichnis
source('./helpers.R') ## Hilfsfunktionen laden

#### Pfade zu Gebäude-Geopackage (relativ zu Arbeits = Stammverzeichnis)
file_buildings <- file.path(dir_root, 'input/GEB/DLM_8000_Bauwerk_20241118.gpkg')

### Konstanten festlegen:
constants <- list(
  flat = 10,  # Schwellenwert (°), unter dem Dach als flach angenommen wird
  a_usable = .7,  # Anteil der für PV nutzbaren Dachfläche (0-1)
  modul_m2 = 2.1,  # Fläche pro Modul [m2]
  pv_e = .18,  # PV efficiency (0-1)
  st_e = .4,  # ST efficiency (0-1)
  buffer = -1,  # Puffer um Gebäudepolygone [m]
  layername_buildings = 'BWK_8100_BAUWERK_F_20241118',
  intervals_solar = c(0, 550, 700, 850, 1000, 1150, Inf), ## Klassen solar
  labels = list(
    aspect = c('N', 'NO', 'O', 'SO', 'S', 'SW', 'W', 'NW'),
    eignung_solar = c('nicht', 'wenig_2040', 'wenig_2020', 'geeignet', 'gut', 'sehr_gut')
    )
)

#### DB-Verbindung zu Geopackage
#### mit Gesamtdatensatz der Gebäudepolygone herstellen:
vectors <- list(
  buildings_austria = vect(file_buildings,
                           layer = constants$layername_buildings,
                           proxy = TRUE ## nur Verbindung, nicht einlesen
  )
)


## Berechnungen ----------------------------------------------------------------
### Anwendungsbsp:

#### tile codes :
tile_code <- '26850-47525'

#### Raster aus GeoTIFFs einlesen und abgeleitete Raster berechnen:
## Dauer: 1.656 s pro Kachel
## microbenchmark::microbenchmark(prepare_rasters(dir_root, tile_code), times = 10)
rasters <- prepare_rasters(dir_root, tile_code)


#### Rasterinformationen in Tabelle (data.table) zusammenführen:
## Dauer: 2.285 s pro Kachel
summary_table <- extract_rasters(rasters)


#### summary_table ergänzen (wird by reference direkt in summary_table geändert)
enrich_extract(summary_table)


#### gesamter Prozess pro Kachel:
calc_and_save(dir_root, tile_code)

#### Durchschleifen mehrerer Kacheln:


tile_codes |> 
  Map(f = \(tile_code){
    cat(sprintf('working on tile %s ...', tile_code))
    tryCatch(calc_and_save(dir_root, tile_code),
             error = \(e) cat(sprintf('processing for tile %s failed', tile_code))
    )
  })



