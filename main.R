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
dir_root <- '.'

setwd(dir_root) ## Stammverzeichnis als Arbeitsverzeichnis
source('./helpers.R') ## Hilfsfunktionen laden

data_root <- "input"
## DOM in Unterverzeichnis "DOM"
## DOM in Unterverzeichnis "GLO_real"

### Konstanten festlegen:
constants <- list(
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
file.path(data_root, 'DOM/') |> list.files()

tile_codes <- list.files(file.path(data_root, 'DOM'),
                         pattern = '\\.tif[f]?$'
) |> 
  gsub(pattern = '(.*)_.*', replacement = '\\1') |> 
  sort()

length(tile_codes) ## 14008

#### Raster aus GeoTIFFs einlesen und abgeleitete Raster berechnen:
## Dauer: 1.8 s / Kachel
## Beschleunigung durch Maskierung von Beginn weg und wschl. Umprojektion des
## Gebäudevektors außerhalb von R

source('./helpers.R')
rasters <- prepare_rasters(data_root, tile_codes[1])

extract_rasters(rasters) |> enrich_extract() |> rio::export("hugo.xlsx")




#### Rasterinformationen in Tabelle (data.table) zusammenführen:
## Dauer: 1.69 s pro Kachel
summary_table <- extract_rasters(rasters) |> enrich_extract()



#### Durchschleifen mehrerer Kacheln, Ausgabe in MySQL-Datenbank:
### Datenbankverbindung öffnen; falls nicht vorhanden, wird Datenbank
### dieses Namens angelegt:


dbDisconnect(conn) ## ggf. bestehende Verbindung schließen
## Verbindung öffnen
conn <- dbConnect( drv = SQLite(), dbname = './output/solarpotenzial.sqlite')

## Ausgabe-table "raw" anlegen:
prepare_db_output_table(conn, 'raw')

## Log-Datei anlegen (laufende Kachelnummer, Fehlermeldungen etc. in Datei)
##
sink() ## ggf. bestehenden Ausgabekanal schließen
sink(file = "./output/process.log", append = TRUE,
     type = c("output", "message"),
     split = FALSE
     )


#### tile codes durchschleifen:
  1 : length(tile_codes) |> ## ggf. ab schon prozedierter Kachel fortsetzen:
  Map(f = \(i){
    if(!tile_codes[i] %in% c('26425-46800')){
    calc_and_save(dir_root, tile_code = tile_codes[i],
                  conn = conn,
                  i = i,
                  save_excels = FALSE,
                  export_images = FALSE
                  )
    }
  }) ##|> microbenchmark::microbenchmark()


dbDisconnect(conn)
## Ausgabe in Logdatei schließen:
sink()



## ---------------------------------------------------------------------------
## DB plausen
## Ergebnisse nicht mehr aktuell
### direkte DB-Abfrage
conn <- dbConnect( drv = SQLite(), dbname = './output/solarpotenzial.sqlite')

## auf eindeutige Werte filtern:

dbGetQuery(conn, "
           SELECT GEMEINDE_ID, OBJECTID, tile,
           MAX(dom_min) AS dom_min 
           FROM raw
           GROUP BY GEMEINDE_ID, OBJECTID, tile;
           "
           ) |> nrow()

## eindeutige Datensätze: 4 308 682
## Gesamtdatensätze: 4 308 682

# res <- dbGetQuery(conn, "
#            SELECT count(GEMEINDE_ID) as N,
#            max(GEMEINDE_ID) as GEMEINDE_ID,
#            max(OBJECTID) as OBJECTID,
#            max(tile) as tile
#            FROM raw
#            GROUP BY GEMEINDE_ID, OBJECTID, tile;
#            "d
# )

## Dachflächen desselben Gebäudes (OBJECTID) kann auf bis zu vier Kacheln 
## lt. Blattschnitt aufgeteilt sein, das tritt in der DB auch tatsächlich auf
## Bsp. OBJECT 1169260 am Westrand von Siegendorf.
## Die Flächen sind deswegen auf OBJECTID und/oder GEMEINDE_ID zu aggregieren,
## nicht aber zusätzlich auf Kachel-ID ('tile')

res |> arrange(desc(N)) |> head()




## fehlen Gemeinden?
dbGetQuery(conn, "SELECT DISTINCT GEMEINDE_ID FROM raw") |> nrow()
## 2047 Gemeinden

## welche Gebäude fehlen? -------------------------------------------------------
objectids_is <- (dbGetQuery(conn, "SELECT DISTINCT OBJECTID FROM raw"))$OBJECTID |> 
  as.integer() |> sort() ## mehrere Sek.
## ist: 4 265 981 Gebäude
objectids_must <- (read_sf(dsn = './input/DEM/DLM_Bauwerk_nur_OBJECTID.gpkg',
        query = 'SELECT DISTINCT OBJECTID FROM dlm_8000_bauwerk_20241118__bwk_8100_bauwerk_f_20241118'
        ))$OBJECTID
## soll: 4 487 063 Gebäude (lt. DLM-GPKG)
objectids_missing <- setdiff(objectids_must, objectids_is)
length(objectids_missing) ## 221 082 Gebäude fehlen


## Darstellung der fehlenden Gebäude:
v_objects <- read_sf(dsn = './input/DEM/DLM_Bauwerk_nur_OBJECTID.gpkg', 
                        layer = 'dlm_8000_bauwerk_20241118__bwk_8100_bauwerk_f_20241118'
)

v_objects_missing <- v_objects |>
  filter(OBJECTID %in% objectids_missing) |> 
  mutate(area = st_area(geom) |> as.double())
## v_objects_missing |> write_sf('./output/objects_missing.shp') ## als SHP speichern

## Größe der fehlenden Gebäude
areas <- v_objects_missing |> mutate(area = st_area(geom)) |> pull(area)
                               
v_objects_missing |> filter(area > 500) |> write_sf('large_missing_objects.shp')




tile_codes_is <- (dbGetQuery(conn, "SELECT DISTINCT tile FROM raw"))$tile
length(tile_codes_is)
## 12183 Kacheln
tile_codes_must <- list.files(file.path(data_root, 'DSM'),
                                            pattern = '\\.tif[f]?$'
) |> 
  gsub(pattern = '(.*)_.*', replacement = '\\1') |> 
  sort()

tile_codes_missing <- setdiff(tile_codes_must, tile_codes_is)
length(tile_codes_missing) ## 825 Kacheln übergangen

## Welche Kacheln fehlen?
tile_shapes <- read_sf("./blattschnitt.shp")
tile_shapes_missing <- tile_shapes |> filter(name %in% tile_codes_missing)
tile_shapes_missing |> plot()
## tile_shapes_missing |> write_sf('./blattschnitt_missing.shp') ## ggf. als Shape speichern




## Aggregieren der Rohdaten aus SQLite-DB:
conn <- dbConnect( drv = SQLite(), dbname = './output/solarpotenzial.sqlite')



d <- dbGetQuery(conn, "SELECT * FROM raw LIMIT 1000")

head(d)
d |> rio::export("hugo.xlsx")
  
  
getwd()

?c_across

d <- dbGetQuery(conn, "SELECT * FROM raw")

d[,
  lapply(.SD, \(xs) sum(xs, na.rm = TRUE)),
  .SDcols = names(d)[lapply(d, is.numeric) |> unlist()],
  keyby = .(GEMEINDE_ID)
] |> rio::export("roh.xlsx")


## Tests:


set.seed(123)
rasters <- list()
## 16 Pixel, mit Globalstrahlung um 1200 kW
rasters$glo <- rast(matrix(1200 + 50 * rnorm(16), 4, 4))
names(rasters$glo) <- "glo"


## Raster mit zwei Hälften (links, rechts)
rasters$buildings <- rast(matrix(gl(2, 8), 4, 4))
names(rasters$buildings) <- "buildings"

## Raster mit zwei Klassen (buildings um 90° rotiert)
rasters$suit <- t(rasters$buildings)
names(rasters$suit) <- "suitability"

## A und B müssen äquivalent sein
## A:
get_areas_wide(rasters$glo, rasters$buildings, rasters$suit)
## B:
data.frame(glo = values(rasters$glo),
      buildings = values(rasters$buildings),
      suit = values(rasters$suit)
      ) |> 
  summarise(glo = sum(glo), .by = c(buildings, suitability)) |> 
  pivot_wider(names_from = suitability, values_from = glo)
## OK


