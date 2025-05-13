## Vorbereitung ----------------------------------------------------------------
### Bibliotheken laden:
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(RSQLite)
library(imager)
library(rio)
library(testthat)


### Arbeitsumgebung setzenDOM
## Stammverzeichnis (mit `main.R` und `helpers.R`):
dir_root <- '.'



setwd(dir_root) ## Stammverzeichnis als Arbeitsverzeichnis
source('./helpers.R') ## Hilfsfunktionen laden
source('./table_definition.R')

## Pfade zu Datenquellen:
file_paths <- list(
  ## Pfad zur Gebäude-Geopackage:
  buildings = file.path(dir_root, 'input/DOM/DLM_EPSG3035.gpkg'),
  ## Pfad zu Gemeinde-Geopackage (für gde.weise Aggregation):
  communities = file.path(dir_root, './input/DOM/GEM_W23_3035.gpkg'),
  ## Pfad zu Oberflächenmodell:
  # DOM = file.path('/oldhome/ivo/Dokumente/fremd/','Christine Brendle/Solarpotenzial/R/input/DOM/'),
  DOM = file.path('./input/Testkacheln/26850-47525/DOM/'), ## Gleisdorf
  ## Pfad zu Globalstrahlung
  GLO = file.path('./input/Testkacheln/26850-47525/GLO_real/')
  ##GLO = file.path('/media/io/LaCie/GLO_real/')
)

### Konstanten festlegen:
constants <- get_constants()

#### DB-Verbindung zu Geopackage
#### mit Gesamtdatensatz der Gebäudepolygone herstellen:
#### die Gebäudedaten sind bereits auf EPSG3035 (LAEA) wie die Rasterdaten
## Gebäudelayer muss in EPSG 3035 (LAEA) sein:
v_buildings_austria <- vect(file_paths$buildings,
       layer = 'DLM_EPSG3035_A',
       proxy = TRUE ## nur Verbindung, nicht einlesen
  )

## Gemeindegrenzen:
v_communities_austria <- vect(file_paths$communities, proxy = TRUE)

plot(v_communities_austria)

get_tile_codes <- \(file_paths){
  tile_codes <- list.files(file_paths$DOM,
                           pattern = '\\.tif[f]?$'
  ) |> 
    gsub(pattern = '(.*)_.*', replacement = '\\1') |> 
    sort()
  cat(sprintf("%.0f DOM tiles found", length(tile_codes)))
  tile_codes
}


tile_code <- "26850-47525"

source('helpers.R')
prepare_rasters(file_paths = file_paths, tile_code = tile_code) |> 
 extract_rasters(multizonal = FALSE) |> 
  #prettify_dataframe() |>
  names()

### Berechnung und Speicherung pro Kachel:
calc_and_save <- \(file_paths, tile_code, export_images = FALSE,
                   multizonal = FALSE,
                   save_excels = FALSE, conn, i
){
  
  cat(paste('\n', i, Sys.time(), ': '))
  cat(sprintf('working on tile %s ...', tile_code))
  cat('preparing rasters...')
  rasters <- prepare_rasters(file_paths, tile_code) ## Arbeitsraster anlegen
  
  tryCatch(
    d <- rasters |> 
      extract_rasters() |> ## Rasterwerte als data.frame extrahieren
      prettify_dataframe() ## zusätzliche Tabellenkalkulationen
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
  
  r_in <- rast(sprintf(file_paths$DOM, sprintf('%s_DOM.tif', tile_code)))
  
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
  ## Gleichheit Gesamtfläche (a_total) und Summe Flächen pro Eignung und Dachtyp:
  expect_equal(
    sum(d$a_total, na.rm = TRUE),
    d |>
      summarise(across((starts_with("a_") & !ends_with("total")), ~ sum(.x, na.rm = TRUE))) |>
      rowSums()
  )
  
  ## Gleichheit Gesamtfläche und Summe Dachflächen nach Ausrichtung:
  expect_equal(
    sum(d$a_total, na.rm = TRUE),
    d |>
      summarise(across(starts_with("aspect_"), ~ sum(.x, na.rm = TRUE))) |>
      rowSums()
  )
  
  ## die Summe der geneigten Dachfläche sollte die der Flachdächer übersteigen: 
  expect_gt(
    summarise(d, across(matches('a_.*_inclined'), ~ sum(.x, na.rm = TRUE))) |> rowSums(),
    summarise(d, across(matches('a_.*_flat'), ~ sum(.x, na.rm = TRUE))) |> rowSums()
  )
  
  
  
  ## alle berechneten Strahlungssummen (kWh/ m²) für geneigte Dachflächen
  ## zwischen 0 und 2000?
  # expect_true(
  #   all(d$glo_inclined / d$inclined <= 2000) &
  #   all(d$glo_inclined / d$inclined >= 0)
  # )
  
  
}



## dasselbe für die Gemeindepolygone:
v_communities_austria <- vect(file_paths$communities, proxy = TRUE)

## Berechnungen ----------------------------------------------------------------
### Anwendungsbsp:
tile_codes <- get_tile_codes(file_paths)

#### Raster aus GeoTIFFs einlesen und abgeleitete Raster berechnen:
## Dauer: 1.8 s / Kachel
## Beschleunigung durch Maskierung von Beginn weg und wschl. Umprojektion des
## Gebäudevektors außerhalb von R
rasters <- prepare_rasters(file_paths, tile_codes[1])



#### Rasterinformationen in Tabelle (data.table) zusammenführen:
## Dauer: 1.69 s pro Kachel, falls nicht mehrfache zonale Auswertungen gemacht
## werden sollen (z. B. Fläche pro Eignungsklasse UND Dachtyp)
## falls multizonale Auswertungen gemacht werden sollen: 8,47 s (!) pro Kachel

source("helpers.R")
extract_rasters(rasters) |> prettify_dataframe()


#### Durchschleifen mehrerer Kacheln, Ausgabe in MySQL-Datenbank:
### Datenbankverbindung öffnen; falls nicht vorhanden, wird Datenbank
### dieses Namens angelegt:


dbDisconnect(conn) ## ggf. bestehende Verbindung schließen
## Verbindung öffnen
conn <- dbConnect(drv = SQLite(), dbname = './output/solarpotenzial.sqlite')

## Ausgabe-table "raw" anlegen:
prepare_db_output_table(conn, 'raw')

## Log-Datei anlegen (laufende Kachelnummer, Fehlermeldungen etc. in Datei)
##
sink() ## ggf. bestehenden Ausgabekanal schließen
sink(file = "./output/process.log", append = TRUE,
     type = c("output", "message"),
     split = FALSE
     )

source('./helpers.R')

#### tile codes durchschleifen:
  c(1) |> ## ggf. ab schon prozedierter Kachel fortsetzen:
  Map(f = \(i){
    if(!tile_codes[i] %in% c('26425-46800')){
    calc_and_save(file_paths, 
                  tile_code = tile_codes[i],
                  conn = conn,
                  i = i,
                  save_excels = FALSE,
                  export_images = FALSE
                  )
    }
  })


  
  
dbDisconnect(conn)
## Ausgabe in Logdatei schließen:
sink()


d <- rasters |> extract_rasters() |> 
   prettify_dataframe() |> 
  head()




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


