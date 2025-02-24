# Doku

## Vorbereitungen
- Geopackage mit Gebäuden wurde von LAEA auf EPSG 31287 (Lambert Austria) umprojiziert, in dem die Eingangsraster vorliegen; 
enthält nur mehr den eigentlichen Gebäudelayer und die OBJECTID als einziges Attribut


## Ordnerstruktur

```
├── helpers.R
├── input
│   ├── 26850-47525
│   │   ├── 26850-47525_DOM.tif
│   │   └── GLO_real
│   │       └── 26850-47525_GLO_real_Jahressumme.tif
│   ├── 27475-45475
│   │   ├── 27475-45475_DOM.tif
│   │   └── GLO_real
│   │       └── 27475-45475_GLO_real_Jahressumme.tif
│   └── DEM
│       ├── DLM_8000_Bauwerk_20241118.gpkg
│       ├── DLM_EPSG3035.gpkg
│       └── DLM_EPSG31287.gpkg
├── main.R
├── output ## hier die Ergebnisse für die Beispielkacheln
│   ├── 26850-47525_aspect.tiff
│   ├── 26850-47525_buildings.tiff
│   ├── 26850-47525_dom.tiff
│   ├── 26850-47525_glo.tiff
│   ├── 26850-47525_outliers.tiff
│   ├── 26850-47525_rooftype.tiff
│   ├── 26850-47525_suit.tiff
│   ├── 27475-45475_aspect.tiff
│   ├── 27475-45475_buildings.tiff
│   ├── 27475-45475_dom.tiff
│   ├── 27475-45475_glo.tiff
│   ├── 27475-45475_outliers.csv
│   ├── 27475-45475_outliers.tiff
│   ├── 27475-45475_rooftype.tiff
│   ├── 27475-45475_suit.tiff
│   ├── aspect_example.png
│   ├── data_26850-47525.csv
│   ├── data_27475-45475.csv
│   ├── outliers_example_1.png
│   ├── outliers_vs_buffer.png
│   └── solarpotenzial.db
├── README.html
├── README.md
└── R.Rproj


```
## Konstanten
Diese Liste am Eingang des Hauptskripts `main.R`
enthält alle user servicable parts: Neigungslimit für Flachdächer,
Nutzbarkeitsklassen etc.

```
constants <- list(
  ## crs = 31287, ## Lambert Austria; nicht mehr benötigt
  flat = 10,  # Schwellenwert (°), unter dem Dach als flach angenommen wird
  min_cluster_size = 9, # erforderliche Mindestgröße für die Dachfläche (in Pixel = m²)
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

```
## Methodik

### Workflow
- Der Gebäudelayer wird für alle weiteren Berechnungen mit Auflösung und Ausdehnung
der jeweiligen Kachel gerastert. Dabei wird alles außerhalb Gebäuden als NA (Null)
gerastert. Damit werden sämtliche anderen Raster maskiert.

- Neigung und Aspekt werden aus den 4 orthogonal angrenzenden Pixeln berechnet
(nicht wie ursprünglich aus den 8 umrandenden). Dadurch reichen auch die Neigungs-
und Aspektzellen bis an den Dachrand (davor fiel ein Saum von 1 Pixelbreite am
Dachrand als NA aus)

- Vorschlag: Neigung zum Beschneiden der Dachfläche verwenden



### Ausscheiden zu kleiner Dachflächen
Die Ausscheidung zu kleiner Dachflächen erfolgt dzt. nicht mit einem rechteckigen
moving window, sondern über die Größe zusammenhängender Dachpixel (m²). Damit
werden auch unregelmäßige Formen ("Tetris-Blöcke erfasst").
Grund ist der drastische Geschwindigkeitsunterschied (Millisekunden vs. Sekunden)
zwischen den Berechnungsarten. Da die Dächer ohnehin häufig schräg zum Gitter
orientiert sind, würde auch das rechteckige moving window die Wirklichkeit nicht
unbedingt besser wiedergeben.


## Ergebnisse
### Einfluss Pufferdistanz
Bei ca. 1,5 m Puffer (Erweiterung des Gebäudeumrissen treten die meisten Höhen-Ausreißer auf, von da weg nehmen sie rel. linear in beide Richtungen
ab. Eine markante Änderung der Ausreißerzahl bei einer bestimmten Pufferdistanz zur Optimierung der Distanz ist leider nicht erkennbar.)


<img src="output/outliers_vs_buffer.png" height=500/>


### Höhenausreißer
Die jetzige Ausreißerdefinition würde Flachdächer desselben Gebäudes bei größeren Niveauunterschieden ausscheiden:
<img src="output/outliers_example_1.png"/>


### Kontrolle Aspekt
Unterscheidung nach acht Himmelsrichtungen am Beispiel der Domkuppel (Salzburg, Kachel 27475-45475)

<img src="output/aspect_example.png"/>
