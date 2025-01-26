## R-Scripts:

- main.R: Hauptroutine
- helpers.R: enthält alle Hilfsfunktionen, wird in main.R ge`source`d

### main.R
hier werden alle Konstanten gesetzt:
```
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
```


Ordnerstruktur (Beispiel):

root
├── helpers.R
├── input
│   ├── DOM
│   │   ├── 26850-47525_DOM.tif
│   │   └── 26850-47525_DOM.tif.aux.xml
│   ├── GEB
│   │   └── DLM_8000_Bauwerk_20241118.gpkg
│   └── GLO
│       ├── 26850-47525_GLO_real_Jahressumme.tif
│       └── 26850-47525_GLO_real_Jahressumme.tif.aux.xml
├── main.R
└── output
    ├── 26850-47525.tiff
    ├── 26850-47525.tiff.aux.xml
    └── data_26850-47525.csv

