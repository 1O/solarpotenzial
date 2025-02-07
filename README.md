- Geopackage mit Gebäuden wurde von LAEA auf EPSG 31287 (Lambert Austria) umprojiziert, in dem die Eingangsraster vorliegen; 
enthält nur mehr den eigentlichen Gebäudelayer und die OBJECTID als einziges Attribut


- Ordnerstruktur
```
.
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
│       └── DLM_EPSG31287.gpkg
├── main.R
├── output
└── README.md
```