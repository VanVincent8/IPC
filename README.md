# CPI 

INE eases the access to its datasets through an API. By defining a specific URL,
we can carry out petitions of any type of data (time series, tables, etc). For this 
case, we want to download the table of CPI of different regions and dissagregated 
in categories and core CPI. An example of url to download this type of data would
be:
  
`https://servicios.ine.es/wstempus/js/ES/DATOS_METADAOPERACIÓN/IPC?g1=115:29&g2=3:74&g3=762:&nult=1`

We will only focus on `/DATOS_METADATAOPERACION/IPC?g1=70:9009&g2=3:74&g3=762:&p=1&nult=1`.

- `/DATOS_METADATAOPERACION` is the type of data. There are many and each one has 
different features.
- `/IPC` is the variable of interest. After that, there is a `?` which precedes some parameters
of the search.
- `g1=115:29`, `115` refers to the code of geographical level. In this case, it is provincial 
level and the province is Madrid, with code 29. Table ___codigo_provincias___ shows all the codes.
- `g2=3:74`, `3` selects the group TYPE OF DATA that for this particular case is anual variation.
See table ___codigos_tipo_dato___ for more options.
- `g3=762`, with this code, we are able to select all the categories in which the CPI 
is computed. See table ___codigos_grupos_ECOICOP___ for more information.
- `nult=10`, the last published data, then `nult=1` will return the last 10 observations
of the series.

If you want to download any type of table, I leave here the webpage where you can
define your own URL to access to any dataset. See https://www.ine.es/dyngs/DataLab/es/manual.html?cid=45.
