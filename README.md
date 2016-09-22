# tsconvert
R library to read and convert time series data of sensors, data
    loggers and urban drainage model outputs.
    
This package has been developed in the course of the project "STBMOD" (https://www.fh-muenster.de/forschung/forschungskatalog/projekt.php?pr_id=722), 
funded by the German Federal Ministry of Education and Research (BMBF, FKZ 03FH033PX2).

Install using devtools:

``` r
if(!require(devtools)) {
  install.packages('devtools')
  devtools::install_github("dleutnant/tsconvert")
}
```