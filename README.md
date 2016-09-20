# tsconvert
R library to read and convert time series data of sensors, data
    loggers and urban drainage model outputs.
    
This package has been developed in the course of the project "STBMOD" (https://www.fh-muenster.de/forschung/forschungskatalog/projekt.php?pr_id=722).

Install using devtools:

``` r
if(!require(devtools)) {
  install.packages('devtools')
  devtools::install_github("dleutnant/tsconvert")
}
```