% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nc.R
\name{read_nc_xts}
\alias{read_nc_xts}
\title{Read time series data from .nc (KALIMOD) file format}
\usage{
read_nc_xts(file, display.overview = FALSE, verbose = FALSE)
}
\arguments{
\item{file}{The file to be read}

\item{display.overview}{logical. Should an overview of the netCDF file printed?}

\item{verbose}{logical. Should informative outputs printed during function evaluation?}
}
\value{
A list of xts-objects.
}
\description{
Reads netCDF structured time series data and creates a list of xts-objects.
}
\seealso{
\code{\link[xts]{xts}}, \code{\link[RNetCDF]{open.nc}}.
}

