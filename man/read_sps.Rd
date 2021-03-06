% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sps_ms.R
\name{read_sps}
\alias{read_sps}
\title{Read time series data of type .txt (SPS)}
\usage{
read_sps(file, verbose = FALSE)
}
\arguments{
\item{file}{The file to be read}

\item{verbose}{logical. Should informative outputs printed during function evaluation?}
}
\value{
A list of xts-objects.
}
\description{
Reads the .txt structured time series data output usually used at 
sps of waste water treatment plants.
}
\seealso{
\code{\link[xts]{xts}}.
}
\keyword{internal.}

