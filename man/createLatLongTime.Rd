\name{createLatLongTime}
\alias{createLatLongTime}
\title{Create empty lat/lon/time netCDF file}
\description{this function creates an empty standardized latitude/longitude/time netCDF file.}
\usage{createLatLongTime(file.name, var.names = sub("[.]nc", "", file.name), 
    lat.values = c(), long.values = c(), time.values = c(), add.dims = list(), 
    lat.length = length(lat.values), long.length = length(long.values), 
    time.length = length(time.values), scale_factor = 1, add_offset = 0, 
    type.var = "NC_DOUBLE", missing_value = -9999, units = "[]")}
\arguments{
  \item{file.name}{character string: name of the target file.}
  \item{var.names}{character vector: names of the variables
in the target file.}
  \item{lat.values}{numeric values: coordinate values for the latitude
positions.}
  \item{long.values}{numeric values: coordinate values for the latitude
positions.}
  \item{time.values}{POSIXct vector: time values for the time dimension}
  \item{add.dims}{
}
  \item{lat.length}{integer: length of the latitude dimension}
  \item{long.length}{integer: length of the longitude dimension}
  \item{time.length}{integer: length of the time dimension}
  \item{scale_factor}{numeric: scale factor}
  \item{add_offset}{numeric: offset}
  \item{type.var}{character string: type of the data}
  \item{missing_value}{numeric: missing data value}
  \item{units}{character string: units of the variables in target file.}
}

\value{Nothing is returned but a file is created. 
TODO: units has to work with more than one variable}

\author{Jannis v. Buttlar}





