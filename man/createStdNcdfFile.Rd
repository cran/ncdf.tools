\name{createStdNcdfFile}
\alias{createStdNcdfFile}
\title{Create an empty netCDF file with standardized attributes and dimensions}
\description{This function writes an empty netCDF file with variable names, dimensions and
attributes formatted in a standardized way.}
\usage{createStdNcdfFile(var.names, file.name = c(), units = "[]", lat.values = c(), 
    long.values = c(), time.values = c(), add.dims = list(), 
    lat.length = length(lat.values), long.length = length(long.values), 
    time.length = length(time.values), year.start.end = c(), 
    scale_factor = 1, add_offset = 0, type.var = "NC_DOUBLE", 
    missing_value = -9999, con.atts = c(), data = c())}
\arguments{
  \item{var.names}{character string: name of the target variable in the file}
  \item{file.name}{character string: name of the file. If not given, this
is determined automatically in a standardized way from 
the variable name and the dimension extends.}
  \item{units}{character string: units of variable (should be compatible with udunits)}
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
  \item{year.start.end}{integer vector (length two): start and end year.
If not given, this is determined from the time
vector.}
  \item{scale_factor}{numeric: scale factor}
  \item{add_offset}{numeric: offset}
  \item{type.var}{character string: type of the data}
  \item{missing_value}{numeric: missing data value}
  \item{con.atts}{RNetCDF file connection: Possible file to use as source
for copying attributes to the new file.}
  \item{data}{
}
}

\value{character string: name off the file created.}

\author{Jannis v. Buttlar}





