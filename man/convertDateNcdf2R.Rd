\name{convertDateNcdf2R}
\alias{convertDateNcdf2R}
\title{Convert netCDF time vector to POSIXct R date object}
\description{This function converts a time vector from a netCDF file or a vector of Julian days (or seconds, minutes, hours)
since a specified origin into a POSIXct R vector.}
\usage{convertDateNcdf2R(time.source, units = "days", origin = as.POSIXct("1800-01-01", 
    tz = "UTC"), time.format = c("\%Y-\%m-\%d", "\%Y-\%m-\%d \%H:\%M:\%S", 
    "\%Y-\%m-\%d \%H:\%M", "\%Y-\%m-\%d \%Z \%H:\%M", "\%Y-\%m-\%d \%Z \%H:\%M:\%S"))}
\arguments{
  \item{time.source}{numeric vector or netCDF connection: either a number of time units since
origin or a netCDF file connection, In the latter case, the time 
vector is extracted from the netCDF file, This file, and especially the 
time variable, has to follow the CF netCDF conventions.}
  \item{units}{character string: units of the time source. If the source
is a netCDF file, this value is ignored and is read from that file.}
  \item{origin}{POSIXct object:
Origin or day/hour zero of the time source. If the source
is a netCDF file, this value is ignored and is read from that file.}
  \item{time.format}{
}
}

\value{POSIXct vector: time vector in native R format}

\author{Jannis v. Buttlar}





