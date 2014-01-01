\name{convertDateR2Ncdf}
\alias{convertDateR2Ncdf}
\title{Convert time vectors in netCDF files to Julian days since a certain origin   }
\description{This function automatically converts time vectors in netCDF files to a standardized Gregorian calendar}
\usage{convertDateR2Ncdf(ncdf.obj, date.vec = "auto", origin = "1800-01-01", 
    write.to.ncdf = TRUE)}\arguments{
  \item{ncdf.obj}{character string or netCDF connection: netCDF file for which to convert the dates}
  \item{date.vec}{POSIXct vector: date vectors for the time dimension. If set to 'auto', this
is tried to be extracted from the netCDF file}
  \item{origin}{character string: origin to be used for the time vector. This start of the
Gregorian calendar should be kept to avoid possible mistakes due to flawed
conversions.}
  \item{write.to.ncdf}{logical: whether to write the time vector to the netCDF file.}
}
\details{This function sets a time vector in a netCDF file to a standardized format which is readable by
most software. It transfers the time vector to days since the start of the Gregorian calendar.}
\value{(invisibly): the time vector. Additionally the time vector is written to the respective file.}

\author{Jannis v. Buttlar}





