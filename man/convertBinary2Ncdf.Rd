\name{convertBinary2Ncdf}
\alias{convertBinary2Ncdf}
\title{transform binary file to netCDF file}
\description{This function transforms a binary data file to a netCDF file formatted
in a standardized way.}
\usage{convertBinary2Ncdf(file.input, date.vec, length = 1, type = numeric(), 
    type.ncdf = "NC_DOUBLE", dimensions, dimension.values, signed = TRUE, 
    var.name, long_name = var.name, var.units = "[]", scale.factor.in = 1, 
    scale.factor.out = scale.factor.in, na.value.in = -9999, 
    na.value.out = na.value.in, offset.in = 0, offset.out = offset.in)}
\arguments{
  \item{file.input}{character string: name of the input file.}
  \item{date.vec}{R date object: time vector for the time coordinate}
  \item{length}{integer: Length in bytes of each entry in the input file.}
  \item{type}{R data type of the data in the input file.}
  \item{type.ncdf}{character string: Desired data type in the netCDF file.}
  \item{dimensions}{character vector: Names of the dimensions in the binary file.}
  \item{dimension.values}{list: Each list element has to contain the coordinate values for
the respective dimension.}
  \item{signed}{logical: Whether the binary file contains signed integer values.}
  \item{var.name}{character string: Short name of the variable in the binary file
(used for the meta data in the NetCDF file).}
  \item{long_name}{character string: long name of the variable in binary file
(used for the meta data in the NetCDF file).}
  \item{var.units}{character string: units of the variable
(used for the meta data in the NetCDF file).}
  \item{scale.factor.in}{numeric: factor to multiply the binary input data with.}
  \item{scale.factor.out}{numeric: desired scale factor of the data in the netCDF file.}
  \item{na.value.in}{numeric: missing value for input data.}
  \item{na.value.out}{numeric: missing value for output data.}
  \item{offset.in}{numeric: offset for input data.}
  \item{offset.out}{numeric: offset for output data.}
}

\value{Nothing is returned but a netCDF file with a standardized name is written
in the working directory.}

\author{Jannis v. Buttlar}





