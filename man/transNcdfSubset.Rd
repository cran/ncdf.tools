\name{transNcdfSubset}
\alias{transNcdfSubset}
\title{Cut and save a subset of a netCDF file}
\description{This function reads a subset of lat/lon/time values out of a netCDF file and creates
a new netCDF file with the results.}
\usage{transNcdfSubset(file.input, dim.values = list(latitude = c(), 
    longitude = c(), time = c()), values.type = c("range", "indices", 
    "values")[2], file.output = sub("[.]nc", "_subs.nc", file.input), 
    var.name = readNcdfVarName(file.input))}
\arguments{
  \item{file.input}{character string: name of the input ncdf file.}
  \item{dim.values}{
}
  \item{values.type}{character string:
type of the dim.values supplied. 'range' means that the lower an upper
border are supplied, 'indices' means that 1:n indices are supplied,
'values' would imply actual coordinate values. }
  \item{file.output}{character string: name
of the results file.}
  \item{var.name}{
}
}

\value{character string: name of the file created.}

\author{Jannis v. Buttlar}





