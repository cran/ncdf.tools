\name{transNcdfSubset}
\alias{transNcdfSubset}
\title{Cut and save a subset of a netCDF file}
\description{This function reads a subset of lat/lon/time values out of a netCDF file and creates
a new netCDF file with the results.}
\usage{transNcdfSubset(file.name, dim.values = list(latitudes = c(), 
    longitudes = c(), timesteps = c()), values.type = c("range", 
    "indices", "values")[2], filename.new = sub("[.]nc", "_subs.nc", 
    file.name))}\arguments{
  \item{file.name}{character string: name of the file to create.}
  \item{dim.values}{
}
  \item{values.type}{character string:
type of the dim.values supplied. 'range' means that the lower an upper
border are supplied, 'indices' means that 1:n indices are supplied,
'values' would imply actual coordinate values. }
  \item{filename.new}{character string: name
of the results file.}
}

\value{character string: name of the file created.}

\author{Jannis v. Buttlar}





