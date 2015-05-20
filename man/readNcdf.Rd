\name{readNcdf}
\alias{readNcdf}
\title{Easy reading of netCDF data}
\description{Convenience function to automatically read in data from a netCDF file
without specifying variable names and opening and closing file connections.}
\usage{readNcdf(file.name, var.name = c())}
\arguments{
  \item{file.name}{character string: name of the netCDF file file to read the data from.}
  \item{var.name}{character string: name of the variable to extract. If not supplied,
this is tried to be determined with readNcdfVarName().}
}

\value{(multidimensional) array: data from the netCDF file.}

\author{Jannis v. Buttlar}





