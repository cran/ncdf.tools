\name{transNcdfRotate}
\alias{transNcdfRotate}
\title{Transpose a NetCDF datacube}
\description{transNcdfRotate is a convenience function to transpose a datacube
arranged in an arbitrary dimension order into a datacube with dimensions [
latitude (decreasing), longitude (increasing), time (increasing)].}
\usage{transNcdfRotate(data.object, file.name.out = "none", file.con = c(), 
    var.name = c(), reverse.dim = TRUE)}
\arguments{
  \item{data.object}{RNetCDF file connection or R array: data object to be transposed.}
  \item{file.name.out}{character string: name of the
netCDF file created for the results. Default 'none' means that no
results file is created.}
  \item{file.con}{RNetCDF file connection: link to the data object to be transposed.
Supplying both data.object and file.con only makes sense if data.object
is an array which saves time as the data does not have to be loaded again.              }
  \item{var.name}{character string: name of the variable to transpose. If
not gives, this name is tried to be inferred by using readNcdfVarName.}
  \item{reverse.dim}{logical: whether to reverse the sequence of the dimensions
in case they are not in the following order : latitude: descending,
longitude: ascending,  time: ascending.}
}

\value{list: data:transposed datacube and aperm.reverse: an index vector that
can be supplied to aperm to reverse the transformation.}

\author{Jannis v. Buttlar}





