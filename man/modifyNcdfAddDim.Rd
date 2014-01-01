\name{modifyNcdfAddDim}
\alias{modifyNcdfAddDim}
\title{Add a new dimension to one or more variables in a netCDF file}
\description{Adds another dimension to specified variables in a netCDF file and saves the results in
another netCDF file.}
\usage{modifyNcdfAddDim(file.con.orig, file.con.copy, var.name = "Default", 
    dim.name = "new.dim", dim.values = c(), dim.length = length(dim.values), 
    dim.pos.copy = 1)}\arguments{
  \item{file.con.orig}{a NetCDF object pointing to the respective netCDF file FROM which to copy}
  \item{file.con.copy}{a NetCDF object pointing to the respective netCDF file TO which to copy}
  \item{var.name}{character vector: names of the variables to which a dimension should be added. Defaults to
all except those with identical names as dimensions in file.con.orig (coordinate variables)}
  \item{dim.name}{character string: name of the dimension to add}
  \item{dim.values}{numeric/character vector with the values for the dimension (coordinate values)}
  \item{dim.length}{integer: length of the dimension to add}
  \item{dim.pos.copy}{integer: position in the new dimension where to copy the original data. If set to 0,
no values are copied and the variable in the new file will be empty. Setting to
1 (default) results in the original values to be filled in the first value of
the new dimension and the remaining values left empty (NaN).}
}



\author{Jannis v. Buttlar}



\seealso{\code{\link{modifyNcdfCopyMetadata}}, \code{\link[RNetCDF]{att.copy.nc}},
\code{\link{modifyNcdfCopyVar}}}

