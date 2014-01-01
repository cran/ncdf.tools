\name{modifyNcdfCopyMetadata}
\alias{modifyNcdfCopyMetadata}
\title{Copy attributes and dimensions between netCDF files}
\description{This function copies all global attributes and/or all dimensions from one
netCDF file to another.}
\usage{modifyNcdfCopyMetadata(file.con.orig, file.con.copy, glob.atts = TRUE, 
    dimensions = TRUE)}\arguments{
  \item{file.con.orig}{a NetCDF object pointing to the respective netCDF file from which to copy}
  \item{file.con.copy}{a NetCDF object pointing to the respective netCDF file to which to copy}
  \item{glob.atts}{logical: whether to copy all global attributes}
  \item{dimensions}{logical: whether to copy all dimensions}
}

\value{nothing is returned.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{modifyNcdfCopyVar}}, \code{\link[RNetCDF]{att.copy.nc}}}

