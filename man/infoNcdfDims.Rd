\name{infoNcdfDims}
\alias{infoNcdfDims}
\title{Show info about all dimensions in a netCDF file}
\description{This function displays summary information about all dimensions in a netCDF file.}
\usage{infoNcdfDims(file.con, extended = TRUE)}
\arguments{
  \item{file.con}{a NetCDF object pointing to the respective netCDF file.}
  \item{extended}{logical: if TRUE, some extended dimension info that
may take time to compute for large files is computed.}
}

\value{A matrix containing the id, name, length, range and step (columns) of all dimensions (rows)}

\author{Jannis v. Buttlar}



\seealso{\code{\link{infoNcdfVars}}, \code{\link[RNetCDF]{dim.inq.nc}}}

