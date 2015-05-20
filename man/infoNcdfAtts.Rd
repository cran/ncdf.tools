\name{infoNcdfAtts}
\alias{infoNcdfAtts}
\title{Print a summary of all netCDF variable attributes}
\description{This function returns a summary of all attributes of a single variable
in a netCDF file. }
\usage{infoNcdfAtts(file.con, var.id = "NC_GLOBAL")}
\arguments{
  \item{file.con}{a NetCDF object pointing to the respective netCDF file.}
  \item{var.id}{the name or id of the variable for which to display attributes.}
}
\details{If an id or variable name is given for 'var.id ', attributes from one variable are returned.
Global attributes are returned if 'NC_GLOBAL' is given. }
\value{A matrix containing the name, value and type (columns) of all attributes (rows)}

\author{Jannis v. Buttlar}



\seealso{\code{\link{infoNcdfDims}}, \code{\link{infoNcdfVars}}, \code{\link[RNetCDF]{att.inq.nc}}}

