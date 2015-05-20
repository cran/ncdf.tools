\name{modifyNcdfDefAtts}
\alias{modifyNcdfDefAtts}
\title{Define a set netCDF attributes at once}
\description{Easily define a couple of attributes for a single netCDF variable in one step.}
\usage{modifyNcdfDefAtts(file.con, var.id, atts)}
\arguments{
  \item{file.con}{a NetCDF object pointing to the respective netCDF file.}
  \item{var.id}{the variable id (integer) or name (string) for which to define attributes.}
  \item{atts}{list: the attributes to define (see details or an example).}
}
\details{The atts attribute should be a list with as many elements as attributes should be added to the
variable in the netCDF file. The names of the attributes are taken from the names of the
elements of this list and the attribute values are defined by the values of the list elements.
The type/class of the attribute (values) is determined automatically.}


\author{Jannis v. Buttlar}



\seealso{\code{\link[RNetCDF]{att.put.nc}}}
\examples{
## needs an open connection to a valid netCDF file pointed to by file.con
attributes.define <- list(LongName = 'This is the long name',
                          missingValue = -99999,
                          units = 'm/s')
library(RNetCDF)
file.con   <- create.nc('test.nc')
dim.def.nc(file.con, 'testdim')
var.def.nc(file.con, 'test', 'NC_CHAR', 'testdim')
modifyNcdfDefAtts(file.con, 'test', atts = attributes.define)

## show all attributes
infoNcdfAtts(file.con, 'test')
}
