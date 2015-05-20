\name{modifyNcdfSetMissing}
\alias{modifyNcdfSetMissing}
\title{Set missing value attribute to a netCDF file}
\description{This function sets the missing_value and the _Fill_value of a
variable in a netCDF file to a given value (-9999 by default).}
\usage{modifyNcdfSetMissing(con, var, value = -9999)}
\arguments{
  \item{con}{file connection to modify}
  \item{var}{variable name (or index) of the variable to modify}
  \item{value}{value of the missing value attribute}
}



\author{Jannis v. Buttlar}





