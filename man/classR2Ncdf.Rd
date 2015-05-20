\name{classR2Ncdf}
\alias{classR2Ncdf}
\title{transfers R classes to netCDF classes}
\description{Crudely determines the netCDF class from R classes. Only integer, character
and double are implemented yet.}
\usage{classR2Ncdf(object)}
\arguments{
  \item{object}{object which class should be determined}
}

\value{character string: netCDF class used in the RNetCDF package.}

\author{Jannis v. Buttlar}



\seealso{\code{\link[RNetCDF]{RNetCDF}}}

