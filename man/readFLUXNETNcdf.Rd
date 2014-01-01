\name{readFLUXNETNcdf}
\alias{readFLUXNETNcdf}
\title{read data from FLUXNET NetCDF file.}
\description{This function reads data from standard (BGI - Jena) FLUXNET netCDF files and returns it in
an R object.}
\usage{readFLUXNETNcdf(path = getwd(), sites, pars = "all", time.ends, 
    dim.borders = list(1))}\arguments{
  \item{path}{character string: path to the input file(s)}
  \item{sites}{character string: ids of the sites to extract}
  \item{pars}{character string: names of the variables to extract}
  \item{time.ends}{POSIXct object: start and end date of the period to extract.}
  \item{dim.borders}{list: indices for other dimensions}
}

\value{array: FLUXNET data}

\author{Jannis v. Buttlar}





