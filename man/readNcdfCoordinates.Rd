\name{readNcdfCoordinates}
\alias{readNcdfCoordinates}
\title{Read coordinate or dimension values from netCDF file}
\description{This function reads the coordinate values from a netCDF file.}
\usage{readNcdfCoordinates(fileCon)}\arguments{
  \item{fileCon}{netCDF file connection or character string: Connection to the
netCDF file or its file name. In the latter case, the connection is
created and closed automatically.}
}

\value{A list with the coordinate values (if available) for all dimensions.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{infoNcdfDims}}}

