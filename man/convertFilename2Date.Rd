\name{convertFilename2Date}
\alias{convertFilename2Date}
\title{Convert file name patterns to R date object}
\description{This function converts parts of netCDF file names to date strings (in case the
file name contains date information). This is used, e.g.
in transNcdfCutFiles.   }
\usage{convertFilename2Date(file.names, fun.extr.string, fun.conv.string)}\arguments{
  \item{file.names}{character vector: names of the files}
  \item{fun.extr.string}{function}
  \item{fun.conv.string}{function}
}

\value{POSIXct object with the date.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{transNcdfCutFiles}}  }

