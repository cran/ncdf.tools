\name{readNcdfVarName}
\alias{readNcdfVarName}
\title{Get name of variable in netCDF file}
\description{readNcdfVarName tries to automatically detect the name of the "main" variable in a netCDF file. The name returned is the
name of a non coordinate variable. If more than one of such variables are existent, the name of the variable
which spans all available dimensions or with a name appearing as a pattern in the file name is used.}
\usage{readNcdfVarName(file)}
\arguments{
  \item{file}{connection to the netCDF file.}
}

\value{character string: name of the variable.   }

\author{Jannis v. Buttlar}



\seealso{\code{\link[RNetCDF]{RNetCDF}}, \code{\link{infoNcdfVars}}}

