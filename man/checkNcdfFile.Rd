\name{checkNcdfFile}
\alias{checkNcdfFile}
\title{check netCDF file for consistency with CF/COARDS/BGI netCDF conventions}
\description{This function checks whether a netCDF file is consistent with the parts of the COARDS/CF
netCDF conventions used in the BGI department (MPI for Biogeochemistry,  Jena,  Germany).}
\usage{checkNcdfFile(file.name, dims = c("longitude", "latitude", "time"), 
    type = "strict", var.check = "single")}\arguments{
  \item{file.name}{character string: file name to check}
  \item{dims}{vector of strings:
names of the dimensions which need to be in the file.}
  \item{type}{character string:
if 'strict', then all aspects are checked. If this
is any other value, only aspects relevant for the processing of
decomp.ncdf are checked.}
  \item{var.check}{character string: If 'single', then readNcdfVarName
is used to infer the name of the variable in the target file
which will then be checked,}
}

\value{logical: (invisible) whether the file passed the check}

\author{Jannis v. Buttlar}





