\name{plotDatacube}
\alias{plotDatacube}
\title{Visualize/plot an overview of a netCDF file}
\description{This function plots some overview statistics of a netCDF file.
\if{html}{\out{<img src="../doc/visualize_ncdf_demo.png" alt="image ..visualize_ncdf_demo should be here"/>}}\ifelse{latex}{}{}}
\usage{plotDatacube(data.object, data = c(), fourth.dim = 0, var.name = "auto", 
    parallel = FALSE, max.cores = 16, n.series = 16, lwd = 2, 
    ...)}\arguments{
  \item{data.object}{object to plot: file name or file.con object linking to a netCDF file}
  \item{data}{array: data to plot. Can be passed to the function to
prevent the repeated loading of huge netCDF data.}
  \item{fourth.dim}{position in possible forth dimension (height, spectral band etc) to plot}
  \item{var.name}{character string: name of the variable to plot}
  \item{parallel}{logical: Whether to parallelize the computations.}
  \item{max.cores}{integer: maximum amount of cores to use for the
parallelized computations.}
  \item{n.series}{integer: amount of example series to plot}
  \item{lwd}{integer: graphical parameter, see ?par}
  \item{\dots}{
}
}

\value{some overview statistics of the different datacubes.}

\author{Jannis v. Buttlar}





