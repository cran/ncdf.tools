\name{transNcdfCutFiles}
\alias{transNcdfCutFiles}
\title{Cut margins of netCDF files}
\description{Convenience wrapper around cdo to cut outer (time) margins 
of NetCDF files.}
\usage{transNcdfCutFiles(file.names, time.range.out = c(), time.range.file = c(), 
    fun.start = c(), fun.end = c(), format = "", convert = function(x) chron(paste(x, 
        "15", sep = ""), format = "ymd", out.format = "d-m-y"))}\arguments{
  \item{file.names}{vector of character stings: file names to process.}
  \item{time.range.out}{
}
  \item{time.range.file}{POSIXct vector of length two or 'auto': time range of the
original files. If not supplied, this is 
determined automatically from the file name 
via convertFilename2Date and fun.start/fun.end.}
  \item{fun.start}{see time.range.file}
  \item{fun.end}{see time.range.file}
  \item{format}{
}
  \item{convert}{
}
}

\value{character string: names of the file names after cutting.}

\author{Jannis v. Buttlar}





