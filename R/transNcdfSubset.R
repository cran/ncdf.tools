transNcdfSubset <- function(
  ##title<< Cut and save a subset of a netCDF file
  file.name ##<< character string: name of the file to create. 
  , dim.values = list(latitudes =c(), longitudes=c(), timesteps=c())
  , values.type =c('range', 'indices', 'values')[2] ##<< character string:
           ## type of the dim.values supplied. 'range' means that the lower an upper
           ## border are supplied, 'indices' means that 1:n indices are supplied,
           ## 'values' would imply actual coordinate values. 
  , filename.new = sub('[.]nc', '_subs.nc', file.name)) ##<< character string: name
           ## of the results file.
##description<<
## This function reads a subset of lat/lon/time values out of a netCDF file and creates
## a new netCDF file with the results.
{
  ##ToDo facilitate other scenarios than lat/lon/time
  ##TODO merge with transNcdfCutTimes

  #determine dimension indices
  dim.indices=list(latitudes=c(), longitudes=c(), timesteps=c())
  con.source <- open.nc(file.name)
  dimvals.src        <- readNcdfCoordinates(con.source)
  if (sum(names(dimvals.src) != c('latitude', 'longitude', 'time')) > 0)
    stop('Dimensions (or their names) in file not latitude/longitude/time. Transpose (or rename)!')
  if (values.type == 'range') {
    for (i in 1:3) dim.indices[[i]] <-  which(dimvals.src[[i]] >= min(dim.values[[i]]) & dimvals.src[[i]] <= max(dim.values[[i]]))

    
  } else  if (values.type == 'indices') {
    dim.indices = dim.values
  } else if (values.type == 'values') {
    for (i in 1:3) 
      dim.indices[[i]] <- match(dim.values[[i]], dimvals.src[[i]])
  } else {
    stop(paste('Value for values.type of \'', values.type, '\' not supported!', sep = ''))
  }

  # create target file
  lat.values.target  <- dimvals.src$latitude[dim.indices$latitudes]
  lon.values.target  <- dimvals.src$longitude[dim.indices$longitudes]
  time.values.target <- dimvals.src$time[dim.indices$timesteps]
  var.name.new <- sub('.*/', '', sub('[.]nc', '', filename.new))
  createLatLongTime(var.names = var.name.new, file.name = filename.new,
                    lat.values = lat.values.target, long.values = lon.values.target,
                    time.values = time.values.target)
  con.target <- open.nc(filename.new, write = TRUE)
  modifyNcdfCopyAtts(con.source, 'time', 'time', con.target)

  vars.copy.atts <- c('latitude', 'longitude', 'time')
  for (var.copy.t in vars.copy.atts)
    modifyNcdfCopyAtts(con.source, var.copy.t, var.copy.t, con.target)
  

  # load, transpose and subset source data
  data.orig   <- transNcdfRotate(con.source)
  close.nc(con.source)
  data.target <- array(data.orig[indexDimVecs2Matrix(dim.indices$latitudes,
                                                      dim.indices$longitudes,
                                                      dim.indices$timesteps)],
                       dim =  sapply(dim.indices, length))

  # write to target
  var.put.nc(con.target, var.name.new, data.target)
  close.nc(con.target)
  cat(paste('Finalized file ', filename.new, '\n', sep = ''))
  ##value<< character string: name of the file created.
  invisible(filename.new)
}


