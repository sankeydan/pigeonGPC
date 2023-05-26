lims = function( data, flight.start.est = 30){
  limmax = max( diff( range ( na.omit(data[,"lon",]))),
                diff( range ( na.omit(data[,"lat",]))))/2
  y.start = median( as.vector(data[1:flight.start.est,"lat",]),na.rm = T)
  x.start = median( as.vector(data[1:flight.start.est,"lon",]),na.rm = T)
  ylim = y.start + ( c ( - limmax ,  limmax))
  xlim = x.start + ( c ( - limmax ,  limmax))

  return( list( xlim, ylim ))
}
