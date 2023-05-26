pair.flight = function( data, paired_prop = 0.5 , fis_dist = 50){
  if ( length(dim(data)) != 3){
    stop("Data must be a gGPS object")
  }
  nam = dimnames(data)[[3]]
  len = length(nam)
  mat = matrix(0,len,len,dimnames = list(nam,nam))
  for ( j in 1:len){
    for ( k in 1:len){
      # k = 4
      # j = 1
      dist = na.omit(
        get_dist(data[,"lon",j],data[,"lat",j],
                 data[,"lon",k],data[,"lat",j],method = "distance" , hz=5))
      len.flight = length(na.omit(data[,"lon",j]))
      len.pair = length( which (dist < fis_dist))
      prop = len.pair/len.flight
      mat[j,k] = ifelse( prop > paired_prop,1,0)
    }
  }
  diag(mat) = 0
  rowsumm = rowSums(mat)
  paired = ifelse(rowsumm > 0 , "paired" , "solo")
  return( paired)
}
