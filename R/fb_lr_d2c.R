##### Distance front / back

############


fb_lr_d2c = function ( data, centroid, dist2cent, hz = 5, plott = F){

  ############

  # vars

  # data = data
  # centroid = cent
  # dist2cent = d2c
  # hz = 5
  # plott = T

  #  centroid speed and heading

  centroid = cbind(centroid,
                   head = get_heading( lon1 =  centroid[,"lon"], lat1 = centroid[,"lat"], indivs = 1),
                   speed =   get_dist( lon1 =  centroid[,"lon"], lat1 = centroid[,"lat"], method = "speed" , hz = hz))
  head(centroid)



  # frontback / leftright dist 2 centroid

  frontback.res.cent = array( NA, c(nrow(data) , dim(data)[3]))
  leftright.res.cent = array( NA, c(nrow(data) , dim(data)[3]))

  for ( j in 1:dim(data)[3]){
    ta <- centroid[,"lon"]
    tb <- data[,"lon", j]
    dl <- data[,"lat", j] - centroid[, "lat" ]
    X <- cos(tb)*sin(dl)
    Y <- (cos(ta)*sin(tb))-(sin(ta)*cos(tb)*cos(dl))
    focal.pos <- atan2(Y,X)
    focal.rel.pos.centroid <- ifelse (focal.pos-centroid[,"head"]<=-pi,
                                      focal.pos+(2*pi)-centroid[,"head"],
                                      ifelse(focal.pos-centroid[,"head"]>=pi,
                                             focal.pos-(2*pi)-centroid[,"head"],
                                             focal.pos-centroid[,"head"]))
    frontback.res.cent[,j] <- cos(focal.rel.pos.centroid)*dist2cent[,j]
    leftright.res.cent[,j] <- sin(focal.rel.pos.centroid)*dist2cent[,j]


  }

  if ( plott){
  boxplot( frontback.res.cent)
  boxplot( leftright.res.cent)
  }

  return( list(frontback.res.cent, leftright.res.cent))
}

