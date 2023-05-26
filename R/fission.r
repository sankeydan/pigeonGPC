fission = function (data,
                    fis_dist = 50,
                    plot = F,
                    cut_radius_home = 550, # 500 was chosen distance to home parameter. 50 is fission distance (as we are not expecting the centroid to reach 500 metres from homw at the exact time the first individual does)
                    return_omitted_individuals = T,
                    trial = "") {

  # objects

  # data= data
  # fis_dist = fis.dist
  # plot = F
  # cut_radius_home = 550
  # return_omitted_individuals = T
  # trial = i

  # objects
  dims = dim(data)


  # load metadata
  rsc = data.frame (lat = c(51.415368, 51.457547, 51.496972, 51.44844, 51.46375),
                    lon =  c(-0.572615, -0.588399, -0.588914, -0.480168, -0.395744), site =  c("home" ,"N4" ,  "N8" ,  "E4" ,  "E8")  )

  home = rsc[rsc$site == "home",]

  # distance to centroid
  d2c = function( dat, return_centorid = F) {

    dist2c = apply( dat[,c("lon","lat"),],1,function(x){
      #x=data[1,c("lon","lat"),]
      clon = mean( x["lon",],na.rm = T)
      clat = mean( x["lat",],na.rm = T)
      return(get_dist(x[1,],x[2,],clon,clat,method= "distance"))
    })
    dist2c = t(dist2c)

    if ( !return_centorid){
      return(dist2c)
    } else{
      centroid = cbind( lon = rowMeans(dat[,"lon",],na.rm=T),
                        lat = rowMeans(dat[,"lat",],na.rm=T))
      return(list(dist2c,centroid))
    }

  }

  foo = d2c(data,return_centorid = T)
  dist2cent = foo[[1]]

  # centroid of whole group regardless of fission
  centroid = as.data.frame(foo[[2]])

  # Fission
  fis.data = data
  omit.list = list() # a list will store omitted individuals.
  for ( j in 1:nrow(dist2cent)){ # for each timestamp
    #j=1
    vec = vector() # set up a vector to record ommitted individuals
    while( length( which(dist2cent[j,]  > fis_dist)) >0){ # while there is at least one individual over the fission distance threshold
      omit = which(dist2cent[j,]== max(dist2cent[j,], na.rm = T) ) # which individual needs to be removed?
      fis.data[j,c("lat","lon"), omit] = NA # make NA the latitude and the longitude of the furthest individual
      centroid$lon[j] = mean(fis.data[j,"lon",], na.rm = T) # calcutate new centroid
      centroid$lat[j] = mean(fis.data[j,"lat",], na.rm = T)
      for( k in 1:dim(data)[3]){
        dist2cent[j,k] = get_dist( fis.data[j,"lon",k], fis.data[j, "lat", k], centroid$lon[j], centroid$lat[j] , method = "distance") # distance to centroid - same as above
      }
    }
    omit.list[[j]] = which(is.na(dist2cent[j,])) # store in the list
  }

  # remove fissioned individuals from the group
  if(return_omitted_individuals){
    omit.mat = matrix(F, dim(data)[1], dim(data)[3], dimnames = list(NULL, dimnames(data)[[3]]))
  }
  for ( i in 1:dim(data)[1]){
    data[i,1:2,][,omit.list[[i]]] = NA
    if(return_omitted_individuals){
      omit.mat[i,omit.list[[i]]] = TRUE
    }
  }

  # new centroid
  centroid = data.frame( lon = apply(data[,"lon",], 1, function(x){ mean(x,na.rm = T)}),
                         lat = apply(data[,"lat",], 1, function(x){ mean(x,na.rm = T)}))
  dist2home = get_dist(centroid$lon, centroid$lat, rep(home$lon,nrow(data)), rep(home$lat,nrow(data)), method = "distance" )
  end = which(dist2home < cut_radius_home)[1]

  # new dist2centroid
  dist2cent = matrix(NA, dim(data)[1], dim(data)[3] )
  for ( j in 1:nrow(dist2cent)){ # for each timestep
    for ( k in 1:dim(data)[3]){ # for each pigeon
      dist2cent[j,k] = get_dist( data[j,"lon",k], data[j, "lat", k], centroid$lon[j], centroid$lat[j] , method = "distance") # get the distance between the individual and the centroid
    }
  }

  # trim data
  if(!is.na(end)){
  data    [end:dims[1],,] = NA
  omit.mat[end:dims[1],] = T
  centroid[end:dims[1],] = NA
  }
  cent.not.one = which((apply(omit.mat,1,function(x){
    length(which(x == F)) <= 1})))
  data[cent.not.one,,] = NA
  centroid[cent.not.one,] = NA
  omit.mat[cent.not.one,] = T
  dist2home2 = get_dist(centroid$lon, centroid$lat, rep(home$lon,nrow(data)), rep(home$lat,nrow(data)), method = "distance" )

  # plot
  if ( plot){
    par(mfrow = c(4,1))
    par(mar=(c(1,1,1,1)))
    plot(data[,1,1],type="l",ylim = range(as.vector(data[,1,]),na.rm = T),main = trial)
    for ( i in 1:dim(data)[3]){
      lines(data[,1,i],col=i)
    }

    plot(data[,1:2,1],type="l",ylim = range(as.vector(data[,2,]),na.rm = T),
         xlim = range(as.vector(data[,1,]),na.rm = T))
    for ( i in 1:dim(data)[3]){
      lines(data[,1:2,i],col=i)
    }

    plot( dist2home)
    plot(dist2home2)
  }

  #return
  if ( return_omitted_individuals){
    return(list(data, omit.mat,centroid, dist2cent ))
  } else {
  return(data)
  }
}

