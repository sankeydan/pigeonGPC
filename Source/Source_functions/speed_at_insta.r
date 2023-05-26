speed_at_insta = function( insta.flock.size,size,data.f,
                           minflight.prop,wind.direct,wind.speed,plotsupportoverspeed= F){
  # size = 3
  # plotsupportoverspeed= T

  whi = which(insta.flock.size == size)
  vars= c("pidge" ,"medspeed" , "meanspeed", "support.wind", "cross.wind","insta.size")
  dims = dim(data.f)
  if (
    {length( whi)/dims[1] > minflight.prop
    }
    ){
    d = data.f[whi,,]
    for( j in 1:dims[3]){
      #j=1
      if( length(which(!is.na(d[,1,j])))/ dims[1] < minflight.prop){
        d[,,j] = NA
      }
    }
    d2 = apply(d[,1:2,],3,function(x){
      #x= data.f[,1:2,2]
      speed = get_dist(x[,1],x[,2],method = "speed",hz=5)

      airspeed = air_speed(x,wind.direct,wind.speed,speed,return.support.cross = T,plot=plotsupportoverspeed)
      return(c(median(speed,na.rm=T),
               mean  (speed,na.rm=T),
               airspeed[[2]],
               airspeed[[3]]))

      })
    d2 = rbind(dimnames(data.f)[[3]], d2,size)
    d2 = t(apply( d2,2,function(x){
      # x = df[,1]
      names(x)= vars
      return(x)}))
    return(d2)
  } else {
    d2 = matrix(NA,dim(data.f)[[
      3]],length(vars),dimnames = list(NULL,vars))
    d2[,1] = dimnames(data.f)[[3]]
    d2[,"insta.size"] = size
    return(d2)
  }
}
