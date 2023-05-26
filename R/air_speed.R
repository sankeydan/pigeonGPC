
air_speed = function( GPS.data ,
                      wind.direct,
                      wind.speed,
                      grnspeed,
                      plot = F,
                      return.support.cross = F){

  # objects

  # GPS.data = data.f[,1:2,1]
  # wind.direct = wind.direct
  # wind.speed = wind.speed
  # grnspeed = speed
  # plot = T
  # return.support.cross = T

  # objects
  dir  = c("S","SSW","SW","WSW","W","WNW","NW","NNW","N","NNE","NE","ENE","E","ESE","SE","SSE")

  #stopifnot
  if ( ! wind.direct %in% dir){
    stop( "wind dir must be one of the following c('S','SSW','SW','WSW','W','WNW','NW','NNW','N','NNE','NE','ENE', 'E','ESE','SE','SSE')")
  }

  if( ncol(GPS.data) != 2 | dimnames(GPS.data)[[2]][1] != "lon"){
    stop( "GPS data must be a matrix with column names 'lon' and 'lat' ")
  }

  #data
  data = as.data.frame(GPS.data)

  # get_heading
  data$head = get_heading(data[,1], data[,2], indivs = 1)
  dir.circ = data.frame ( circ = seq(-pi,pi-(pi/16),length.out=16),
                          dir)
  wind.dir = dir.circ$circ[which(dir.circ$dir == wind.direct)]


  #Airspeed
  cross     = rep(NA, nrow(data))
  support   = rep(NA, nrow(data))
  air.speed = rep(NA, nrow(data))
  for (i in 2:nrow(data)){
    x = abs(atan2(sin(wind.dir - data$head[i]), cos(wind.dir - data$head[i])))
    if(!is.na(x)){
      if ( abs(x) > pi/2){
        x = abs(x) - (pi/2)
        cross[i] = cos(x) * wind.speed
        support[i]  = -abs(sin(x)) * wind.speed
      } else{
        cross[i] = sin(x) * wind.speed
        support[i]  = abs(cos(x)) * wind.speed
      }
      air.speed[i] = sqrt ( (grnspeed[i] - support[i])^2 + cross[i]^2)
    } else{
      air.speed[i] = NA
    }
  }

  #plot?
  if(plot){
    plot(grnspeed~support)
  }

  support = mean( support, na.rm = T)
  cross   = mean( cross  , na.rm = T)

  # return data
  if( return.support.cross){
  return(list(air.speed,support,cross))
  } else{
    return(air.speed)
  }

  #

}
