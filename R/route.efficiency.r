route.efficiency = function (data , rsc, spl, cut_radius_sum , hz = 5){

row = rsc$site == paste0(spl[i,7:6],collapse = "")
site = rsc[row,]
rsc2 = rsc[-which(row == T),]
if ( spl[i,6] == "8" ){
  vec  = rsc2$dir == spl[i,7]
  home = rsc2[which(vec == T),]
} else {
  home = rsc[rsc$site == "home",]
}
bee = get_dist(home$lon , home$lat, site$lon , site$lat , method = "distance")
bee = bee - 1500

# speed
speed = get_dist( data$lon, data$lat , method= "speed" , hz= 5)
speed[speed < 5] = NA
speed[speed > 35] = NA

total.dist = sum(speed,na.rm = T)/hz
total.time = length( which (!is.na(speed)))/hz
route.eff = bee/total.dist

return( list( route.eff, total.dist , total.time))
}
