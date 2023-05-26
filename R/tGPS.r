#' Trim and (clean) GPS
#'
#' @export

tGPS = function (
  data,
  plot_traj = F,
  cut_radius_site = 1000,
  cut_radius_home = 500,
  mins_record_at_home = 0,
  remove_outliers_speed = 100, # more than 100m/s is outlier
  remove_outliers_deg = 2, # more than n degrees (lon/lat) from mean (lon/lat)
  hz =5,
  site.dir.dist = "N8",
  sol.gro = "Sol",
  i = NULL,
  save_plot_traj = F,
  filePath = NULL
){

  #############################

  ## objects

  # dat = data
  # plot_traj = T
  # cut_radius_site = 1000
  # cut_radius_home = cut_radius_home_enter
  # mins_record_at_home = 0
  # remove_outliers_speed = 100
  # remove_outliers_deg = 2
  # hz = 5
  # site.dir.dist = site.dir.dis[i]
  # save_plot_traj = F

  #plot(dist2$site)
  #libraries
  library(plyr)
  library(groupMassComp)


  # load metadata
  load( file.path(PROJHOME , "Data" ,"metadata" ,"release-site-coordinates.rda"))
  site = rsc[rsc$site == site.dir.dist,]
  home = rsc[rsc$site == "home",]

  # cuthome
  if( site$dist == 8 ){
    site4 = rsc[rsc$dir == site$dir & rsc$dist == 4,]
    cut_radius_home = cut_radius_home + site4$dist.exact
  }


  dat = data
  # remove outliers - using speed, and deg
  sig = sign ( median( na.omit( dat$lon)) >0)
  if (sig == 1){ dat$lon = -dat$lon } else {dat$lon }
  out.speed = which(get_dist(dat$lon, dat$lat, hz = 5, method = "speed")>remove_outliers_speed)
  out.lon   = which(abs(dat$lon - median(dat$lon,na.rm=T)) > remove_outliers_deg)
  out.lat   = which(abs(dat$lat  - median(dat$lat ,na.rm=T)) > remove_outliers_deg)
  out = unique(c(out.speed, out.lat, out.lon))
  dat[out,c("lon","lat")] = NA

  # trim start / fin
  dist2 = data.frame ( site = get_dist(dat$lon, dat$lat, site$lon, site$lat, method = "distance" ),
                       home = get_dist(dat$lon, dat$lat, rep(home$lon,nrow(dat)), rep(home$lat,nrow(dat)), method = "distance" ))
  dist2site.rev = rev(dist2$site)
  #plot(dist2site.rev)
  start = nrow(dat) - which(dist2site.rev < cut_radius_site/2)[1]
  dat = dat[start: nrow(dat),]
    dist2 = data.frame ( site = get_dist(dat$lon, dat$lat, rep(site$lon,nrow(dat)), rep(site$lat,nrow(dat)), method = "distance" ),
                       home = get_dist(dat$lon, dat$lat, rep(home$lon,nrow(dat)), rep(home$lat,nrow(dat)), method = "distance" ))
  cut_home = ifelse( na.omit(dist2$site)[1] < cut_radius_site,
                     which(dist2$site>cut_radius_site)[1], 1)
  if ( any ( na.omit(dist2$home[(cut_home+1):nrow(dist2)]) < cut_radius_home)){
    t = which(dist2$home[(cut_home+1):nrow(dist2)] < cut_radius_home )[1]+ (hz*60*mins_record_at_home)
    t = ifelse( t > nrow(dat), nrow(dat),t)
  } else {
    t = nrow(dat)
  }


  # Trim!
  dat2 = dat[(cut_home+1):((cut_home+1) +t),]


  # now plot
  if( plot_traj ){
    if ( save_plot_traj ){
      png ( paste0( filePath , i, ".png" ))
    }
    plot(dat2$lon, dat2$lat, main = paste ( "i = ", i , site$site),
         xlim = range ( c( dat2$lon , site$lon, home$lon), na.rm = T),
         ylim = range ( c( dat2$lat  , site$lat, home$lat), na.rm = T))
    points( site$lon, site$lat , pch= 19, col = "red")
    points( home$lon, home$lat , pch= 19, col = "blue")
    if ( site$dist == 8){
      points( site4$lon , site4$lat , pch = 19,  col = "purple")
    }
    if ( save_plot_traj ){
      dev.off()
    }
  }


  #return
  return( dat2)

}
