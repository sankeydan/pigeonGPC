metricsFunct = function(data,
                        cut_radius = 1000,
                        fis.dist = 50,
                        hz = 5,
                        minflight.prop = 0.1,
                        beeline = 9098.532,
                        wind.speed = NULL,
                        wind.direct = NULL,
                        indvars = c( "pidge"      ,   "medspeed"   ,   "meanspeed" ,    "support.wind" , "cross.wind"
                                     , "insta.size"  , "fbmed"   ,      "fbsd"  ,       "lrmed"   ,      "lrsd"  ,
                                     "tort"    ,      "medneidist",  "mednndist" ,    "allneisd" ,
                                     "unique.flight", "group"     ,    "flight"     ,   "lead.fol"    ,  "date")

) {

  # VARIABLES

  {
    # cut_radius = 1000
    # fis.dist = fis.dist
    # hz = 5
    # minflight.prop = minflight.prop
    # beeline = 9098.532
    # indvars = c( "pidge","medspeed","meanspeed" ,"support.wind" ,"cross.wind","insta.size","fbmed","fbsd", "lrmed","lrsd","tort","medneidist","mednndist","allneisd" ,"unique.flight", "group","flight","lead.fol","date")
  }

  #### SCRIPT ####

  # extra functions
  sourcefunctions()

  # objects
  {
    beeline = beeline - (cut_radius*2)
    len = dim(data)[[3]]
    name = dimnames(data)[[3]]
  }

  # fission
  {
    source(file.path(PROJHOME, "R", "fission.r"))
    data.fis = fission(data,fis_dist = fis.dist, plot=F)
    data.f    = data.fis[[1]]
    omit.mat  = data.fis[[2]]
    centroid  = data.fis[[3]]
    dist2cent = data.fis[[4]]
  }

  # near neighbours
  {
    data.nn = near_nei(data)
    neidists  = data.nn[[1]]
    nn        = data.nn[[2]]
  }

  #### INDIVIDUAL METRICS

  # speed at each flock size
  {
    insta.flock.size = apply(dist2cent,1,function(x){length(which(!is.na(x)))})
    unq = unique(insta.flock.size)
    unq = unq[unq>2]
    df = speed_at_insta( insta.flock.size,unq[1],data.f,minflight.prop,wind.direct,wind.speed,plotsupportoverspeed = F)
    if ( length(unq)>1){
      for ( j in 2:length(unq)){
        #j=2
        df2 =  speed_at_insta( insta.flock.size,unq[j],data.f,minflight.prop,wind.direct,wind.speed,plotsupportoverspeed = F)
        df = rbind(df,df2)
      }
    }
    dimnames(df) = list(NULL,dimnames(df)[[2]])
    df=as.data.frame(df)
    for ( j in 2:ncol(df)){
      df[,j] = as.numeric(as.character(df[,j]))
    }
    df.speed = df
  }

  # fb/lr at each flock size - SAVE GROUP METRICS TOO
  {
    fblr = fb_lr_funct( data.f, insta.flock.size,unq[1],minflight.prop)
    df = fblr[[1]]
    df2= fblr[[2]]
    if ( length(unq)>1){
      for ( j in 2:length(unq)){
        #j=2
        fblr = fb_lr_funct(data.f ,insta.flock.size,unq[j],minflight.prop)
        df = rbind(df,fblr[[1]])
        df2= rbind(df2,fblr[[2]])
      }
    }
    if( is.null(dim(df2))){
      if (length(unq) == 0){
        df2= c(df2,insta.size = NA)
      } else{
        df2= c(df2,insta.size = unq)
      }
    } else{
      df2 = cbind(df2,insta.size = unq)
    }
    df$fbmed = as.numeric(as.character(df$fbmed))
    df$lrmed = as.numeric(as.character(df$lrmed))
    df$fbsd = as.numeric(as.character(df$fbsd))
    df$lrsd = as.numeric(as.character(df$lrsd))
    df.fblr = df
  }

  # Other individual metrics
  {
    vars2 = c("tort", "medneidist","mednndist","allneisd")
    df3s = as.data.frame(matrix(NA,0,length(vars2),dimnames = list(NULL,vars2)))
    for ( k in unq){
      #k=unq[1]
      df3 = as.data.frame(matrix( NA, length(name) ,length(vars2),dimnames = list(NULL,vars2)))
      whi = which(insta.flock.size == k)

      if( length(whi)/ nrow(nn) > minflight.prop){


        om = omit.mat[whi,]

        # loop by individual
        for ( j in 1:len){
          #j=3
          #Distance to all neighbours in group
          nd = neidists[whi,,j]

          for( l in 1:ncol(nd)){
            #l=1
            nd[,l][nd[,l]>fis.dist] = NA
          }
          if( length(which(om[,j] == F))/ nrow(nn) > minflight.prop){

            nn2 = nn[whi,]
            nn.take = nn2[!om[,j],j]
            nn.take = nn.take[nn.take<fis.dist]
            head = get_heading(data[whi,"lon",j],data[whi,"lat",j],indivs = 1)
            meandistall = rowMeans(nd,na.rm = T)

            # tortuosity
            y = abs( diff(head))
            y = ifelse ( y > pi, abs(y- (2*pi)),y )

            # # # work
            if (  median(meandistall,na.rm = T) > 60){
              stop()
            }
            # # #
            # return
            df3$tort      [j]   = ifelse( length(whi)/nrow(nn) > minflight.prop, median(y         ,na.rm = T),NA)
            df3$medneidist[j]   = ifelse( length(whi)/nrow(nn) > minflight.prop, median(meandistall,na.rm = T),NA)
            df3$mednndist [j]   = ifelse( length(whi)/nrow(nn) > minflight.prop, median(nn.take   ,na.rm = T),NA)
            df3$allneisd  [j]   = ifelse( length(whi)/nrow(nn) > minflight.prop, sd    (meandistall,na.rm = T),NA)
          }
        }
      }
      # combine
      df3s = rbind( df3s,df3)
    }
    if(nrow(df3s) == 0){
      df3s = rbind(df3s,matrix(NA,len,ncol(df3s)))
      names(df3s) = vars2
    }
    df.indmet = df3s
  }

  # combine all individual metrics
  df = cbind( df.speed,
              fbmed = df.fblr$fbmed,
              fbsd  = df.fblr$fbsd,
              lrmed = df.fblr$lrmed,
              lrsd  = df.fblr$lrsd,
              df.indmet)


  #### GROUP LEVEL MEASURES

  # centroid speed
  {
    vars.speed = c("medspeed" , "meanspeed" , "support.wind", "cross.wind", "tort" )
    mat = matrix(NA,0,length(vars.speed),dimnames = list(NULL,vars.speed))
    speed = get_dist(centroid$lon, centroid$lat,method="speed",hz=hz)
    head = get_heading(centroid$lon,centroid$lat,indivs = 1)
    if (length(unq)!=0){

      for ( j in 1:length(unq)){
        #j=3
        whi = which(insta.flock.size == unq[j])
        if( length(whi)/ nrow(nn) > minflight.prop){
          # tortuosity
          y = abs( diff(head[whi]))
          y = ifelse ( y > pi, abs(y- (2*pi)),y )
          # cross/support
          airspeed = air_speed(centroid[whi,],wind.direct,wind.speed,speed[whi],return.support.cross = T)
          #combine
          mat = rbind(mat,
                      c(median(speed[whi],na.rm = T),
                        mean  (speed[whi],na.rm = T),
                        airspeed[[2]],
                        airspeed[[3]],
                        median(y,na.rm = T)))
        } else{
          mat = rbind(mat,rep(NA,length(vars.speed)))
        }
      }
    } else {
      mat = rbind(mat,rep(NA,length(vars.speed)))
    }
    cent.speed.met = mat
  }



  # flock spread, and flock stasis
  {
    nd2 = neidists
    for( j in 1:len){
      #j=1
      for( k in 1:len){
        nd2[,j,k][neidists[,j,k]>fis.dist] =NA
      }
    }
    flock.spread =vector()
    flock.stasis  =vector()
    if( length(unq) != 0){
      for ( j in 1:length(unq)){
        #j=1
        whi = which(insta.flock.size == unq[j])
        if( length(whi)/ nrow(nn) > minflight.prop){
          dists = apply(nd2[whi,,],1,function(x){
            mean(as.vector(x),na.rm=T)})
          flock.spread = c(flock.spread,median(dists,na.rm = T))
          flock.stasis = c(flock.stasis,sd    (dists,na.rm = T))
        } else{
          flock.spread = c(flock.spread,NA)
          flock.stasis = c(flock.stasis,NA)
        }
      }
      flock.met = cbind(flock.spread,flock.stasis)
    } else {
      flock.met = c(flock.spread=NA,flock.stasis=NA)
    }
  }

  # front/back distances
  {
    if (is.null(dim(df2))){
      names(df2) = c("fbspread","lrspread","insta.size")
    } else {
      dimnames(df2) = list(NULL,c("fbspread","lrspread","insta.size"))
    }
    fb.met = df2
  }

  # combine
  {
    if ( is.null(dim(df2)) & length(unq) ==0){
      mat = t(as.matrix(c(fb.met, flock.met, cent.speed.met)))
      nam = c(names(fb.met),
              names(flock.met),
              dimnames(cent.speed.met)[[2]])
      dimnames(mat) = list(NULL,nam)
    }
    if ( is.null(dim(df2)) & length(unq) ==1){
      mat = t(as.matrix(c(fb.met, flock.met, cent.speed.met)))
      nam = c(names(fb.met),
              dimnames(flock.met)[[2]],
              dimnames(cent.speed.met)[[2]])
      dimnames(mat) = list(NULL,nam)
    }
    if( !is.null(dim(df2)) & length(unq) >1){
      mat = cbind( fb.met, flock.met, cent.speed.met)
    }
    mat = as.data.frame(mat)
    mat$insta.size = as.factor(mat$insta.size)
  }

  # RETURN ALL METRICS- solo group
  return( list(df,mat))
}
