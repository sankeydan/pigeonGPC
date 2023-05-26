
######## CONTENTS #########

###### 0. Set up variables

###### 1. Function to output Group and Indiv. level metrics

## 1.1 Lead manip
## 1.2 GMC
## 1.3 Training (leadership determining) flights

###### 2. Combining all metrics with metadata

## 2.1 Lead manip
## 2.2 GMC
## 2.3 Training
## 2.4 Extra variables - combine and save


###########



############

###### 0. Set up variables

sensitivity = function( fis.dist, minflight.prop){
  # fis.dist = 25
  # minflight.prop = 0.1

  fold.vars = paste0("fis-dist=",fis.dist,"minflight-prop=",minflight.prop)
  li = list( fis.dist, minflight.prop , fold.vars)
  save( li, file = file.path(PROJHOME , "Data" , "metadata" , "temporary-variables.rda"))



  ###### 1. Function to output Group and Indiv. level metrics



  ## 1.1. Lead manip

  {
    # housekeeping
    rm(list=ls())

    #libraries
    library(speedManip)

    #files/folders
    fold = file.path(PROJHOME , "Data", "Leadmanip", "groupedGPS")
    files= list.files(fold)

    # metadata
    load( file.path(PROJHOME ,"Data" , "metadata", "weather-data.rda"))
    load( file.path(PROJHOME ,"Data" , "metadata", "temporary-variables.rda"))
    fis.dist = li[[1]]
    minflight.prop = li[[2]]
    fold.vars = li[[3]]
    filePath = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "leadManip", fold.vars)

    # outfolder
    if(!dir.exists(filePath)){
      dir.create(filePath)
    }

    # extra functions
    sourcefunctions()
  }
  # loop
  for ( i in 1:length(files)){
    #i=6
    load(file.path(fold,files[i]))

    #weather
    {
      flight.time = as.POSIXct( na.omit( data[1 ,3,])[1] ,origin = "1970-01-01")
      weather.row = which.min(abs(as.numeric( flight.time - weather.data$Time.posix)))
      wind.direct = as.character(weather.data$Wind_Direction[weather.row])
      wind.speed  = as.numeric(weather.data$Wind_Speed_m_s[weather.row])
    }

    # function
    data.list = metricsFunct(data,wind.speed = wind.speed,wind.direct = wind.direct, fis.dist = fis.dist, minflight.prop = minflight.prop)

    # save
    save( data.list,file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "leadManip", fold.vars, files[i]))

    # takestock
    print(paste(i,"/",length(files)))
  }

  ## 1.2 GMC


  # housekeeping
  rm(list=ls())

  #libraries
  library(speedManip)

  #files/folders
  fold = file.path(PROJHOME , "Data", "GMC", "groupedGPS")
  files= list.files(fold)

  # metadata
  load( file.path(PROJHOME ,"Data" , "metadata", "weather-data.rda"))
  load( file.path(PROJHOME ,"Data" , "metadata", "temporary-variables.rda"))
  fis.dist = li[[1]]
  minflight.prop = li[[2]]
  fold.vars = li[[3]]
  filePath = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "GMC", fold.vars)

  # outfolder
  if(!dir.exists(filePath)){
    dir.create(filePath)
  }

  # extra functions
  sourcefunctions()

  # loop
  for ( i in 1:length(files)){
    #i=1
    load(file.path(fold,files[i]))

    #weather
    {
      flight.time = as.POSIXct( na.omit( data[1 ,3,])[1] ,origin = "1970-01-01")
      weather.row = which.min(abs(as.numeric( flight.time - weather.data$Time.posix)))
      wind.direct = as.character(weather.data$Wind_Direction[weather.row])
      wind.speed  = as.numeric(weather.data$Wind_Speed_m_s[weather.row])
    }

    # function
    data.list = metricsFunct(data,wind.speed = wind.speed,wind.direct = wind.direct, fis.dist = fis.dist, minflight.prop = minflight.prop)

    # save
    save( data.list,file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "GMC", fold.vars, files[i]))

    # takestock
    print(paste(i,"/",length(files)))
  }


  ## 1.3 Training (leadership determining) flights


  { # skip to loop

    # housekeeping
    rm(list=ls())

    #libraries
    library(speedManip)

    #files/folders
    fold = file.path(PROJHOME , "Data", "Training", "groupedGPS")
    files= list.files(fold)

    # metadata
    load( file.path(PROJHOME ,"Data" , "metadata", "weather-data.rda"))
    load( file.path(PROJHOME ,"Data" , "metadata", "temporary-variables.rda"))
    fis.dist = li[[1]]
    minflight.prop = li[[2]]
    fold.vars = li[[3]]
    filePath = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "Training", fold.vars)

    # outfolder
    if(!dir.exists(filePath)){
      dir.create(filePath)
    }

    # extra functions
    sourcefunctions()
  }
  # loop
  for ( i in 1:length(files)){
    #i=6

    #data
    load(file.path(fold,files[i]))

    #weather
    {
      flight.time = as.POSIXct( na.omit( data[1 ,3,])[1] ,origin = "1970-01-01")
      weather.row = which.min(abs(as.numeric( flight.time - weather.data$Time.posix)))
      wind.direct = as.character(weather.data$Wind_Direction[weather.row])
      wind.speed  = as.numeric(weather.data$Wind_Speed_m_s[weather.row])
    }

    # function
    data.list = metricsFunct(data,wind.speed = wind.speed,wind.direct = wind.direct, fis.dist = fis.dist, minflight.prop = minflight.prop)

    # save
    save( data.list,file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "Training", fold.vars, files[i]))

    # takestock
    print(paste(i,"/",length(files)))
  }
  ## 2.1 Lead manip

  { # skip to loop

    # Housekeeping
    rm(list=ls())

    # libraries
    library(speedManip)
    sourcefunctions()

    # metadata
    load( file.path(PROJHOME ,"Data" , "metadata", "temporary-variables.rda"))
    fold.vars = li[[3]]

    # files/folders
    fol = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics")
    fold = file.path(fol , "leadManip", fold.vars)
    fol = file.path(fol, "training", fold.vars)
    fold.raw = file.path(PROJHOME, "Data", "leadManip", "groupedGPS")
    files = list.files(fold)

    # flight.order / unique flight
    uf = length(list.files(file.path(fol)))
    fo = uf/3

    # split
    spl = stringr::str_split_fixed(files,"\\.",12)

    # comb matrices
    vars.ind =  c("pidge","medspeed","meanspeed","support.wind","cross.wind","insta.size", "fbmed","fbsd","lrmed","lrsd","tort","medneidist","mednndist","allneisd","date","flight.order","unique.flight","lead.fol","masscond","group","experiment")
    vars.gro = c("fbspread","lrspread","insta.size","flock.spread","flock.stasis","medspeed","meanspeed","support.wind","cross.wind","tort","date","flight.order","unique.flight","lead.fol","masscond","group","experiment")
    dat.inds = as.data.frame(matrix(NA,0,length(vars.ind),dimnames=list(NULL, vars.ind)))
    dat.gros = as.data.frame(matrix(NA,0,length(vars.gro),dimnames=list(NULL, vars.gro)))
  }
  # loop
  for ( i in 1:length(files)){
    #i=1

    # data
    load( file.path(fold,files[i]))
    dat.ind = data.list[[1]]
    dat.gro = data.list[[2]]

    # date
    load( file.path(fold.raw,files[i]))
    flight.time = as.POSIXct( na.omit( data[1 ,3,])[1] ,origin = "1970-01-01")
    date = substr(flight.time,1,10)
    dat.ind$date = date
    dat.gro$date = date

    # flight order
    dat.ind$flight.order = as.factor(fo + as.numeric(spl[i,5]))
    dat.gro$flight.order = as.factor(fo + as.numeric(spl[i,5]))

    # unique flight
    dat.ind$unique.flight = as.factor(uf + i)
    dat.gro$unique.flight = as.factor(uf + i)

    # lead.fol
    dat.ind$lead.fol = spl[i,6]
    dat.gro$lead.fol = spl[i,6]

    # mass cond
    dat.ind$masscond = NA
    dat.gro$masscond = NA

    # group
    dat.ind$group = spl[i,3]
    dat.gro$group = spl[i,3]

    # study
    dat.ind$experiment = "leadManip"
    dat.gro$experiment = "leadManip"

    # combine
    dat.inds = rbind(dat.inds,dat.ind)
    dat.gros = rbind(dat.gros,dat.gro)
  }

  # save
  save( dat.inds , file = file.path(PROJHOME, "Output", "foos", "leadManipcombind.rda"))
  save( dat.gros , file = file.path(PROJHOME, "Output", "foos", "leadManipcombgro.rda"))


  ## 2.2 GMC

  {
    # Housekeeping
    rm(list=ls())

    # libraries
    library(speedManip)
    sourcefunctions()

    # metadata
    load( file.path(PROJHOME ,"Data" , "metadata", "temporary-variables.rda"))
    fold.vars = li[[3]]

    # files/folders
    fol = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics")
    fold = file.path(fol , "GMC",fold.vars)
    fold.raw = file.path(PROJHOME, "Data", "GMC", "groupedGPS")
    files = list.files(fold)

    # flight.order / unique flight
    uf.t = length(list.files(file.path(fol,"training",fold.vars)))
    uf.l = length(list.files(file.path(fol,"leadManip",fold.vars)))
    uf = uf.t + uf.l
    fo = (uf.t/3) + ceiling(uf.l/6)
    load( file.path(PROJHOME ,"Data","metadata", "smallvec_mat_donotmove","order_GMCflights.rda"))
    fo.add = vec

    # split
    spl = stringr::str_split_fixed(files,"\\.",12)
    masscond = apply(spl[,1:2],1,function(x){paste0(x,collapse = ".")})

    # comb matrices
    vars.ind =  c("pidge","medspeed","meanspeed","support.wind","cross.wind","insta.size", "fbmed","fbsd","lrmed","lrsd","tort","medneidist","mednndist","allneisd","date","flight.order","unique.flight","lead.fol","masscond","group","experiment")
    vars.gro = c("fbspread","lrspread","insta.size","flock.spread","flock.stasis","medspeed","meanspeed","support.wind","cross.wind","tort","date","flight.order","unique.flight","lead.fol","masscond","group","experiment")
    dat.inds = as.data.frame(matrix(NA,0,length(vars.ind),dimnames=list(NULL, vars.ind)))
    dat.gros = as.data.frame(matrix(NA,0,length(vars.gro),dimnames=list(NULL, vars.gro)))
  }
  # loop
  for ( i in 1:length(files)){
    #i=1

    # data
    load( file.path(fold,files[i]))
    dat.ind = data.list[[1]]
    dat.gro = data.list[[2]]

    # date
    load( file.path(fold.raw,files[i]))
    flight.time = as.POSIXct( na.omit( data[1 ,3,])[1] ,origin = "1970-01-01")
    date = substr(flight.time,1,10)
    dat.ind$date = date
    dat.gro$date = date

    # flight order
    dat.ind$flight.order = as.factor(fo + ceiling(fo.add[i]/2) )
    dat.gro$flight.order = as.factor(fo + ceiling(fo.add[i]/2) )

    # unique flight
    dat.ind$unique.flight = as.factor(uf + fo.add[i])
    dat.gro$unique.flight = as.factor(uf + fo.add[i])

    # lead.fol
    dat.ind$lead.fol = NA
    dat.gro$lead.fol = NA

    # mass cond
    dat.ind$masscond = masscond[i]
    dat.gro$masscond = masscond[i]

    # group
    dat.ind$group = NA
    dat.gro$group = NA

    # study
    dat.ind$experiment = "GMC"
    dat.gro$experiment = "GMC"

    # combine
    dat.inds = rbind(dat.inds,dat.ind)
    dat.gros = rbind(dat.gros,dat.gro)
  }

  # save
  save( dat.inds , file = file.path(PROJHOME, "Output", "foos", "GMCcombind.rda"))
  save( dat.gros , file = file.path(PROJHOME, "Output", "foos", "GMCcombgro.rda"))


  ## 2.3 Training

  {
    # Housekeeping
    rm(list=ls())

    # libraries
    library(speedManip)
    sourcefunctions()

    # metadata
    load( file.path(PROJHOME ,"Data" , "metadata", "temporary-variables.rda"))
    fold.vars = li[[3]]

    # files/folders
    fold = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "Training", fold.vars)
    fold.raw = file.path(PROJHOME, "Data", "Training", "groupedGPS")
    files = list.files(fold)

    # flight.order / unique flight
    uf = 0
    fo = 0

    # split
    spl = stringr::str_split_fixed(files,"\\.",12)

    # comb matrices
    vars.ind =  c("pidge","medspeed","meanspeed","support.wind","cross.wind","insta.size", "fbmed","fbsd","lrmed","lrsd","tort","medneidist","mednndist","allneisd","date","flight.order","unique.flight","lead.fol","masscond","group","experiment")
    vars.gro = c("fbspread","lrspread","insta.size","flock.spread","flock.stasis","medspeed","meanspeed","support.wind","cross.wind","tort","date","flight.order","unique.flight","lead.fol","masscond","group","experiment")
    dat.inds = as.data.frame(matrix(NA,0,length(vars.ind),dimnames=list(NULL, vars.ind)))
    dat.gros = as.data.frame(matrix(NA,0,length(vars.gro),dimnames=list(NULL, vars.gro)))
  }
  # loop
  for ( i in 1:length(files)){
    #i=1

    # data
    load( file.path(fold,files[i]))
    dat.ind = data.list[[1]]
    dat.gro = data.list[[2]]

    # date
    load( file.path(fold.raw,files[i]))
    flight.time = as.POSIXct( na.omit( data[1 ,3,])[1] ,origin = "1970-01-01")
    date = substr(flight.time,1,10)
    dat.ind$date = date
    dat.gro$date = date

    # flight order
    dat.ind$flight.order = as.factor(fo + as.numeric(spl[i,4]))
    dat.gro$flight.order = as.factor(fo + as.numeric(spl[i,4]))

    # unique flight
    dat.ind$unique.flight = as.factor(uf + i)
    dat.gro$unique.flight = as.factor(uf + i)

    # lead.fol
    dat.ind$lead.fol = NA
    dat.gro$lead.fol = NA

    # mass cond
    dat.ind$masscond = NA
    dat.gro$masscond = NA

    # group
    dat.ind$group = spl[i,2]
    dat.gro$group = spl[i,2]

    # study
    dat.ind$experiment = "Training"
    dat.gro$experiment = "Training"

    # combine
    dat.inds = rbind(dat.inds,dat.ind)
    dat.gros = rbind(dat.gros,dat.gro)
  }

  # save
  save( dat.inds , file = file.path(PROJHOME, "Output", "foos", "Trainingcombind.rda"))
  save( dat.gros , file = file.path(PROJHOME, "Output", "foos", "Trainingcombgro.rda"))

  # Housekeeping
  rm(list=ls())

  # metadata
  load( file.path(PROJHOME ,"Data" , "metadata", "temporary-variables.rda"))
  fold.vars = li[[3]]
  filePath = file.path(PROJHOME , "output", "Groupflight_ind_grp_metrics", "dat4stats",fold.vars)

  # outfolder
  if(!dir.exists(filePath)){
    dir.create(filePath)
  }



  # folder
  fold = file.path(PROJHOME, "Output", "foos")

  # data - combine
  load( file.path(fold,"leadManipcombgro.rda" ))
  g1 = dat.gros
  load( file.path(fold,"GMCcombgro.rda" ))
  g2 = dat.gros
  load( file.path(fold,"Trainingcombgro.rda"  ))
  g3 = dat.gros
  dg = rbind(g1,g2,g3)
  load( file.path(fold, "leadManipcombind.rda"))
  i1 = dat.inds
  load( file.path(fold, "GMCcombind.rda" ))
  i2 = dat.inds
  load( file.path(fold, "Trainingcombind.rda" ))
  i3 = dat.inds
  di = rbind(i1,i2,i3)

  # extra variables
  di$flock.stasis = di$allneisd
  di$flock.spread = di$medneidist
  di$fbspread = di$fbsd
  di$lrspread = di$lrsd
  dg$mass.mixed = stringr::str_split_fixed(dg$masscond,"\\.",2)[,2]
  dg$homogenous_condition = apply(cbind(dg$lead.fol, dg$mass.mixed,dg$experiment),1,function(x){paste0(na.omit(x),collapse = ".")})
  dg$homo_hetero = NA
  dg$homo_hetero[dg$mass.mixed == "all"] = "homo"
  dg$homo_hetero[dg$mass.mixed == "mixed"]="hetero"
  dg$homo_hetero[dg$experiment == "leadManip"] = "homo"
  dg$homo_hetero[dg$lead.fol == "lead"] = "homo"
  dg$homo_hetero[dg$lead.fol == "fol"] = "homo"
  dg$homo_hetero[dg$experiment == "Training"] = "hetero"
  dg$homo_hetero_sub_fol = dg$homo_hetero
  dg$homo_hetero_sub_fol[dg$lead.fol == "fol"] = NA
  dg$insta.size.num = as.numeric(as.character(dg$insta.size))
  di$mass.mixed = stringr::str_split_fixed(di$masscond,"\\.",2)[,2]
  di$homogenous_condition = apply(cbind(di$lead.fol, di$mass.mixed,di$experiment),1,function(x){paste0(na.omit(x),collapse = ".")})
  di$homo_hetero = NA
  di$homo_hetero[di$mass.mixed == "all"] = "homo"
  di$homo_hetero[di$mass.mixed == "mixed"]="hetero"
  di$homo_hetero[di$experiment == "leadManip"] = "homo"
  di$homo_hetero[di$lead.fol == "lead"] = "homo"
  di$homo_hetero[di$lead.fol == "fol"] = "homo"
  di$homo_hetero[di$experiment == "Training"] = "hetero"
  di$homo_hetero_sub_fol = di$homo_hetero
  di$homo_hetero_sub_fol[di$lead.fol == "fol"] = NA
  di$insta.size.num = as.numeric(as.character(di$insta.size))
  dg$flock.stasis = -dg$flock.stasis
  di$flock.stasis = -di$flock.stasis
  dg$flight.order = as.numeric(as.character(dg$flight.order))
  di$flight.order = as.numeric(as.character(di$flight.order))
dg$fbspread[dg$fbspread==0] = NA
  # save
  save( dg , file= file.path(PROJHOME , "output", "Groupflight_ind_grp_metrics", "dat4stats",fold.vars, "dat-group.rda"))
  save( di , file= file.path(PROJHOME , "output", "Groupflight_ind_grp_metrics", "dat4stats",fold.vars, "dat-ind.rda"))

}
