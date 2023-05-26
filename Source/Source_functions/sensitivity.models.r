sensitivity.models = function( fis.dist, minflight.prop){
  # fis.dist = 50
  # minflight.prop = 0.25
  fold.vars = paste0("fis-dist=",fis.dist,"minflight-prop=",minflight.prop)
  li = list( fis.dist, minflight.prop , fold.vars)
  save( li, file = file.path(PROJHOME , "Data" , "metadata" , "temporary-variables.rda"))

  filePath = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "sensitivity", fold.vars)
if(!dir.exists(filePath)){
  dir.create(filePath)
}

  rm(list=ls())

  # folder
  load( file.path(PROJHOME , "Data" , "metadata", "temporary-variables.rda"))
  fold.vars = li[[3]]
  fold = file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", fold.vars)

  # data
  load(file.path(fold, "dat-group.rda"))
  dgro = dg
  load(file.path(fold, "dat-ind.rda"))
  dind = di

  # variables
  vars = c("fbspread","lrspread","flock.spread","flock.stasis")

  # empty mat
  mat.gro = matrix(NA, length(vars), length(vars),dimnames = list(vars,vars))
  mat.ind = matrix(NA, length(vars), length(vars),dimnames = list(vars,vars))

  # correlations
  for ( i in 1:length(vars)){
    for ( j in 1:length(vars)){
      # i =2
      # j =1
      if ( i > j){
        whi = which(names(dgro) %in% vars[c(i,j)])
        d = dgro[,whi]
        d = d[complete.cases(d),]
        mat.gro[i,j] = cor(d[,1],d[,2])
      }
    }
  }

  for ( i in 1:length(vars)){
    for ( j in 1:length(vars)){
      # i =2
      # j =1
      if ( i > j){
        whi = which(names(dind) %in% vars[c(i,j)])
        d = dind[,whi]
        d = d[complete.cases(d),]
        mat.ind[i,j] = cor(d[,1],d[,2])
      }
    }
  }

  mat.gro
  mat.ind

  dimnames(mat.gro) = list( c("Cranio-caudal spread", "Dorso-ventral spread", "Flock spread", "Flock stasis"),
                            c("Cranio-caudal spread", "Dorso-ventral spread", "Flock spread", "Flock stasis"))
  for ( i in 1:ncol(mat.gro)){
    mat.gro[,i] = round(mat.gro[,i],6)
  }
  write.csv( mat.gro , file = file.path (PROJHOME , "Output", "Groupflight_ind_grp_metrics", "sensitivity", fold.vars, "correlation-matrix.csv"))
  save     ( mat.gro , file = file.path (PROJHOME , "Output", "Groupflight_ind_grp_metrics", "sensitivity", fold.vars, "correlation-matrix.rda"))

  {
    #Houskeeping
    rm(list = ls())

    # libraries
    library( nlme)
    library( speedManip)
    library( MASS)

    # folder
    load( file.path(PROJHOME , "Data" , "metadata", "temporary-variables.rda"))
    fold.vars = li[[3]]
    fold = file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", fold.vars)

    # data
    load(file.path(fold, "dat-group.rda"))
dg$flock.stasis = -dg$flock.stasis
    # stats
    exps = c("leadManip", "GMC", "Training", "all")
    indvars = c("fbspread", "lrspread", "flock.stasis", "flock.spread")
    mat = matrix(NA, length(indvars),length(exps),dimnames = list(indvars,exps))
    vars = expand.grid(indvars,exps, stringsAsFactors = F)
    names(vars) = c("indvars", "exps")
  }
  #loop
  for ( i in 1:nrow(vars)){
    #i=9
    indvar1 = vars$indvars[i]
    exp1 = vars$exps[i]
    d = stat.funct.group(dg,"log",depvar = "medspeed",indvar = indvar1,cond = NULL,experiment = exp1, rou = 6)
    #diagnostics.plot(d[[1]])
    d[[2]]
    mat[indvar1 ,exp1] = as.numeric(as.character(d[[2]]$'p-value'[2]))
  }
  write.csv(mat, file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "Stats_tables", "Flock_variables_vs_speed_all_experiments-GROUP.csv"))

  dimnames(mat) = list(c("Cranio-caudal spread", "Dorso-ventral spread", "Flock stasis", "Flock spread"),
                       c("Leadership composition" , "Mass composition", "Training", "All"))
  write.csv(mat, file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "sensitivity", fold.vars, "Flock_variables_vs_speed_all_experiments-GROUP.csv"))
  save(mat, file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "sensitivity", fold.vars, "Flock_variables_vs_speed_all_experiments-GROUP.rda"))
  mat
  mat.1var = mat
  d[[2]]

  #Houskeeping


  # libraries
  library( nlme)
  library( speedManip)
  library( MASS)

  # folder
  load( file.path(PROJHOME , "Data" , "metadata", "temporary-variables.rda"))
  fold.vars = li[[3]]
  fold = file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", fold.vars)

  # data
  load(file.path(fold, "dat-group.rda"))


  # objects
  exps = c("leadManip","GMC", "Training", "all")
  if ( length (which(dg$medspeed == 0) ) >0){
  dg = dg[    -which(dg$medspeed == 0),]
  }

  mat.1var[,4]

  # STATS FUNCTION
  fun = function( exp, boxlog = NULL, indvar1 = "flock.stasis", indvar2 = "lrspread"){
    # boxlog = "log"
    # exp = "all"
    # indvar1 = indvar1
    # indvar2 = indvar2

    d = stat.funct.group(dg,boxlog,depvar = "medspeed", indvar = "flock.stasis", experiment = exp, rou = 6)
    mod.1 = d[[1]]
    tab.1 = d[[2]]

    d = stat.funct.group.2var.int(dg,boxlog,depvar = "medspeed",indvar1 = indvar1, indvar2 = indvar2 ,experiment = exp)
    mod.int = d[[1]]
    tab.int = d[[2]]

    d = stat.funct.group.2var.sum(dg,boxlog,depvar = "medspeed",indvar1 = indvar1, indvar2 = indvar2 ,experiment = exp)
    tab.sum = d[[2]]
    mod.sum = d[[1]]

    whi = which.min( suppressWarnings( AIC(mod.1 , mod.int, mod.sum)$AIC))

    if ( whi == 1){
      p = c(as.numeric(as.character(tab.1$`p-value`[2])) ,NA, "1var")
      return(list(mod.1, tab.1, p))
    }
    if ( whi == 2){
      p = c(as.numeric(as.character(tab.int$`p-value`[2:3])), "int")
      return(list(mod.int, tab.int, p))
    }
    if ( whi == 3) {
      p = c(as.numeric(as.character(tab.sum$`p-value`[2:3])) , "sum")
      return(list(mod.sum, tab.sum, p))
    }
  }


vars = c( "fbspread", "lrspread" , "flock.stasis", "flock.spread")
vars.names =   dimnames(mat.1var)[[1]]

indvar1    = vars      [ order( mat.1var[,"All"])[1]]
var.name1  = vars.names[ order( mat.1var[,"All"])[1]]

indvar2    = vars      [ order( mat.1var[,"All"])[2]]
var.name2  = vars.names[ order( mat.1var[,"All"])[2]]

# Run function
  d1 = fun("leadManip","log",indvar1 = indvar1 , indvar2 = indvar2)
  d2 = fun("GMC"      ,"log",indvar1 = indvar1 , indvar2 = indvar2)
  d3 = fun("Training" ,"log",indvar1 = indvar1 , indvar2 = indvar2)
  d4 = fun("all"      ,"log",indvar1 = indvar1 , indvar2 = indvar2)


  # Combine results
  mat = cbind(
    d1[[3]],
    d2[[3]],
    d3[[3]],
    d4[[3]])
  dimnames(mat) = list( c(var.name1, var.name2, "Best model"),
                        c("Lead comp." , "Mass comp.", "Training", "All"))
  mat
  # save results
  write.csv(mat, file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "sensitivity", fold.vars, "bestmodel.csv"))
  save     (mat, file = file.path(PROJHOME, "Output", "Groupflight_ind_grp_metrics", "sensitivity", fold.vars, "bestmodel.rda"))
}
