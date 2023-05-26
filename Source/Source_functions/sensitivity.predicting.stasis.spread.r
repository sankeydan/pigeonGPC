sensitivity.predicting.stasis.spread = function( depvar = NULL, folder = NULL){
  # folder = "fis-dist=50minflight-prop=0.1"
  # depvar = "flock.stasis"


  # libraries
  library( nlme)
  library( speedManip)
  library( MASS)

  # folder
  fold = file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", folder)

  # data
  load(file.path(fold, "dat-group.rda"))
  dg$flock.stasis = -dg$flock.stasis
  dg$masscond = as.factor(dg$masscond)
  dg$lead.fol = as.factor(dg$lead.fol)
  if ( levels(dg$lead.fol)[1] == "fol"){
    whi = c(1,2)
  } else {
    whi = c(2,1)
  }
  dg$masscond = factor( dg$masscond, levels = levels(dg$masscond)[c(3,4,2,1)])
  dg$lead.fol = factor( dg$lead.fol, levels = levels(dg$lead.fol)[whi])

  # vars
  vars = c( "Leadership comp." , "l.all - l.mixed",   "l.all - h.mixed",   "l.all - h.all",     "l.mixed - h.mixed", "l.mixed - h.all",   "h.mixed - h.all", "Mass comp. homogeneity", "Homogeneity" , "Homogeneity (sub followers)")
  stats = c( "trans",  "Value"   ,   "Std.Error",  "DF"      ,   "t-value" ,   "p-value")
  mat = matrix( NA, length(vars),length(stats),dimnames = list(vars,stats))


  ###### stats

  # Does mass comp. predict lrspread
  d = stat.funct.group(dg,"box",depvar = depvar,indvar = "masscond",cond = NULL,experiment = "GMC")
  mat[,"trans"] = d[[2]]$trans
  tab = pvalue(model = d[[1]], use_tukey = T)
  vec = which(dimnames(mat)[[1]] %in% tab$contrast)
  for ( i in vec){
    #i=1
    mat[i,2:6] = as.matrix(tab[(i-1),2:6])
  }
  mat
  # Does leader comp. predict lrspread
  d = stat.funct.group(dg,"box",depvar = depvar,indvar = "lead.fol",cond = NULL,experiment = "leadManip")
  mat["Leadership comp.",stats] =as.numeric(as.vector(as.matrix(d[[2]][2,names(d[[2]])%in%stats])))

  # When only considering homo vs hetero
  d = stat.funct.group(dg,"box",depvar = depvar,indvar = "homo_hetero",cond = NULL,experiment = "GMC")
  mat["Mass comp. homogeneity",stats] = as.numeric(as.vector(as.matrix(d[[2]][2,names(d[[2]])%in%stats])))

  # Does homogeneity predict lrspread
  d = stat.funct.group(dg,"box",depvar = depvar,indvar = "homo_hetero",cond = NULL,experiment = "all")
  mat["Homogeneity",stats] = as.numeric(as.vector(as.matrix(d[[2]][2,names(d[[2]])%in%stats])))

  # when followers removed?
  d = stat.funct.group(dg,"box",depvar = depvar,indvar = "homo_hetero_sub_fol",cond = NULL,experiment = "all")
  mat["Homogeneity (sub followers)",stats] = as.numeric(as.vector(as.matrix(d[[2]][2,names(d[[2]])%in%stats])))

  # if flock stasis, then transpose
  if ( depvar == "flock.stasis"){
    mat[,"Value"]  = -mat[,"Value"]
    mat[,"t-value"]= -mat[,"t-value"]
  }

  ## RETURN
  return(mat)

}
