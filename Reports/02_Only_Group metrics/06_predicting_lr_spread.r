
#1.  Does condition predict lr spread?

{
  #Houskeeping
  rm(list = ls())

  # libraries
  library( nlme)
  library( speedManip)
  library(leadManip)
  library( MASS)

  # folder
  fold = file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", "fis-dist=50minflight-prop=0.1")

  # data
  load(file.path(fold, "dat-group.rda"))

}

###### stats

# Does leader comp. predict lrspread
d = stat.funct.group(dg,"box",depvar = "lrspread",indvar = "lead.fol",cond = NULL,experiment = "leadManip")
paste ( "t =" , as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
#yes

# Does mass comp. predict lrspread
d = stat.funct.group(dg,"box",depvar = "lrspread",indvar = "masscond",cond = NULL,experiment = "GMC")
paste ( "t =" , as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# no - all conditions
d = stat.funct.group(dg,"box",depvar = "lrspread",indvar = "homo_hetero",cond = NULL,experiment = "GMC")
paste ( "t =" , as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# yes - only considering homo vs hetero

# Does homogeneity predict lrspread
d = stat.funct.group(dg,"box",depvar = "lrspread",indvar = "homo_hetero",cond = NULL,experiment = "all")
paste ( "t =" , as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# no - all
d = stat.funct.group(dg,"box",depvar = "lrspread",indvar = "homo_hetero_sub_fol",cond = NULL,experiment = "all")
paste ( "t =" , as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# no - if we remove annoying followers

# Does flight order predict lrspread?
d = stat.funct.group(dg,"log",depvar = "lrspread",indvar = "flight.order",cond = NULL,experiment = "all")
paste ( "t =" , as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# increasing - not sig. All
d = stat.funct.group(dg,"log",depvar = "lrspread",indvar = "flight.order",cond = NULL,experiment = "leadManip")
paste ( "t =" ,  as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# decreasing - not sig. Leadmanip
d = stat.funct.group(dg,"log",depvar = "lrspread",indvar = "flight.order",cond = NULL,experiment = "GMC")
paste ( "t =" ,  as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# decreasing - not sig. massmanip
d = stat.funct.group(dg,"log",depvar = "lrspread",indvar = "flight.order",cond = NULL,experiment = "Training")
paste ( "t =" ,  as.numeric(as.character(d[[2]]$`t-value`[2])),
      ", p =" ,  as.numeric(as.character(d[[2]]$`p-value`[2])))
# increasing - close to sig. Training

plot(dg$lrspread~dg$flight.order)

  #Houskeeping
  rm(list = ls())

  # libraries
  library( nlme)
  library( speedManip)
  library(leadManip)
  library( MASS)

  # folder
  fold = file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", "fis-dist=50minflight-prop=0.1")

  # data
  load(file.path(fold, "dat-group.rda"))

###### stats

# Does insta.size.num predict lrspread
d = stat.funct.group(dg,"box",depvar = "lrspread",indvar = "insta.size.num",cond = NULL,experiment = "all")
d[[2]]
# yes

# Does insta.size predict lrspread
d = stat.funct.group(dg,"box",depvar = "lrspread",indvar = "insta.size",cond = NULL,experiment = "all")
d[[2]]
# yes

# Does insta.size predict speed
d = stat.funct.group(dg,"box",depvar = "medspeed",indvar = "insta.size.num",cond = NULL,experiment = "all")
d[[2]]
# no

# Does insta.size predict speed
d = stat.funct.group(dg,"box",depvar = "medspeed",indvar = "insta.size",cond = NULL,experiment = "all")
d[[2]]
# some do

boxplot(dg$lrspread~dg$insta.size)
