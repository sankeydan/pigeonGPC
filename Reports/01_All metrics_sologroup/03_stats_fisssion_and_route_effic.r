
#### CONTENTS

# 0. SETUP

# 1. Group mass
# 1.1. Fission
# 1.2. Route efficiency

# 2. Lead manip
# 2.1. Fission
# 2.2. Route efficiency

# 3. Plots

#########

## SCRIPT

########

# 0. SETUP
{

  # housekeeping
  rm(list = ls())

  ## Libraries
  library( nlme )
  library(emmeans)
  library(plyr)
  library(speedManip)
  library(MASS)

  ## Data
  fold = file.path(PROJHOME , "Output", "01_allmetrics_first_phase")
  load(file.path(fold,"group-masscomp-metrics.rda" ))
  d.grpmas = mat
  d.grpmas$indvar = d.grpmas$var
  rm(mat)
  load(file.path(fold,"lead-manip_metrics.rda"  ))
  d.lead = dat
  d.lead$indvar = d.lead$var
  rm(dat)
  load(file.path(fold,"solo-massload_metrics.rda" ))
  d.solo = metrics
  d.solo$indvar = d.solo$var
  rm(metrics)
  rm(fold)
}
# source
source( file.path(PROJHOME , "Source", "02_statsource.r"))


## Stats

# 1. Group mass

# 1.1. Fission

# fission - GrpMas.
d.grpmas$fission[d.grpmas$fission==0] = NA
b = boxcox(fission  ~ as.factor(prop.heavyatstart) + cross.wind + support.wind, data = d.grpmas ,plotit = F)
bb = bestbox(b, d.grpmas$fission)
d.grpmas$depvar = bb[[1]]
d.grpmas$indvar = as.factor(d.grpmas$prop.heavyatstart)
mod = lme( depvar ~ indvar + support.wind + cross.wind ,na.action = "na.omit", random = list(date=~1, unique.flight=~1, pidge=~1),data=d.grpmas)
summary(mod)
pvalue(model = mod,use_tukey = T)
mat = pvalue(model = mod,use_tukey = T)
# only report the biggest difference. 0-0.67. df = 13,  t =  1.88459237 p = 0.2810875
diagnostics.plot(mod)
d.grpmas$depvar1 = d.grpmas$depvar
boxy.lady( "depvar", "prop.heavyatstart",d.grpmas, jit = 0.3, colz = "grey50")

# as numeric
b = boxcox(fission  ~ prop.heavyatstart + cross.wind + support.wind, data = d.grpmas ,plotit = F)
bb = bestbox(b, d.grpmas$fission)
d.grpmas$depvar = bb[[1]]
d.grpmas$indvar = d.grpmas$prop.heavyatstart
mod = lme( depvar ~ indvar + support.wind + cross.wind ,na.action = "na.omit", random = list(date=~1, unique.flight=~1, pidge=~1),data=d.grpmas)
sum = summary(mod)
sum
# DF = 15, t = -1.625569 , p =  0.1249
for ( i in 1:ncol(mat)){
  mat[,i]= as.character(mat[,i])
}
tab = sum$tTable
mat = rbind(mat , mat[1,])
mat[nrow(mat),] = c("mass comp. as num.", as.character(tab["indvar",]), pvalue(pval = tab["indvar","p-value"]))

# homovs hetero
d.grpmas$indvar = d.grpmas$homo_hetero
b = boxcox(fission  ~ indvar + cross.wind + support.wind, data = d.grpmas ,plotit = F)
bb = bestbox(b, d.grpmas$fission)
d.grpmas$depvar = bb[[1]]
mod = lme( depvar ~ indvar+ support.wind + cross.wind ,na.action = "na.omit", random = list(date=~1, unique.flight=~1, pidge=~1),data=d.grpmas)
sum = summary(mod)
# DF = 14  , t =  0.934090  , p = 0.3661
tab = sum$tTable
mat = rbind(mat , mat[1,])
mat[nrow(mat),] = c("Hom. vs. Het.", as.character(tab["indvarHomo",]), pvalue(pval = tab["indvarHomo","p-value"]))
diagnostics.plot(mod)
d.grpmas$depvar2 = d.grpmas$depvar
boxy.lady( "depvar","homo_hetero",d.grpmas, jit = 0.3,colz = "grey50")
mat = cbind(var = "fission", mat)

# 1.2 Route efficiency

# route effic - Grpmas
b = boxcox(effic  ~ as.factor(prop.heavyatstart) + cross.wind + support.wind, data = d.grpmas ,plotit = F)
bb = bestbox(b, d.grpmas$effic)
d.grpmas$depvar = bb[[1]]
d.grpmas$indvar = as.factor(d.grpmas$prop.heavyatstart)
mod = lme( depvar ~ indvar + support.wind + cross.wind ,na.action = "na.omit", random = list(date=~1, unique.flight=~1, pidge=~1),data=d.grpmas)
summary(mod)
mat2 = pvalue(model = mod,use_tukey = T)
mat2
# only report the biggest difference. 0.33-0.67.df =  13, t =   1.1474067 , p =  0.6682718
diagnostics.plot(mod)
d.grpmas$depvar3 = d.grpmas$depvar
boxy.lady( "depvar", "indvar",d.grpmas, jit = 0.1, colz = "grey50", ylims = c(0,1),mult = 0.6)

d.grpmas$indvar = d.grpmas$homo_hetero
b = boxcox(effic  ~ indvar + cross.wind + support.wind, data = d.grpmas ,plotit = F)
bb = bestbox(b, d.grpmas$effic)
d.grpmas$depvar = bb[[1]]
mod = lme( depvar ~ indvar+ support.wind + cross.wind ,na.action = "na.omit", random = list(date=~1, unique.flight=~1, pidge=~1),data=d.grpmas)
sum = summary(mod)
sum
# DF =  14 , t= 0.588731, p=  0.5654
tab = sum$tTable
mat2 = rbind(mat2 , mat2[1,])
for ( i in 1:ncol(mat2)){
  mat2[,i]= as.character(mat2[,i])
}
mat2[nrow(mat2),] = c("Hom. vs. Het.", as.character(tab["indvarHomo",]), pvalue(pval = tab["indvarHomo","p-value"]))
mat2 = cbind( var =  "route.effic", mat2)
mat = rbind( mat , mat2)
mat = cbind ( exp  = "grpmas" , mat)
diagnostics.plot(mod)
d.grpmas$depvar4 = d.grpmas$depvar
boxy.lady( "depvar","homo_hetero",d.grpmas, jit = 0.1,colz = "grey50",ylims= c(0,1), mult = 0.4)


#1.3 Plots
par(mfrow =c(2,2))
boxy.lady( "depvar1", "prop.heavyatstart",d.grpmas, jit = 0.3, colz = "grey50")
boxy.lady( "depvar2", "homo_hetero"      ,d.grpmas, jit = 0.3, colz = "grey50")
boxy.lady( "depvar3", "indvar"           ,d.grpmas, jit = 0.1, colz = "grey50", ylims = c(0,1),mult = 0.6)
boxy.lady( "depvar4", "homo_hetero"      ,d.grpmas, jit = 0.1, colz = "grey50", ylims= c(0,1) ,mult = 0.4)

# 2. Lead manip

# 2.1. Fission
d.lead$fission[d.lead$fission==0] = NA
b = boxcox(fission  ~ lead.fol + as.factor(group.size)+ cross.wind + support.wind, data = d.lead ,plotit = F)
bb = bestbox(b, d.lead$fission)
d.lead$depvar = bb[[1]]
mod = lme( depvar ~ lead.fol+  as.factor(group.size) + support.wind + cross.wind ,na.action = "na.omit", random = list(date=~1, uniqueflight=~1, pidge=~1),data=d.lead)
sum = summary(mod)
sum
# DF =  61, t=  1.561526 , p = 0.1247
tab = sum$tTable
mat = rbind(mat , mat[1,])
for ( i in 1:ncol(mat)){
  mat[,i]= as.character(mat[,i])
}
mat[nrow(mat),] = c("leadManip", "Fission", "lead. vs. fol.", as.character(tab["lead.follead",]), pvalue(pval = tab["lead.follead","p-value"]))
diagnostics.plot(mod)
d.lead$depvar1 = d.lead$depvar
boxy.lady( "depvar","lead.fol",d.lead, jit = 0.3,colz = "grey50")

# 2.2. Route efficiency
b = boxcox(route.effic  ~ lead.fol + cross.wind + as.factor(group.size) + support.wind, data = d.lead ,plotit = F)
bb = bestbox(b, d.lead$route.effic)
d.lead$depvar = bb[[1]]
mod = lme( depvar ~ lead.fol+ support.wind + as.factor(group.size) +cross.wind  ,na.action = "na.omit", random = list(date=~1, uniqueflight=~1, pidge=~1),data=d.lead)
summary(mod)
# DF =  50, t =   1.199687, p =   0.2359
sum = summary(mod)
tab = sum$tTable
mat = rbind(mat , mat[1,])
for ( i in 1:ncol(mat)){
  mat[,i]= as.character(mat[,i])
}
mat[nrow(mat),] = c("leadManip", "Route.effic", "lead. vs. fol.", as.character(tab["lead.follead",]), pvalue(pval = tab["lead.follead","p-value"]))

diagnostics.plot(mod)
d.lead$depvar2 = d.lead$depvar
boxy.lady( "depvar","lead.fol",d.lead, jit = 0.1,colz = "grey50",ylims= c(0,1), mult = 0.4)

# save mat

write.csv( mat , file = file.path ( PROJHOME , "Output" , "Tables_stats" , "fission_route-effic.csv"))


# 3. Plots

{
par(mfrow = c(2,3))
boxy.lady( "depvar1", "prop.heavyatstart",d.grpmas, jit = 0.3, colz = "grey50")
boxy.lady( "depvar2", "homo_hetero"      ,d.grpmas, jit = 0.3, colz = "grey50")
boxy.lady( "depvar1", "lead.fol"         ,d.lead  , jit = 0.3, colz = "grey50",mult = 0.1)
boxy.lady( "depvar3", "indvar"           ,d.grpmas, jit = 0.1, colz = "grey50", ylims = c(0,1),mult = 0.6)
boxy.lady( "depvar4", "homo_hetero"      ,d.grpmas, jit = 0.1, colz = "grey50", ylims= c(0,1) ,mult = 0.4)
boxy.lady( "depvar2", "lead.fol"         ,d.lead  , jit = 0.3, colz = "grey50", ylims= c(0,1) ,mult = 0.2)
}


