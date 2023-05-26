{

# housekeeping
rm(list = ls())

## Libraries
library( nlme )
library(emmeans)
library(plyr)
functions.fold = file.path ( PROJHOME , "R")
functions = list.files( functions.fold)
for ( i in 1:length(functions)){
  source ( file.path ( functions.fold , functions[i]) )
}

## Data
fold = file.path(PROJHOME , "Output", "01_allmetrics_first_phase")
load(file.path(fold,"group-masscomp-metrics.rda" ))
d.grpmas = mat
d.grpmas$indvar = d.grpmas$var
rm(mat)
load(file.path(fold,"lead-manip_metrics2.rda"  ))
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

{
table = matrix(NA,0,9,dimnames = list(NULL,c("data", "depvar", "contrast" , "estimate", "se","df","t.val-or-ratio", "pval", "sig")))

  # solo
  {
dat = "Solo flights with artificial mass loadings"
model = lme( speed ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.solo )
sum   = summary(model) # save
table = rbind( table , as.matrix(cbind(data = dat,depvar =  "speed" , pvalue(model = model,use_tukey = T))))
model = lme( ff ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.solo )
table = rbind( table , as.matrix(cbind(data = dat,depvar = "ff" ,  pvalue(model = model,use_tukey = T))))
model = lme( amp ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.solo )
table = rbind( table , as.matrix(cbind(data = dat,depvar = "amp" ,  pvalue(model = model,use_tukey = T))))
cb = cbind( data = dat,
            depvar = "speed" ,
            contrast = c("support.wind","cross.wind"),
            sum$tTable[c("support.wind","cross.wind"),],
            sig = pvalue(pval = sum$tTable[c("support.wind","cross.wind"),"p-value"]))
dimnames(cb)[[2]] = dimnames(table)[[2]]
table = rbind(table,cb)
model = lme( speed ~ indvar.num + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.solo )
sum.acc = summary(model)
acc = c( data = dat, depvar = "speed" , indvar = "Exp. cond., as num.", sum.acc$tTable["indvar.num",],
         pvalue(pval = sum.acc$tTable["indvar.num","p-value"], use_tukey = F)
)


names(acc) = dimnames(table)[[2]]
table = as.matrix(table)
table = rbind( table,as.character(acc))
model = lme( ff ~ indvar.num + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.solo )
sum.acc = summary(model)
acc = c( data = dat, depvar = "ff" , indvar = "Exp. cond., as num.", sum.acc$tTable["indvar.num",],
         pvalue(pval = sum.acc$tTable["indvar.num","p-value"], use_tukey = F)
)
names(acc) = dimnames(table)[[2]]
table = as.matrix(table)
table = rbind( table,as.character(acc))
model = lme( amp ~ indvar.num + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.solo )
sum.acc = summary(model)
acc = c( data = dat, depvar = "amp" , indvar = "Exp. cond., as num.", sum.acc$tTable["indvar.num",],
         pvalue(pval = sum.acc$tTable["indvar.num","p-value"], use_tukey = F)
)

names(acc) = dimnames(table)[[2]]
table = as.matrix(table)
table = rbind( table,as.character(acc))
table = as.matrix(table)
}

# lead
  {
dat = "Group leadership composition"
d.lead.speed = d.lead[!is.na(d.lead$speed),]
model = lme( speed ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.lead.speed )
sum = summary(model) # save
table = rbind( table , as.matrix(cbind( data= dat,depvar = "speed" ,  pvalue(model = model,use_tukey = T))))
d.lead.ff    = d.lead[!is.na(d.lead$ff),]
model = lme( ff ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.lead.ff )
table = rbind( table , as.matrix(cbind( data =dat, depvar = "ff" ,  pvalue(model = model,use_tukey = T))))
d.lead.amp   = d.lead[!is.na(d.lead$amp),]
model = lme( amp ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.lead.amp )
table = rbind( table , as.matrix(cbind( data = dat, depvar ="amp" ,  pvalue(model = model,use_tukey = T))))
cb = cbind( data = dat,
            depvar = "speed" ,
            contrast = c("support.wind","cross.wind"),
            sum$tTable[c("support.wind","cross.wind"),],
            sig = pvalue(pval = sum$tTable[c("support.wind","cross.wind"),"p-value"]))
dimnames(cb)[[2]] = dimnames(table)[[2]]
table = rbind(table,cb)
table = as.matrix(table)
}
#grpmas
dat = "Group mass composition"
d.grpmas.speed = d.grpmas[!is.na(d.grpmas$speed),]
d.grpmas.speed
model = lme( speed ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.speed )
sum = summary(model) # save

## EXTRA STUFF
model2 = lme( speed ~ homo_hetero + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.speed )
summary(model2) # save
load(file.path ( PROJHOME, "Output" , "02_postreviewOutput", "speedvariance.rda"))
d.grpmas$sd = dat$sds
d.grpmas.speed = d.grpmas[!is.na(d.grpmas$speed),]
model3 = lme( sd ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.speed )
summary(model3) # save
names ( d.grpmas.speed) [39] = "speed.variance"
model4 = lme( speed.variance ~ homo_hetero + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.speed )
summary(model4) # save

table = rbind( table , as.matrix(cbind( data =dat,depvar="speed" ,  pvalue(model = model,use_tukey = T))))

d.grpmas.ff    = d.grpmas[!is.na(d.grpmas$ff),]
model = lme( ff ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.ff )
summary(model)

# extra stuff
model2 = lme( totff ~ homo_hetero + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.ff )
summary(model2)

model2 = lme( totamp ~ homo_hetero + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.ff )
summary(model2)

table = rbind( table , as.matrix(cbind( data = dat, depvar = "ff" ,  pvalue(model = model,use_tukey = T))))
d.grpmas.amp   = d.grpmas[!is.na(d.grpmas$amp),]
model = lme( amp ~ indvar + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.amp )
table = rbind( table , as.matrix(cbind( data =dat, depvar = "amp" ,  pvalue(model = model,use_tukey = T))))
cb = cbind( data = dat,
            depvar = "speed" ,
            contrast = c("support.wind","cross.wind"),
            sum$tTable[c("support.wind","cross.wind"),],
            sig = pvalue(pval = sum$tTable[c("support.wind","cross.wind"),"p-value"]))
dimnames(cb)[[2]] = dimnames(table)[[2]]
table = rbind(table,cb)
model = lme( speed ~ indvar.num + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.speed )
sum.acc = summary(model)
acc = c( data = dat, depvar = "speed" , indvar = "Exp. cond., as num.", sum.acc$tTable["indvar.num",],
         pvalue(pval = sum.acc$tTable["indvar.num","p-value"], use_tukey = F)
)
names(acc) = dimnames(table)[[2]]
table = as.matrix(table)
table = rbind( table,as.character(acc))
model = lme( ff ~ indvar.num + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.ff )
sum.acc = summary(model)
acc = c( data = dat, depvar = "ff" , indvar = "Exp. cond., as num.", sum.acc$tTable["indvar.num",],
         pvalue(pval = sum.acc$tTable["indvar.num","p-value"], use_tukey = F)
)
names(acc) = dimnames(table)[[2]]
table = as.matrix(table)
table = rbind( table,as.character(acc))
model = lme( amp ~ indvar.num + cross.wind + support.wind, random = list(pidge=~1,date=~1), data = d.grpmas.amp )
sum.acc = summary(model)
acc = c( data = dat, depvar = "amp" , indvar = "Exp. cond., as num.", sum.acc$tTable["indvar.num",],
         pvalue(pval = sum.acc$tTable["indvar.num","p-value"], use_tukey = F)
)
names(acc) = dimnames(table)[[2]]
table = as.matrix(table)
table = rbind( table,as.character(acc))
table = as.matrix(table)
}
# finish
table = as.matrix(table)
dimnames(table)=list(NULL,dimnames(table)[[2]])
num.vars = c("estimate", "se","t.val-or-ratio","pval")
whi = which(dimnames(table)[[2]] %in% num.vars)
for ( i in whi){
  #i=whi[1]
  vec = as.character(round(as.numeric(table[,i]),4))
  table[,i] = ifelse ( vec == "0", "<0.001", vec)
}
dimnames(table)[[2]] = c("Data",	"Dep. var", "Indep. var", 	"Value"	,"SE" ,"df",	"t-rat./val.",	"p-val.",	"Sig.")
table



write.csv ( table , file = file.path(PROJHOME, "Output" , "Tables_stats" ,"sigTables.csv"))



# 2. mass predicts speed in solo fliers.

library(MASS)
d.solo$indvar = d.solo$birdmass
b = boxcox(grnspeed ~ indvar + as.numeric(flight) + support.wind + cross.wind, data= d.solo)
bb = bestbox(b,d.solo$indvar)
d.solo$indvar = bb[[1]]
mod = lme ( grnspeed ~ birdmass + as.numeric(flight) + support.wind + cross.wind , random = list(date=~1,massload=~1) ,data = d.solo)
summary(mod)
diagnostics.plot(mod)
d.solo2 = d.solo[-which(d.solo$ff > 10),]
d.solo$indvar = d.solo$birdmass
b = boxcox(ff~ indvar + as.numeric(flight) + support.wind + cross.wind, data= d.solo2)
bb = bestbox(b,d.solo2$birdmass)
#d.solo2$indvar = bb[[1]]
 d.solo2$indvar = d.solo2$birdmass
mod = lme ( ff ~ indvar + as.numeric(flight) + support.wind + cross.wind , random = list(date=~1,massload=~1) ,data = d.solo2)
summary(mod)
diagnostics.plot(mod)
mod = lme ( amp ~ birdmass + as.numeric(flight) + support.wind + cross.wind ,
            random = list(date=~1,massload=~1) ,data = d.solo2)
summary(mod)
diagnostics.plot(mod)


#3. Flap frequency is greater in leaders after controlling for speed?

mod = lme( ff ~ lead.fol + as.numeric(flight.num) + medspeed + support.wind + cross.wind,
           random = list( date=~1, uniqueflight=~1, pidge=~1), data = d.lead,
           na.action = na.omit)
diagnostics.plot(mod)
summary(mod)

mod = lme( amp ~ lead.fol + as.numeric(flight.num) + medspeed + support.wind + cross.wind,
           random = list( date=~1, uniqueflight=~1, pidge=~1), data = d.lead,
           na.action = na.omit)
diagnostics.plot(mod)
summary(mod)

d.lead$ffamp = d.lead$ff^3 + d.lead$amp^2
b = boxcox(ffamp ~ lead.fol + as.numeric(flight.num) + medspeed + support.wind
           + cross.wind, data= d.lead)
bb = bestbox(b,d.lead$ffamp)
d.lead$depvar = bb[[1]]
mod = lme( depvar ~ lead.fol + as.numeric(flight.num) + medspeed + support.wind + cross.wind,
           random = list( date=~1, uniqueflight=~1, pidge=~1), data = d.lead,
           na.action = na.omit)
diagnostics.plot(mod)
summary(mod)
bb[[2]]

##4. Do homogeneous mass compositions have decreased energy expenditure when controlling for speed

d.grpmas$prop.num = as.numeric(as.character ( d.grpmas$prop.heavyatstart))

d.grpmas$support           = as.numeric( as.character( d.grpmas$support))
d.grpmas$cross             = as.numeric( as.character( d.grpmas$cross))
d.grpmas$medspeedingroup   = as.numeric( as.character( d.grpmas$medspeedingroup))

d.grpmas$daymonth = apply( cbind ( d.grpmas$day, d.grpmas$month), 1, function(x){paste(x,collapse = ".")})

d.grpmas$homo.hetero = NA
d.grpmas$homo.hetero[d.grpmas$prop.num > 0.33 &
                     d.grpmas$prop.num < 0.67] = "hetero"
d.grpmas$homo.hetero[d.grpmas$prop.num < 0.33 |
                       d.grpmas$prop.num > 0.67] = "homo"



b = boxcox(amp ~ homo.hetero + + medspeedingroup + support
           + cross, data= d.grpmas)
bb = bestbox(b,d.grpmas$amp)
d.grpmas$depvar = bb[[1]]
mod = lme( depvar ~ homo.hetero
           # + medspeedingroup
           + support + cross
           ,random = list(  unique.flight=~1, pidge=~1)
           , data = d.grpmas,
           na.action = na.omit)
summary(mod)
diagnostics.plot(mod)
#  28  -0.725  0.480

b = boxcox(ff ~ homo.hetero + + medspeedingroup + support
           + cross, data= d.grpmas)
bb = bestbox(b,d.grpmas$ff)
d.grpmas$depvar = bb[[1]]
mod = lme( depvar ~ homo.hetero
           # + medspeedingroup
           + support + cross
           ,random = list(  unique.flight=~1, pidge=~1)
           , data = d.grpmas,
           na.action = na.omit)
summary(mod)
diagnostics.plot(mod)

# 28 0.396  0.694

# descripitives
tapply( d.grpmas$amp, d.grpmas$prop.num, function(x){ median(x, na.rm = T)})
tapply( d.grpmas$amp, d.grpmas$prop.num, function(x){    max(x, na.rm = T)})


