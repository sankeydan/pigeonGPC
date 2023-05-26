# housekeeping
rm(list = ls())

## Libraries
library( nlme )
library(emmeans)
library(plyr)

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

# source
source( file.path(PROJHOME , "Source", "02_statsource.r"))

load(file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", "fis-dist=50minflight-prop=0.1","dat-group.rda"))
d.training = dg[dg$experiment == "Training",]
