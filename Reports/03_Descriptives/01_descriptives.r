## Descriptives

## Contents

# 1. How many flights in each condition?
# 2. Fission and route efficiency differences between groups.

############

## SCRIPT

############

# 1. How many flights in each condition?

# Source
source(file.path(PROJHOME , "Source", "03_01_descriptives.r"))

# Tables
table(d.solo$pidge)
# 15 flights
table(d.lead$pidge)
# 11 flights
table(d.grpmas$pidge)
# 16 perfect flights

# Enhanced tables
table(d.solo$pidge,as.numeric(as.character(d.solo$day)))
# lost some. Because of paired flights?



# 2. Fission and route efficiency differences between groups.

# Source
source(file.path(PROJHOME , "Source", "03_01_descriptives.r"))

d.grpmas$masscond = c( "c",as.character(d.grpmas$prop.heavyatstart))
tapply( d.grpmas$fission   , d.grpmas$masscond, FUN = function(x){ mean(x,na.rm = T)})
round(tapply( d.grpmas$effic     , d.grpmas$masscond, FUN = function(x){ mean(x,na.rm = T)}),2)*100

tapply( d.lead$fission     , d.lead$lead.fol, FUN = mean)
tapply( d.lead$route.effic , d.lead$lead.fol, FUN = mean)


# 3. Average decrease in lr-spread across groups

#Houskeeping
rm(list = ls())
fold = file.path(PROJHOME ,"Output", "Groupflight_ind_grp_metrics","dat4stats", "fis-dist=50minflight-prop=0.1")
load(file.path(fold, "dat-group.rda"))
exps = c("leadManip","GMC", "Training", "all")

tapply(dg$lrspread,dg$lead.fol,function(x){mean(x,na.rm = T)})
# wow. Despite leader flocks being larger than follower flocks due to losses from follower flocks


# 4. Dates of the experiments

# Source
source(file.path(PROJHOME , "Source", "03_01_descriptives.r"))
table(d.solo$date)
table(d.lead$date)
table(d.training$date)
table(d.grpmas$date)

# 5. Paired flights from solo flights

load ( file.path ( PROJHOME, "Data", "metadata" , "paired-flights.rda"))
spl = stringr::str_split_fixed(rb[,3],"\\.",12)
rb = cbind ( rb[,2], spl[,c(1,3)])
dimnames(rb) = list(NULL, c("sol.pair", "pidge", "mass"))
rb = data.frame(rb)
tab =table(rb$sol.pair,rb$pidge, rb$mass)
tab = t(rbind ( tab[,,1],
              tab[,,3],
              tab[,,2]))
write.csv(tab , file = file.path (PROJHOME , "Output" , "tables_stats" , "paired-flights.csv"))

# How many useable flights from training?

fold = "C:\\Users\\Sankey_Dan\\Dropbox\\R_Projects\\leadManip/Data/rawXYT"
files = list.files(fold)
spl = stringr::str_split_fixed(files, "\\.",12)
spl = spl[spl[,3] != "1",]
length(which(spl[,6] == "training"))
