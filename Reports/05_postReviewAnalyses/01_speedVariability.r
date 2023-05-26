rm ( list = ls())

# functions
functions.fold = file.path (getwd() , "R")
functions = list.files( functions.fold)
for ( i in 1:length(functions)){
  source ( file.path ( functions.fold , functions[i]) )
}

# data
data.fold = file.path (getwd() , "Data" , "GMC" , "groupedGPS")
data.files = list.files (data.fold)

sdss = NULL
medss = NULL
for ( i in 1:length(data.files)){
  #i=1
  load ( file.path ( data.fold , data.files[i]))
  dims = dim ( data)
  sds = NULL
  meds = NULL
  for ( j in 1:dims[3]){


  speeds = get_dist(data[,1,j], data[,2,j], hz = 5 , method = "speed")
  speeds [ speeds > 35] = NA
  sds = c( sds , sd ( speeds,  na.rm = T))
  meds = c(meds, median ( speeds, na.rm=T))
  }
  sdss = rbind ( sdss , data.frame(sds  ,i,dimnames(data)[[3]]))
  medss= rbind ( medss, data.frame(meds ,i,dimnames(data)[[3]]))


}


mat = stringr::str_split_fixed( data.files , "\\." , 6)
mat = as.data.frame( mat)
mat$propheavy = rep ( c(1,0.67,0,0.33) , each = 8)
mat$flight = rownames (mat)
sdss$flight = mat$propheavy[ sdss$i ]
dat= sdss
dat$meds = medss$meds
save( dat, file = file.path ( PROJHOME, "Output" , "02_postreviewOutput", "speedvariance.rda"))
rm ( list = ls())
fold = file.path(PROJHOME , "Output", "01_allmetrics_first_phase")
load(file.path(fold,"group-masscomp-metrics.rda" ))
load(file.path ( PROJHOME, "Output" , "02_postreviewOutput", "speedvariance.rda"))
plot ( mat$pidge , dat$dimnames.data...3..)
mat$sd.speed = dat$sds

plot ( mat$sd.speed ~ mat$prop.heavyatstart)
mat$medspeedingroup =  as.numeric( as.character( mat$medspeedingroup))
mat$medspeedingroup [ mat$medspeedingroup < 5] = NA
plot (mat$medspeedingroup~ mat$prop.heavyatstart)

mat$homohetero = NA

mat$homohetero [ round ( as.numeric( as.character ( mat$prop.heavyatstart)), 2) %in% c( 0.33,0.67)] = "hetero"
mat$homohetero [ mat$prop.heavyatstart %in% c( 0,1)] = "homo"
names( mat)
plot (mat$medspeedingroup~ as.factor( mat$homohetero) )
data = mat
# Load required packages
library(lme4)

data = mat [ , c( "homohetero" , "prop.heavyatstart" , "support" , "cross" , "day" , "pidge" ,"medspeedingroup")]

# Encode categorical variables as factors if needed
data$homohetero <- as.factor(data$homohetero)
data$prop.heavyatstart <- as.factor(data$prop.heavyatstart)
data$support <- as.numeric(as.character ( data$support))
data$cross <- as.numeric(as.character ( data$cross))
data$day <- as.factor(data$day)
data$pidge <- as.factor(data$pidge)

# Specify the mixed model formula
model_formula <- medspeedingroup ~ prop.heavyatstart + support + cross

# Fit the mixed model
mixed_model <- lme(model_formula, random = ~ 1 | day, data = na.exclude(data))

# Print the model summary
summary(mixed_model)
