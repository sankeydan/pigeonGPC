# 02_statsource

# for ( i in 1:ncol( d.grpmas)){
# print( paste( names(d.grpmas)[i] , class(d.grpmas[,i])))
# }

for  ( i in 6:ncol( d.grpmas)){
d.grpmas[,i] = as.numeric(as.character(d.grpmas[,i]))
}

d.solo$massload=as.factor(d.solo$massload)


d.grpmas$date = apply(d.grpmas[,c("day","month")],1, function(x){paste0(x,collapse = ".")})
d.solo$date   = apply(d.solo  [,c("day","month")],1, function(x){paste0(x,collapse = ".")})
spl = stringr::str_split_fixed(d.lead$date,"-",3)
d.lead$date   = apply(spl     [,3:2]             ,1, function(x){paste0(x,collapse = ".")})

d.grpmas$speed = d.grpmas$medspeedingroup
d.lead$speed   = d.lead$medspeedin

names(d.solo)[1]= "pidge"
names(d.lead)[2]= "pidge"
names(d.grpmas)

d.lead$cross.wind = -d.lead$cross
d.lead$support.wind = -d.lead$support
d.grpmas$cross.wind = d.grpmas$cross
d.grpmas$support.wind = d.grpmas$support

d.grpmas$prop.heavyatstart = round(d.grpmas$prop.heavyatstart,2)
d.solo$indvar = as.character( paste0( d.solo$massload       ,"g"  ))
d.lead$indvar = as.character( d.lead$lead.fol            )
d.grpmas$indvar=as.character( d.grpmas$prop.heavyatstart)
d.solo$indvar.num = as.numeric(as.character(d.solo$massload))
d.lead$indvar.num = as.numeric(as.factor(d.lead$lead.fol))
d.grpmas$indvar.num = as.numeric(as.character(d.grpmas$prop.heavyatstart))


indvars = c("indvar","indvar.num", "speed","support.wind","cross.wind","pidge","date","ff","amp")

d = rbind(
           d.lead[,indvars],
           d.grpmas[,indvars])

d$indvar = as.factor(d$indvar)

d.grpmas$homo_hetero = "Homo"
d.grpmas$homo_hetero[d.grpmas$prop.heavyatstart >0.2 &
                     d.grpmas$prop.heavyatstart <0.8 ] = "Hetero"



mat = stringr::str_split_fixed(dg$date, "-", 3)
dg$date = apply (mat[,3:2], 1, function( x) { paste(x,collapse = ".")})

dgl = dg[dg$experiment == "leadManip",]

mat = cbind( as.character(dgl$date) , as.character(dgl$lead.fol))
dgl$datelf = apply( mat, 1, function (x){paste(x,collapse = ".")})



mat = cbind( as.character(d.lead$date) , as.character(d.lead$lead.fol))

d.lead$datelf = apply( mat, 1, function (x){paste(x,collapse = ".")})
df1 = data.frame(
stasis= tapply( dgl$flock.stasis, dgl$datelf   , function (x){mean(x,na.rm = T)}),
ff    = tapply( d.lead$ff       , d.lead$datelf, function (x){mean(x,na.rm = T)}),
amp   = tapply( d.lead$amp      ,d.lead$datelf , function (x){mean(x,na.rm = T)}),
speed = tapply( d.lead$speed    ,d.lead$datelf , function (x){mean(x,na.rm = T)}),
exp   = "leadManip")

cbind( names(d1),names(d2))
summary(lm( d1~d2))#ff
summary(lm( d1~d3))#amp

dgm = dg[dg$experiment=="GMC",]
dgm$prop.heavy = NA
dgm$prop.heavy[dgm$masscond == "h.all"]   = 1
dgm$prop.heavy[dgm$masscond == "h.mixed"] = 0.67
dgm$prop.heavy[dgm$masscond == "l.all"]   = 0.33
dgm$prop.heavy[dgm$masscond == "l.mixed"] = 0


d.grpmas$day   = stringr::str_pad(d.grpmas$day   , 2, pad = "0")
d.grpmas$month = stringr::str_pad(d.grpmas$month , 2, pad = "0")
d.grpmas$date  = apply( cbind( d.grpmas$day  , d.grpmas$month            ), 1, function (x){paste(x, collapse = ".")})
dgm$datem      = apply( cbind(      dgm$date ,      dgm$prop.heavy       ), 1, function (x){paste(x, collapse = ".")})
d.grpmas$datem = apply( cbind( d.grpmas$date , d.grpmas$prop.heavyatstart), 1, function (x){paste(x, collapse = ".")})

df2 = data.frame(
stasis = tapply( dgm$flock.stasis,     dgm$datem, function (x){mean(x,na.rm = T)}),
ff     = tapply( d.grpmas$ff     ,d.grpmas$datem, function (x){mean(x,na.rm = T)}),
amp    = tapply( d.grpmas$amp    ,d.grpmas$datem, function (x){mean(x,na.rm = T)}),
speed  = tapply( d.grpmas$speed  ,d.grpmas$datem, function (x){mean(x,na.rm = T)}),
exp = "GMC")

df = rbind( df1, df2)

ggplot2()
mod = lme ( stasis ~ ff , random = list ( exp=~1), data = df, na.action =  na.omit)
summary(mod)
mod = lme ( stasis ~ amp , random = list ( exp=~1), data = df, na.action =  na.omit)
summary(mod)
