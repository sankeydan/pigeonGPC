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

indvars = c("indvar","indvar.num", "speed","support.wind","cross.wind","pidge","date")

d = rbind( d.solo[,indvars],
           d.lead[,indvars],
           d.grpmas[,indvars])

d$indvar = as.factor(d$indvar)

d.grpmas$homo_hetero = "Homo"
d.grpmas$homo_hetero[d.grpmas$prop.heavyatstart >0.2 &
                     d.grpmas$prop.heavyatstart <0.8 ] = "Hetero"
