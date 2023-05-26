# housekeeping
rm(list = ls())

## objects
ss = 1
pdf_it = F
filePath = file.path( PROJHOME , "Figures", "Boxplots", "Boxplots.pdf")

## Data
fold = file.path(PROJHOME , "Output", "01_allmetrics_first_phase")
files= list.files(fold)
load(file.path(fold,files[1]))
d.grpmas = mat
rm(mat)
load(file.path(fold,files[2]))
d.lead = dat
rm(dat)
load(file.path(fold,files[3]))
d.solo = metrics
rm(metrics)

cols1 = c("#61E8E1","#F25757","#F2E863")


# data sorting
source(file.path(PROJHOME, "Source", "01_plotsource.r"))

# plot function
boxy.lady = function ( var1, var2 , data,ylims=NULL,ylb=NULL,mai = NULL,xlb=NULL,jit,colz=NULL){
  # data = d.solo
  # var1 = "amp"
  # var2 = "massload"
  # jit = 0.1
  # ylims = NULL
  # ylb=NULL
  # mai = "fluff"
  # xlb=NULL
  # colz = cols1[3]

  d1 = data[,which( names(data) %in% var1)]
  d2 = data[,which( names(data) %in% var2)]
  boxplot( d1 ~ d2,cex.lab = 0.5, bty = "n" ,data = data ,ylim = ylims, ylab = ylb,xlab=xlb,col = colz)
  title( main = mai, cex.main = 0.5)
  par(new=T)
  unq = unique(d2)
  d2=apply ( t(d2),2, function(x){
    # x = t(d2)[1]
    which(sort(unq) == x)} )
  d1.diff = abs(d1 - median(d1,na.rm = T))+1
  set.seed(ss)
  d2.rnorm = rnorm(d2,d2,jit)
  d2.diff = d2.rnorm - d2
  add1 = d2.diff/d1.diff
  d2 = d2 + add1
  d2.diff = d2/d1.diff



  suppressWarnings(plot(d1~ d2 , bty = "l", xaxt = "n"  , yaxt = "n" , xlab = "", ylab = "",data = data, xlim = c(0.5,length(unq)+0.5),
             vertical = TRUE, ylim = ylims,
             pch = 21, cex = 1.5,col = "black", bg = colz,
             add1 = TRUE))
}

# plot
if ( pdf_it ){
  pdf(filePath,useDingbats = F)
par(mfcol =c(3,3),mar = c(5,5,4,1))
boxy.lady("airspeed"       , "massload"         ,d.solo  , c(12,27),"Speed (m/s)"               ,"Solo mass loading"                                          ,jit = 0.1, col = cols1[1])
boxy.lady("ff"             , "massload"         ,d.solo  , c(4,9)  ,"Flap frequency (Hz)"       ,NULL                                                         ,jit = 0.1, col = cols1[1])
boxy.lady("amp"            , "massload"         ,d.solo  , c(8,28) ,"Dorsal body amplitude (mm)",NULL                          ,"Mass load (g)"               ,jit = 0.1, col = cols1[1])
boxy.lady("medspeed"       , "lead.fol"         ,d.lead  , c(12,27),NULL                        ,"Group Leadership composition"                               ,jit = 0.1, col = cols1[2])
boxy.lady("ff"             , "lead.fol"         ,d.lead  , c(4,9)  ,NULL                        ,NULL                                                         ,jit = 0.1, col = cols1[2])
boxy.lady("amp"            , "lead.fol"         ,d.lead  , c(8,28) ,NULL                        ,NULL                          ,"Leader or follower groups"   ,jit = 0.1, col = cols1[2])
boxy.lady("medspeedingroup", "prop.heavyatstart",d.grpmas, c(12,27),NULL                        ,"Group Mass composition"                                     ,jit = 0.1, col = cols1[3])
boxy.lady("ff"             , "prop.heavyatstart",d.grpmas, c(4,9)  ,NULL                        ,NULL                                                         ,jit = 0.1, col = cols1[3])
boxy.lady("amp"            , "prop.heavyatstart",d.grpmas, c(8,28) ,NULL                        ,NULL                          ,"Proportion heavy individuals",jit = 0.1, col = cols1[3])
dev.off()
}
dev.off()
{
  par(mfcol =c(3,3),mar = c(5,5,4,1))
  boxy.lady("airspeed"       , "massload"         ,d.solo  , c(12,27),"Speed (m/s)"               ,"Solo mass loading"                                          ,jit = 0.2, col = cols1[1])
  boxy.lady("ff"             , "massload"         ,d.solo  , c(4,9)  ,"Flap frequency (Hz)"       ,NULL                                                         ,jit = 0.2, col = cols1[1])
  boxy.lady("amp"            , "massload"         ,d.solo  , c(8,28) ,"Dorsal body amplitude (mm)",NULL                          ,"Mass load (g)"               ,jit = 0.2, col = cols1[1])
  boxy.lady("medspeed"       , "lead.fol"         ,d.lead  , c(12,27),NULL                        ,"Group Leadership composition"                               ,jit = 0.2, col = cols1[2])
  boxy.lady("ff"             , "lead.fol"         ,d.lead  , c(4,9)  ,NULL                        ,NULL                                                         ,jit = 0.2, col = cols1[2])
  boxy.lady("amp"            , "lead.fol"         ,d.lead  , c(8,28) ,NULL                        ,NULL                          ,"Leader or follower groups"   ,jit = 0.2, col = cols1[2])
  boxy.lady("medspeedingroup", "prop.heavyatstart",d.grpmas, c(12,27),NULL                        ,"Group Mass composition"                                     ,jit = 0.2, col = cols1[3])
  boxy.lady("ff"             , "prop.heavyatstart",d.grpmas, c(4,9)  ,NULL                        ,NULL                                                         ,jit = 0.2, col = cols1[3])
  boxy.lady("amp"            , "prop.heavyatstart",d.grpmas, c(8,28) ,NULL                        ,NULL                          ,"Proportion heavy individuals",jit = 0.2, col = cols1[3])

}


# 2. birdmass

  {
  library(ggplot2)
  library(gridExtra)
g1 = ggplot(d.solo, aes ( birdmass, grnspeed))+
  geom_point()+
  geom_smooth(method = "lm")+
ylab ( "Ground speed (m/s)")
g2 = ggplot(d.solo, aes ( birdmass, ff))+
  geom_point()+
  geom_smooth(method = "lm")+
ylab ( "Flap frequency (Hz)")
g3 = ggplot(d.solo, aes ( birdmass, amp))+
  geom_point()+
  geom_smooth(method = "lm")+
ylab ( "Dorsal body amplitude (mm)")
grid.arrange(g1, g2, g3 , ncol= 1)
  }



### bird mass and amount of mass

library(ggplot2)
d.solo$perc.mass = (20.5 + d.solo$massload) / d.solo$birdmass

d.solo$ffamp = d.solo$ff ^3 + d.solo$amp ^2
ggplot ( d.solo, aes ( perc.mass , speed, col = "p"))+
  geom_smooth(method = "lm")

ggplot ( d.solo, aes ( perc.mass , amp, col =as.factor( pigeon) ))+
  geom_smooth(method = "lm")

ggplot ( d.solo, aes ( perc.mass , ff, col =as.factor( pigeon) ))+
  geom_smooth(method = "lm")

ggplot ( d.solo, aes ( perc.mass , ffamp, col =as.factor( pigeon) ))+
  geom_smooth(method = "lm")
