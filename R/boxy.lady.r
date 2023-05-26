boxy.lady = function ( var1, var2 , data,ylims=NULL,ylb=NULL,mai = NULL,xlb=NULL,jit=1,colz=NULL,ss=1,boxcol = NULL,mult = 1){
  # data = d.grpmas
  # var1 ="depvar"
  # var2 = "prop.heavyatstart"
  # jit = 0.1
  # ylims = NULL
  # ylb=NULL
  # mai = "fluff"
  # xlb=NULL
  # colz = "grey50"
  # ss = 1
  # boxcol = NULL
  # mult = 1

  d1 = data[,which( names(data) %in% var1)]
  d2 = data[,which( names(data) %in% var2)]
  d2 = as.numeric(as.factor(d2))
  d1 = d1[!is.na(d2)]
  d2 = d2[!is.na(d2)]

  if ( is.null(colz)){
  colz = data$col
  colz = colz[!is.na(d2)]
  }
  unq = unique(d2)
  d2=apply ( t(d2),2, function(x){
    # x = t(d2)[1]
    which(sort(unq) == x)} )

  colzz = colz[which(!duplicated(data$cond))]

  if ( is.null(boxcol)){
    boxplot( d1 ~ d2,cex.lab = 0.5, bty = "n" ,data = data ,ylim = ylims, ylab = ylb,xlab=xlb,outline = F,col = colzz)
  } else {
    boxplot( d1 ~ d2,cex.lab = 0.5, bty = "n" ,data = data ,ylim = ylims, ylab = ylb,xlab=xlb,outline = F,col = boxcol)
  }
  title( main = mai, cex.main = 0.5)
  par(new=T)

  d1.diff = abs(d1 - median(d1,na.rm = T))+1
  set.seed(ss)
  d2.rnorm = rnorm(d2,d2,jit)
  d2.diff = d2.rnorm - d2
  add1 = d2.diff/d1.diff*mult
  d2 = d2 + add1



  suppressWarnings(plot(d1~ d2 , bty = "l", xaxt = "n"  , yaxt = "n" , xlab = "", ylab = "",data = data, xlim = c(0.5,length(unq)+0.5),
                        vertical = TRUE, ylim = ylims,
                        pch = 21, cex = 1.5,col = "black", bg = colz,
                        add1 = TRUE))
}
