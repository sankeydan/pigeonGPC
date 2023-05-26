gplot = function( yvar , ylim, ylab, sig){

  # yvar = allneisd
  # ylim = lim(allneisd    ,1.05)
  # ylab = "sd nei dist"
  # sig  = "*"

  dat = dat[which(!is.na(yvar)),]
  yvar= na.omit(yvar)
  ggplot(dat, aes(x=lead.fol, y=yvar))+
    geom_boxplot() +
    geom_signif(comparisons = list(c("fol", "lead")), annotations = sig) +
    coord_cartesian(ylim = ylim)+
    xlab("")+
    ylab(ylab)
}
