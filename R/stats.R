stats = function( dat,depvar,fixedvar = "lead.follead", confint = F,
                  ylab = depvar, group.level = F,limit = 1.1, mass.vars = T){

  # Objects


  # dat.save = dat
  # dat = dat.save
  # depvar = "time2ret"
  # fixedvar = "lead.follead"
  # wind.vars = T
  # confint = F
  # ylab = depvar
  # limit = 1.1
  # group.level = F

  # libraries
  {
    library( lme4)
    library( MuMIn)
    library(ggplot2)
    library(ggsignif)
    library(gridExtra)
    library(leadManip)
  }
  # functions
  lim = function ( x,const = 1.05){
    c(min(x,na.rm = T),(max(x,na.rm = T)*const))
  }

  # grouplevel
  if ( group.level){
    dat = dat[!duplicated( dat$uniqueflight),]
  }

  # manip
  col = which ( names (dat) == depvar)

  # model

  if ( group.level){
    if( mass.vars ){
      mod  = lme4::lmer( dat[,col] ~ lead.fol  + group.size + (1|group.num) + (1|date)  + gromeanmass + support * cross, data=dat)
    } else{
      mod  = lme4::lmer( dat[,col] ~ lead.fol  + group.size + (1|group.num) + (1|date)  + support * cross, data=dat)
    }
  } else{
    if( mass.vars ){
      mod  = lme4::lmer( dat[,col] ~ lead.fol  + group.size + (1|group.num) + (1|name) + (1|date)  + mass  + gromeanmass + support * cross, data=dat)
    }  else {
      mod  = lme4::lmer( dat[,col] ~ lead.fol  + group.size + (1|group.num) + (1|name) + (1|date)  + support * cross, data=dat)
    }
  }


#confint
if ( confint){
  ci = confint(mod)[fixedvar,]
} else {
  ci= rep(NA,2)
}

# r2
r2 = r.squaredGLMM(mod)[2]

# t
t  = summary(mod)$coeff[fixedvar,3]

# p
coefs <- data.frame(coef(summary(mod)))
p.z <- 2 * (1 - pnorm(abs(coefs[fixedvar,"t.value"])))
p.z
sig = ifelse ( p.z < 0.001, "***", ifelse( p.z < 0.01, "**", ifelse(p.z < 0.05, "*", "N.S.")))
if( confint){
  sig = ifelse( sign(ci)[1] == sign(ci)[2], "*", "N.S.")
}
# plot
attach(dat)
g = gplot( dat[,col], lim(dat[,col],limit),depvar, sig)
detach(dat)

# stats combined
sta = c( ci,r2,t,p.z)
names( sta ) = c("2.5%","97.5%", "r2c", "t","p")

# return
return( list ( mod,sta , g))
}
