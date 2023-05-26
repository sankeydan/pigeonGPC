

generate_results = function( sd = 2,
flight.mean.speed = c(14,16,20,12,13,16,12,17,20),
pigeon.mean.speed = c(12,15,15,16,17,18,18,18,20),
mass.speed = c(0,0.5,1)){


vec = vector()


for ( i in 1:3){
vec = c(vec , sample(mass.speed))
}
vec2 = vector()
for ( i in 1:9){
  vec2 = c(vec2 , sample(vec))
}


mat = cbind(       rep(flight.mean.speed,each=9),rep(pigeon.mean.speed,9),vec2)

ml = cbind( c(0,5,10), mass.speed)
df = data.frame( flight = rep(1:9, each = 9),
                 pigeon = rep(1:9,9),
                 massload = apply( t(mat[,3]),2, function (x){ ml[,1][which(ml[,2] == x)]} )
)
df$speed = apply( mat , 1, function(x){ rnorm(1,mean(x[1:2]),sd) + x[3]})

return(df)
}

df =  generate_results(sd=2)

boxplot( df$speed ~ df$massload)

df$massload.f = as.factor(df$massload)
require(nlme)
require(lme4)
df$flight = as.character(df$flight)
df$pigeon = as.character(df$pigeon)
list(year=~1, date=~time)





lme_speed.f.fac = lmer(speed ~ massload.f + (1|flight) , data=df)

lme_speed.f.num = lmer(speed ~ massload   + (1|flight) , data=df)

lme_speed.p.fac = lmer(speed ~ massload.f + (1|pigeon) , data=df)

lme_speed.p.num = lmer(speed ~ massload   + (1|pigeon) , data=df)

lme_speed.fp.fac= lmer(speed ~ massload.f + (1|flight)
                                         + (1|pigeon) , data=df)

lme_speed.fp.num= lmer(speed ~ massload   + (1|flight)
                                         + (1|pigeon) , data=df)


require(car)
Anova(lme_speed.f.fac)
Anova(lme_speed.f.num)
Anova(lme_speed.p.fac)
Anova(lme_speed.p.num)
Anova(lme_speed.fp.fac)
Anova(lme_speed.fp.num)

