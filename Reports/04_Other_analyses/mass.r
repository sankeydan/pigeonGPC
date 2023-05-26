#mass repeatability

rm(list =ls())
load( file.path(PROJHOME ,"Data" ,"metadata" , "pigeon-mass-raw.rda"))

library(lmerTest)
library(rptR)

massdata = data.frame( massdata)
massdata$Short_ring = as.character(massdata$Short_ring)
massdata$Mass = as.numeric(massdata$Mass)
stacked.data <- data.frame(ID = massdata$Short_ring,
                           dave = massdata$Mass)

m1 <- lmer(dave ~ 1 + (1|ID), data = stacked.data)
summary(m1)

#Repeatability
unlist(VarCorr(m1))["ID"] / (unlist(VarCorr(m1))["ID"] + attr(VarCorr(m1), "sc")^2)

rep1 <- rpt(dave ~ (1|ID), grname = "ID", data = stacked.data,
            datatype = "Gaussian", nboot = 1000, npermut = 0)

#In order in the paper
summary(rep1)

par(mfrow = c(1,1), mar = c(4,4,0,0)+0.5)

plot(range(stacked.data$dave, na.rm = TRUE) ~ c(1,3),
     lwd = 1.5, las = 1,
     xlab = expression(bold("Trial")),
     ylab = expression(bold("Furthest point reached (m)")),
     type = "n", axes = F)
axis(side = 1, las = 1, lwd = 1.5, at = c(1,2,3), labels = c(1,2,3))
axis(side = 2, las = 1, lwd = 1.5)
box(lwd = 1.5)
for(i in 1:nrow(data)){
  lines(x = c(1:3), y = data[i,c("FP1", "FP2", "FP3")], lwd = 1.5)
}
text(expression(bold("(a)")),
     x = (0.07*(par("usr")[2] - par("usr")[1]) + par("usr")[1]),
     y = (0.93*(par("usr")[4] - par("usr")[3]) + par("usr")[3]))

plot(range(stacked.data$route.efficiency, na.rm = TRUE) ~ c(1,6),
     lwd = 1.5, las = 1,
     xlab = expression(bold("Trial")),
     ylab = expression(bold("Route efficiency")),
     type = "n", axes = F)
axis(side = 1, las = 1, lwd = 1.5, at = c(1:6), labels = c(1:6))
axis(side = 2, las = 1, lwd = 1.5)
box(lwd = 1.5)
for(i in 1:nrow(data)){
  lines(x = c(1:6), y = data[i,c("RE1", "RE2", "RE3", "RE4", "RE5", "RE6")], lwd = 1.5)
}
text(expression(bold("(b)")),
     x = (0.07*(par("usr")[2] - par("usr")[1]) + par("usr")[1]),
     y = (0.93*(par("usr")[4] - par("usr")[3]) + par("usr")[3]))

plot(range(stacked.data$total.distance, na.rm = TRUE) ~ c(1,3),
     lwd = 1.5, las = 1,
     xlab = expression(bold("Trial")),
     ylab = expression(bold("Total distance travelled (km)")),
     type = "n", axes = F)
axis(side = 1, las = 1, lwd = 1.5, at = c(1,2,3), labels = c(1,2,3))
axis(side = 2, las = 1, lwd = 1.5)
box(lwd = 1.5)
for(i in 1:nrow(data)){
  lines(x = c(1:3), y = data[i,c("TD1", "TD2", "TD3")], lwd = 1.5)
}
text(expression(bold("(c)")),
     x = (0.07*(par("usr")[2] - par("usr")[1]) + par("usr")[1]),
     y = (0.93*(par("usr")[4] - par("usr")[3]) + par("usr")[3]))

plot(range(stacked.data$total.time, na.rm = TRUE) ~ c(1,3),
     lwd = 1.5, las = 1,
     xlab = expression(bold("Trial")),
     ylab = expression(bold("Total time (min)")),
     type = "n", axes = F)
axis(side = 1, las = 1, lwd = 1.5, at = c(1,2,3), labels = c(1,2,3))
axis(side = 2, las = 1, lwd = 1.5)
box(lwd = 1.5)
for(i in 1:nrow(data)){
  lines(x = c(1:3), y = data[i,c("T1", "T2", "T3")], lwd = 1.5)
}
text(expression(bold("(d)")),
     x = (0.07*(par("usr")[2] - par("usr")[1]) + par("usr")[1]),
     y = (0.93*(par("usr")[4] - par("usr")[3]) + par("usr")[3]))


plot(range(stacked.data$exploration, na.rm = TRUE) ~ c(1,3),
     lwd = 1.5, las = 1,
     xlab = expression(bold("Trial")),
     ylab = expression(bold("Exploration")),
     type = "n", axes = F)
axis(side = 1, las = 1, lwd = 1.5, at = c(1,2,3), labels = c(1,2,3))
axis(side = 2, las = 1, lwd = 1.5)
box(lwd = 1.5)
for(i in 1:nrow(data)){
  lines(x = c(1:3), y = data[i,c("Exploration.1", "Exploration.2", "Exploration.3")], lwd = 1.5)
}
text(expression(bold("(e)")),
     x = (0.93*(par("usr")[2] - par("usr")[1]) + par("usr")[1]),
     y = (0.93*(par("usr")[4] - par("usr")[3]) + par("usr")[3]))


