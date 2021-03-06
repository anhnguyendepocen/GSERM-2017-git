###############################################
# GSERM 2017 Day Four a.m.
#
# File created June 21, 2017
#
# File last updated June 21, 2017
###############################################
# Set working directory as necessary:
#
setwd("~/Dropbox (Personal)/GSERM/Materials 2017/Notes and Slides")
#
###############################################
# Options:
#
options(scipen = 6) # bias against scientific notation
options(digits = 6) # show fewer decimal places
###############################################
# Packages:

require(RCurl)
require(plyr)
require(lme4)
library(plm)
require(rgeos)
require(maptools)
require(rworldmap)

###############################################
# Data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/HIVDeaths.csv")
HIV<-read.csv(text=temp, header=TRUE)
HIV<-HIV[ which(is.na(HIV$HIVDeathRate)==FALSE), ]
HIV$LnDeathPM <- log(HIV$HIVDeathRate*1000)
summary(HIV)

pdf("HIVDeaths.pdf",6,5)
par(mar=c(4,4,2,2))
with(HIV, plot(density(HIVDeathRate,na.rm=TRUE),
               xlim=c(0,8.5),lwd=3,main="",
               xlab="Value"))
with(HIV, lines(density(LnDeathPM,na.rm=TRUE),
                   lwd=3,lty=2,col="red"))
legend("topright",bty="n",lwd=3,col=c("black","red"),
       legend=c("Deaths Per Thousand","ln(Deaths Per Million)"))
dev.off()

###############################################
# Models...
#
# OLS:

OLSfit<-with(HIV, lm(LnDeathPM~GDPLagK+GDPGrowthLag+
                     OPENLag+POLITYLag+POLITYSQLag+CivilWarDummy+
                     InterstateWarLag+BatDeaths1000Lag))
summary(OLSfit)

# Fixed Effects:

FEfit<-plm(LnDeathPM~GDPLagK+GDPGrowthLag+
                     OPENLag+POLITYLag+POLITYSQLag+CivilWarDummy+
                     InterstateWarLag+BatDeaths1000Lag,
                     data=HIV,effect="individual", model="within",
                     index=c("ISO3","year"))
summary(FEfit)

# Random effects via lmer:

REfit<-lmer(LnDeathPM~GDPLagK+GDPGrowthLag+OPENLag+
              POLITYLag+POLITYSQLag+CivilWarDummy+
              InterstateWarLag+BatDeaths1000Lag+(1|ISO3),
              data=HIV,REML=FALSE)
summary(REfit)

# HLMs / varying coefficient models:
#
# Varying (random) intercept and slope on GDP

HLMfit1<-lmer(LnDeathPM~GDPLagK+(GDPLagK|ISO3)+GDPGrowthLag+
             OPENLag+POLITYLag+POLITYSQLag+CivilWarDummy+
             InterstateWarLag+BatDeaths1000Lag,
             data=HIV,REML=FALSE)
summary(HLMfit1)

# Testing:

anova(REfit,HLMfit1)
VarCorr(HLMfit1)

# Coefficients:

Bs<-data.frame(coef(HLMfit1)[1])

head(Bs)
mean(Bs$ISO3.GDPLagK)
var(Bs$ISO3.GDPLagK)

pdf("GDPRandomBetas.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(density(ISO3.GDPLagK),lwd=3,
              main="",xlab="Beta Values"))
abline(v=mean(Bs$ISO3.GDPLagK),lty=2)
dev.off()

# Varying intercept and slope on trade openness:

HLMfit2<-lmer(LnDeathPM~GDPLagK+GDPGrowthLag+OPENLag+
                (GDPGrowthLag|ISO3)+POLITYLag+POLITYSQLag+CivilWarDummy+
                InterstateWarLag+BatDeaths1000Lag,
              data=HIV,REML=TRUE)
summary(HLMfit2)


