###############################################
# GSERM 2017 Day One p.m.
#
# File created June 19, 2017
#
# File last updated June 23, 2017
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

######################
# Dummy variables
#
# Ugly dummy variable scatterplot:

set.seed(7222009)
X <- rbinom(100,1,0.5)
Y <- 10 + 10*X + (5*rnorm(100))
Dfit <- lm(Y~X)
Dhat <- predict(Dfit)

pdf("UglyDummyScatterplotR.pdf",5,5)  
par(mar=c(4,4,2,2))
plot(X,Y,pch=20)
points(X,Dhat,pch=4,col="red",cex=2)
abline(Dfit,lwd=2,col="red",lty=2)
dev.off()

# Supreme Court example:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/SCOTUS-cases.csv")
SCOTUS<-read.csv(text=temp, header=TRUE)
summary(SCOTUS)

SCOTUS$civil.econ<-SCOTUS$civlibs + SCOTUS$econs

SCOTUS$termdummies<-factor(SCOTUS$term)

fit1<-with(SCOTUS, lm(Namici~civlibs))
summary(fit1)
with(SCOTUS, t.test(Namici~civlibs))

SCOTUS$civlibeffect<-SCOTUS$civlibs
SCOTUS$civlibeffect[SCOTUS$civlibs==0]<-(-1)
fit2<-with(SCOTUS, lm(Namici~SCOTUS$civlibeffect))
summary(fit2)

fit3<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib))
summary(fit3)

fit4<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib+term))
summary(fit4)

fit5<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib+as.factor(term)))
summary(fit5)

# Plot coefficients:

termbetas<-fit5$coefficients[8:39]
SE5<-sqrt(diag(vcov(fit5)))[8:39]
termUBs <- termbetas + 1.96*(SE5)
termLBs <- termbetas - 1.96*(SE5)
term<-seq(1954,1985)
pdf("TermBetasR.pdf",6,5)  
par(mar=c(4,4,2,2))
plot(term,termbetas, xlab="Term",ylab="Estimated Betas",
     pch=19,ylim=c(-0.5,2.1))
lines(term,termbetas,lwd=2)
segments(term,termLBs,term,termUBs,lwd=2,lty=2)
abline(h=0,lwd=2,lty=2,col="red")
dev.off()

#############################
# Interactions:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/ClintonTherm.csv",
             stringsAsFactors=FALSE)
ClintonTherm<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(ClintonTherm)

summary(with(ClintonTherm, lm(ClintonTherm~RConserv+GOP)))

fit1<-with(ClintonTherm, lm(ClintonTherm~RConserv+GOP+RConserv*GOP))
summary(fit1)

# Plot of thermometer scores vs. conservatism:

pdf("ClinThermScatterR.pdf",6,6)
scatterplot(ClintonTherm$ClintonTherm~ClintonTherm$RConserv|as.factor(ClintonTherm$GOP),
            legend.plot=FALSE,
            xlab="Respondent Conservatism",
            ylab="Clinton Thermometer Score",
            smooth=FALSE,boxplots=FALSE,
            pch=c(4,16),col=c("red","blue","red","blue"),
            lwd=2,grid=FALSE)
dev.off()

# Separate regressions:

NonReps<-subset(ClintonTherm,GOP==0)
summary(with(NonReps, lm(ClintonTherm~RConserv)))

Reps<-subset(ClintonTherm,GOP==1)
summary(with(Reps, lm(ClintonTherm~RConserv)))


# psi_1:
Psi1<-fit1$coeff[2]+fit1$coeff[4]
Psi1
SPsi1<-sqrt(vcov(fit1)[2,2] + (1)^2*vcov(fit1)[4,4] + 2*1*vcov(fit1)[2,4])
SPsi1
Psi1 / SPsi1 # <-- t-statistic

# psi_2 | RConserv = 1

fit1$coeff[3]+(1 * fit1$coeff[4])

sqrt(vcov(fit1)[3,3] + (1)^2*vcov(fit1)[4,4] + 2*1*vcov(fit1)[3,4])

# Implies t is approximately 2


# psi_2 | RConserv = 7

fit1$coeff[3]+(7 * fit1$coeff[4])

sqrt(vcov(fit1)[3,3] + (7)^2*vcov(fit1)[4,4] + 2*7*vcov(fit1)[3,4])

# t is approximately 11

# Using linearHypothesis:

# library(car)
linearHypothesis(fit1,"RConserv+RConserv:GOP")

# Note: Same as t-test:
sqrt(72.99)

# psi_2 | RConserv = 7:
  
  linearHypothesis(fit1,"GOP+7*RConserv:GOP")

# MFX / psi plots:

ConsSim<-seq(1,7,1)
psis<-fit1$coeff[3]+(ConsSim * fit1$coeff[4])
psis.ses<-sqrt(vcov(fit1)[3,3] + 
                 (ConsSim)^2*vcov(fit1)[4,4] + 2*ConsSim*vcov(fit1)[3,4])

pdf("ClinMFX1.pdf",7,6)
par(mar=c(4,4,2,2))
plot(ConsSim,psis,t="l",lwd=2,xlab="Respondent Conservatism",
     ylab="Estimated Marginal Effect",ylim=c(-40,0))
lines(ConsSim,psis+(1.96*psis.ses),lty=2,lwd=2)
lines(ConsSim,psis-(1.96*psis.ses),lty=2,lwd=2)
abline(h=0,lwd=1,lty=2)
dev.off()

# Continuous covariates:

fit2<-with(ClintonTherm,
           lm(ClintonTherm~RConserv+ClintonConserv+RConserv*ClintonConserv))
summary(fit2)

# Hypothesis tests:

fit2$coef[2]+(1*fit2$coef[4])
sqrt(vcov(fit2)[2,2] + (1)^2*vcov(fit2)[4,4] + 2*1*vcov(fit2)[2,4])

linearHypothesis(fit2,"RConserv+1*RConserv:ClintonConserv")

# More hypothesis tests:

# psi_1 | ClintonConserv = mean
fit2$coef[2]+((mean(ClintonTherm$ClintonConserv))*fit2$coef[4])
sqrt(vcov(fit2)[2,2] + (mean(ClintonTherm$ClintonConserv)^2*vcov(fit2)[4,4] +
                          2*(mean(ClintonTherm$ClintonConserv))*vcov(fit2)[2,4]))
pt(((fit2$coef[2]+(2.985*fit2$coef[4])) / sqrt(vcov(fit2)[2,2] + 
                                                 (2.985)^2*vcov(fit2)[4,4] + 2*2.985*vcov(fit2)[2,4])),df=1293)

# psi_2 | RConserv = 1
fit2$coef[3]+(1*fit2$coef[4])

# psi_2 | RConserv = 6
fit2$coef[3]+(6*fit2$coef[4])

# Marginal Effect Plot II:

psis2<-fit2$coef[3]+(ConsSim*fit2$coef[4])
psis2.ses<-sqrt(vcov(fit2)[3,3] + (ConsSim)^2*vcov(fit2)[4,4]
                + 2*ConsSim*vcov(fit2)[3,4])

pdf("ClinMFX2.pdf",6,6)
plot(ConsSim,psis2,t="l",lwd=2,xlab="Respondent's Conservatism",
     ylab="Marginal Effect of Clinton's 
     Conservatism",ylim=c(-10,20))
lines(ConsSim,psis2+(1.96*psis2.ses),lty=2,lwd=2)
lines(ConsSim,psis2-(1.96*psis2.ses),lty=2,lwd=2)
abline(h=0,lty=2,lwd=1,col="red")
dev.off()

# Contour Plot:

library(lattice)
grid<-expand.grid(RConserv=seq(1,7,1),
                  ClintonConserv=seq(1,7,1))
hats<-predict(fit2,newdata=grid)

pdf("ClinContour.pdf",6,6)
levelplot(hats~grid$RConserv*grid$ClintonConserv,
          contour=TRUE,
          cuts=12,pretty=TRUE,xlab="Respondent's Conservatism",
          ylab="Clinton's Conservatism",
          col.regions=heat.colors)
dev.off()

# Wireframe plot:

trellis.par.set("axis.line",list(col="transparent"))

pdf("ClinWireframe.pdf",7,7)
wireframe(hats~grid$RConserv*grid$ClintonConserv,
          drape=TRUE,
          xlab=list("Respondent's Conservatism",rot=30),
          ylab=list("Clinton's Conservatism",
                    rot=-40),zlab=list("Predictions",rot=90),
          scales=list(arrows=FALSE,col="black"),
          zoom=0.85,pretty=TRUE)
dev.off()

#######################
# 
