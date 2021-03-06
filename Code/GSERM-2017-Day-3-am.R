###############################################
# GSERM 2017 Day Three a.m.
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

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/flintstones.csv")
flintstones<-read.csv(text=temp, header=TRUE)
rm(temp)

# No Barney OR Dino:
summary(lm(Y~X,data=subset(flintstones,name!="Dino" & name!="Barney")))

# No Barney (Dino included):
summary(lm(Y~X,data=subset(flintstones,name!="Barney")))

# Variance:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/LittleDahl.csv")
LittleDahl<-read.csv(text=temp, header=TRUE)
rm(temp)

library(car)
with(LittleDahl, scatterplotMatrix(~age+tenure+unified+nulls))

Fit<-with(LittleDahl, lm(nulls~age+tenure+unified))
summary(Fit)

FitResid<-with(LittleDahl, (nulls - predict(Fit))) # residuals
FitStandard<-rstandard(Fit) # standardized residuals
FitStudent<-rstudent(Fit) # studentized residuals
FitCooksD<-cooks.distance(Fit) # Cook’s D
FitDFBeta<-dfbeta(Fit) # DFBeta
FitDFBetaS<-dfbetas(Fit) # DFBetaS
FitCOVRATIO<-covratio(Fit) # COVRATIOs

FitStudent[74]
LittleDahl$Congress74<-rep(0,length=104)
LittleDahl$Congress74[74]<-1
summary(with(LittleDahl, lm(nulls~age+tenure+unified+Congress74)))

influencePlot(Fit,id.n=4,labels=LittleDahl$Congress,id.cex=0.8,
              id.col="red",xlab="Leverage")

dfbetasPlots(Fit,id.n=5,id.col="red",main="",pch=19)

plot(FitCOVRATIO~names(FitCOVRATIO),pch=19,xlab="Congress",
     ylab="Value of COVRATIO")
abline(h=1,lty=2)

Outlier<-rep(0,104)
Outlier[74]<-1
Outlier[98]<-1
Outlier[104]<-1
DahlSmall<-LittleDahl[which(Outlier==0),]

summary(lm(nulls~age+tenure+unified,data=DahlSmall))

########
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Africa)

Fit <- with(Africa, 
            lm(adrate~gdppppd+muslperc+subsaharan+healthexp+
                 literacy+internalwar))
summary(Fit)

# What not to do:

library(gvlma)
Nope <- gvlma(Fit)
display.gvlmatests(Nope)

# Better:

pdf("DefaultLMPlots.pdf",10,8)
par(mfrow=c(2,3))
plot(Fit,which=c(1:6))
dev.off()

# Unpacking that:
#
# #1: Residuals vs. fitted, same as:

pdf("ResidVsFitted.pdf",7,6)
plot(Fit$residuals~Fit$fitted.values,ylab="Residuals",
     xlab="Fitted Values",main="Residuals vs Fitted")
abline(h=0,lty=2)
lines(lowess(Fit$residuals~Fit$fitted.values),lwd=2,
      col="red")
dev.off()

# #2: QQ plot of residuals:

pdf("ResidQQPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=2)
dev.off()

# #3: Scale-Location plot:

pdf("ScaleLocationPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=3)
dev.off()

# #4: Cook's Distance (D):

pdf("CooksDPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=4)
dev.off()


# #5: Residuals vs. Leverage:

pdf("ResidVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=5)
dev.off()

# #6: Cook's D vs. Leverage:

pdf("CooksDVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=6)
dev.off()

# Another useful plot:

library(car)

ToPlot<-data.frame(Africa$adrate,Fit$fitted.values,
                   Fit$residuals,Africa$gdppppd,Africa$muslperc,
                   Africa$subsaharan,Africa$healthexp,Africa$literacy,
                   Africa$internalwar)

pdf("UsefulPlot.pdf",8,7)
scatterplotMatrix(ToPlot)
dev.off()

# Bootstrapping:

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/Justices.csv")
Justices<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Justices)

OLSfit<-with(Justices, lm(civrts~score))
summary(OLSfit)

WLSfit<-with(Justices, lm(civrts~score,weights=lnNedit))
summary(WLSfit)

pdf("WLSBubblePlotR.pdf",6,6)
par(mar=c(4,4,2,2))
with(Justices, symbols(score, civrts,circles=Neditorials,
                       ylab="Civil Rights Voting",xlab="Segal-Cover Score",
                       ylim=c(0,100)))
abline(reg=OLSfit,lwd=2)
abline(reg=WLSfit,lwd=2,lty=2)
with(Justices, points(score,civrts,pch=20))
legend("topleft",bty="n",lty=c(1,2),lwd=2,
       legend=c("OLS","WLS"))
dev.off()

# "Robust"

library(car)
hccm(OLSfit, type="hc1")

library(rms)
OLSfit2<-ols(civrts~score, x=TRUE, y=TRUE)
RobSEs<-robcov(OLSfit2)
RobSEs

# Bootstrapping

N<-100
reps<-999

set.seed(1337)
X<-rnorm(N)
Y<-2+2*X+rnorm(N)
data<-data.frame(Y,X)
fitOLS<-lm(Y~X)
CI<-confint(fitOLS)

B0<-numeric(reps)
B1<-numeric(reps)

for (i in 1:reps) {
  temp<-data[sample(1:N,N,replace=TRUE),]
  temp.lm<-lm(Y~X,data=temp)
  B0[i]<-temp.lm$coefficients[1]
  B1[i]<-temp.lm$coefficients[2]  
}

ByHandB0<-median(B0)
ByHandB1<-median(B1)
ByHandCI.B0<-quantile(B0,probs=c(0.025,0.975)) # <-- 95% c.i.s
ByHandCI.B1<-quantile(B1,probs=c(0.025,0.975))

# Bootstrap SEs and CIs using boot package

library(boot)

Bs<-function(formula, data, indices) { # <- regression function
  dat <- data[indices,]
  fit <- lm(formula, data=dat)
  return(coef(fit)) 
} 

Boot.fit<-boot(data=data, statistic=Bs, 
               R=reps, formula=Y~X)

BootB0<-median(Boot.fit$t[,1])
BootB1<-median(Boot.fit$t[,2])
BootCI.B0<-boot.ci(Boot.fit,type="basic",index=1)
BootCI.B1<-boot.ci(Boot.fit,type="basic",index=2)

# Same, using the simpleboot package

library(simpleboot)
Simple<-lm.boot(fitOLS,reps)
SimpleB0<-perc(Simple,.50)[1]
SimpleB1<-perc(Simple,.50)[2]
Simple.CIs<-perc(Simple,p=c(0.025,0.975))


######################
# Plot:

pdf("BootstrapSims.pdf",7,7)
par(mar=c(4,4,2,2))
plot(c(1,6),c(fitOLS$coefficients[1],fitOLS$coefficients[2]),
     xlim=c(0,10),ylim=c(1.5,2.5),xlab="Parameter",ylab="Estimate",
     xaxt="n",pch=19,col="black")
abline(h=2,lwd=1,lty=2)
axis(1,at=c(2.5,7.5),labels=c("Intercept","Slope"))
points(c(2,7),c(ByHandB0,ByHandB1),pch=4,col="red")
points(c(3,8),c(BootB0,BootB1),pch=17,col="blue")
points(c(4,9),c(SimpleB0,SimpleB1),pch=15,col="darkgreen")
segments(1,CI[1,1],1,CI[1,2],col="black",lwd=2)
segments(6,CI[2,1],6,CI[2,2],col="black",lwd=2)
segments(2,ByHandCI.B0[1],2,ByHandCI.B0[2],col="red",lwd=2)
segments(7,ByHandCI.B1[1],7,ByHandCI.B1[2],col="red",lwd=2)
segments(3,BootCI.B0$basic[4],3,BootCI.B0$basic[5],col="blue",lwd=2)
segments(8,BootCI.B1$basic[4],8,BootCI.B1$basic[5],col="blue",lwd=2)
segments(4,Simple.CIs[1,1],4,Simple.CIs[2,1],col="darkgreen",lwd=2)
segments(9,Simple.CIs[1,2],9,Simple.CIs[2,2],col="darkgreen",lwd=2)
legend("topright",c("OLS","By-Hand","boot","simpleboot"),bty="n",
       pch=c(19,4,17,15),col=c("black","red","blue","darkgreen"))
dev.off()

################################################
################################################
# Second set of sims (non-normal errors):
################################################
################################################

rm(list=ls())

N<-100
reps<-999

set.seed(1337)
X<-rnorm(N)
ustar<-rchisq(N,2) # <- skewed residuals
Y<-2+2*X+(ustar-mean(ustar))
data<-data.frame(Y,X)
fitOLS<-lm(Y~X)
CI<-confint(fitOLS)

pdf("BootScatter2.pdf",5,5)
plot(X,Y,pch=19)
dev.off()

# Bootstrap SEs and CIs "by hand"

B0<-numeric(reps)
B1<-numeric(reps)

for (i in 1:reps) {
  temp<-data[sample(1:N,N,replace=TRUE),]
  temp.lm<-lm(Y~X,data=temp)
  B0[i]<-temp.lm$coefficients[1]
  B1[i]<-temp.lm$coefficients[2]  
}

ByHandB0<-median(B0)
ByHandB1<-median(B1)
ByHandCI.B0<-quantile(B0,probs=c(0.025,0.975))
ByHandCI.B1<-quantile(B1,probs=c(0.025,0.975))

# Bootstrap SEs and CIs using boot package

library(boot)

Bs<-function(formula, data, indices) { # <- regression function
  dat <- data[indices,]
  fit <- lm(formula, data=dat)
  return(coef(fit)) 
} 

Boot.fit<-boot(data=data, statistic=Bs, 
               R=reps, formula=Y~X)

BootB0<-median(Boot.fit$t[,1])
BootB1<-median(Boot.fit$t[,2])
BootCI.B0<-boot.ci(Boot.fit,type="basic",index=1)
BootCI.B1<-boot.ci(Boot.fit,type="basic",index=2)

# Same, using the simpleboot package

library(simpleboot)
Simple<-lm.boot(fitOLS,reps)
SimpleB0<-perc(Simple,.50)[1]
SimpleB1<-perc(Simple,.50)[2]
Simple.CIs<-perc(Simple,p=c(0.025,0.975))


######################
# Plot:

pdf("BootstrapSims2.pdf",7,7)
par(mar=c(4,4,2,2))
plot(c(1,6),c(fitOLS$coefficients[1],fitOLS$coefficients[2]),
     xlim=c(0,10),ylim=c(1,3),
     xlab="Parameter",ylab="Estimate",
     xaxt="n",pch=19,col="black")
abline(h=2,lwd=1,lty=2)
axis(1,at=c(2.5,7.5),labels=c("Intercept","Slope"))
points(c(2,7),c(ByHandB0,ByHandB1),pch=4,col="red")
points(c(3,8),c(BootB0,BootB1),pch=17,col="blue")
points(c(4,9),c(SimpleB0,SimpleB1),pch=15,col="darkgreen")
segments(1,CI[1,1],1,CI[1,2],col="black",lwd=2)
segments(6,CI[2,1],6,CI[2,2],col="black",lwd=2)
segments(2,ByHandCI.B0[1],2,ByHandCI.B0[2],col="red",lwd=2)
segments(7,ByHandCI.B1[1],7,ByHandCI.B1[2],col="red",lwd=2)
segments(3,BootCI.B0$basic[4],3,BootCI.B0$basic[5],col="blue",lwd=2)
segments(8,BootCI.B1$basic[4],8,BootCI.B1$basic[5],col="blue",lwd=2)
segments(4,Simple.CIs[1,1],4,Simple.CIs[2,1],col="darkgreen",lwd=2)
segments(9,Simple.CIs[1,2],9,Simple.CIs[2,2],col="darkgreen",lwd=2)
legend("topright",c("OLS","By-Hand","boot","simpleboot"),bty="n",
       pch=c(19,4,17,15),col=c("black","red","blue","darkgreen"))
dev.off()


