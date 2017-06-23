###############################################
# GSERM 2017 Day Two a.m.
#
# File created June 20, 2017
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

# Simulations for non-normal residuals (this
# did not appear in the notes/slides):

sims <- 1000
set.seed(7222009)
N <- 5  # sample size; change as needed...

OKTs <- numeric(sims)
BadTs <- numeric(sims)

for (i in 1:sims) {
  u <- rnorm(N,0,2) # mean zero, s.d = 2
  # Exponentiate:
  eu <- exp(u)
  eu <- eu-mean(eu) # new residuals are mean-zero
  eu <- (eu/sd(eu))*2 # and also sd = 2
  
  # Generate Ys:
  
  X <- runif(N,-6,6)
  Y1 <- 0 + 1*X + 1*u 
  Y2 <- 0 + 1*X + 1*eu # same Xs in both
  
  fit1 <- lm(Y1~X)
  fit2 <- lm(Y2~X)
  
  OKTs[i] <- summary(fit1)$coefficients[2,3]
  BadTs[i] <- summary(fit2)$coefficients[2,3]
} 

# Plot...

par(mar=c(4,4,2,2))
plot(OKTs,BadTs,pch=20,xlab="T-statistics: Normal residuals",
     ylab="T-statistics: Skewed residuals")
abline(a=0,b=1,lwd=2,col="black")
abline(lm(BadTs~OKTs),lwd=2,col="red",lty=2)
legend("topleft",bty="n",lwd=c(2,2),col=c("black","red"),
       lty=c(1,2),legend=c("45-degree line","OLS fit"))

#############################################
# Real-Data Example:

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/fordham.csv")
Data<-read.csv(text=temp, header=TRUE)
rm(temp)

with(Data, summary(milgdp))
with(Data, summary(gdp))

# Ladder Plots in R:

par(mfrow=c(3,3))
with(Data, plot(density(gdp^3,na.rm=TRUE),main="Cubic"))
with(Data, plot(density(gdp^2,na.rm=TRUE),main="Square"))
with(Data, plot(density(gdp,na.rm=TRUE),main="Identity"))
with(Data, plot(density(sqrt(gdp),na.rm=TRUE),main="Square Root"))
with(Data, plot(density(log(gdp),na.rm=TRUE),main="Log"))
with(Data, plot(density(1/sqrt(gdp),na.rm=TRUE),main="1 / Square Root"))
with(Data, plot(density(1/gdp,na.rm=TRUE),main="Inverse"))
with(Data, plot(density(1/gdp^2,na.rm=TRUE),main="1 / Square"))
with(Data, plot(density(1/gdp^3,na.rm=TRUE),main="1 / Cubic"))

with(Data, plot(density(milgdp^3,na.rm=TRUE),main="Cubic"))
with(Data, plot(density(milgdp^2,na.rm=TRUE),main="Square"))
with(Data, plot(density(milgdp,na.rm=TRUE),main="Identity"))
with(Data, plot(density(sqrt(milgdp),na.rm=TRUE),main="Square Root"))
with(Data, plot(density(log(milgdp),na.rm=TRUE),main="Log"))
with(Data, plot(density(1/sqrt(milgdp),na.rm=TRUE),main="1 / Square Root"))
with(Data, plot(density(1/milgdp,na.rm=TRUE),main="Inverse"))
with(Data, plot(density(1/milgdp^2,na.rm=TRUE),main="1 / Square"))
with(Data, plot(density(1/milgdp^3,na.rm=TRUE),main="1 / Cubic"))

# Other plots:

par(mfrow=c(2,2))
with(Data, plot(gdp,milgdp))
with(Data, plot(log(gdp),milgdp))
with(Data, plot(gdp,log(milgdp)))
with(Data, plot(log(gdp),log(milgdp)))

# Regressions:

linlin <- with(Data, lm(milgdp~gdp))
summary(linlin)
linlog <- with(Data, lm(milgdp~log(gdp)))
summary(linlog)
loglin <- with(Data, lm(log(milgdp+0.01)~gdp))
summary(loglin)
loglog <- with(Data, lm(log(milgdp+0.01)~log(gdp)))
summary(loglog)

# Residual plot:

pdf("MilSpendGDPResidsDensities-R.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
plot(density(linlog$residuals),xlim=c(-5,10),lwd=2,lty=3,
     col="blue",main="",xlab="Residual Values")
lines(density(linlin$residuals),lwd=1,lty=1,col="black")
lines(density(loglin$residuals),lwd=2,lty=1,col="red")
lines(density(loglog$residuals),lwd=4,lty=1,col="darkgreen")
abline(v=0,lty=2,lwd=1)
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4),
       col=c("black","red","blue","darkgreen"),
       legend=c("Untransformed","Logged Y","Logged X",
                "Both Logged"))
dev.off()
