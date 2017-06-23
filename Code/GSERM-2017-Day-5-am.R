###############################################
# GSERM 2017 Day Five a.m.
#
# File created June 23, 2017
#
# File last updated June 23, 2017
###############################################
# Set working directory as necessary:
#
setwd("~/Dropbox (Personal)/GSERM/Materials 2017/Notes and Slides")
#

require(RCurl)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

###########################
# "Toy" example:

set.seed(7222009)
ystar<-rnorm(100)
y<-ifelse(ystar>0,1,0)
x<-ystar+(0.5*rnorm(100))
data<-data.frame(ystar,y,x)
head(data)

pdf("YstarYX-R.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,ystar,pch=19,ylab="Y* / Y",xlab="X")
points(x,y,pch=4,col="red")
abline(h=0)
legend("topleft",bty="n",pch=c(19,4),col=c("black","red"),
       legend=c("Y*","Y"))
dev.off()

# probits and logits...

myprobit<-glm(y~x,family=binomial(link="probit"),
              data=data)
summary(myprobit)

mylogit<-glm(y~x,family=binomial(link="logit"),
             data=data)
summary(mylogit)

pdf("LogitProbitHats.pdf",6,5)
plot(mylogit$fitted.values,myprobit$fitted.values,
     pch=20,xlab="Logit Predictions",
     ylab="Probit Predictions")
dev.off()

#################################
# NAFTA example...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/NAFTA.csv")
NAFTA<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(NAFTA)

# Logit:

NAFTA.GLM.fit<-glm(vote~democrat+pcthispc+cope93+DemXCOPE,
                   NAFTA,family=binomial)
summary(NAFTA.GLM.fit)

# Interactions...

NAFTA.GLM.fit$coeff[4]+NAFTA.GLM.fit$coeff[5]
(NAFTA.GLM.fit$coeff[4]+NAFTA.GLM.fit$coeff[5]) / 
  (sqrt(vcov(NAFTA.GLM.fit)[4,4] + 
  (1)^2*vcov(NAFTA.GLM.fit)[5,5] + 
  2*1*vcov(NAFTA.GLM.fit)[4,5]))

# Same thing, using -car-:

library(car)
linear.hypothesis(NAFTA.GLM.fit,"cope93+DemXCOPE=0")

# Predicted values:

preds<-NAFTA.GLM.fit$fitted.values
hats<-predict(NAFTA.GLM.fit,se.fit=TRUE)

# Plotting in-sample predictions:

XBUB<-hats$fit + (1.96*hats$se.fit) 
XBLB<-hats$fit - (1.96*hats$se.fit)
plotdata<-cbind(as.data.frame(hats),XBUB,XBLB)
plotdata<-data.frame(lapply(plotdata,binomial(link="logit")$linkinv))
par(mfrow=c(1,2))
library(plotrix)
plotCI(cope93[democrat==1],plotdata$fit[democrat==1],ui=plotdata$XBUB[democrat==1],
         li=plotdata$XBLB[democrat==1],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)")
plotCI(cope93[democrat==0],plotdata$fit[democrat==0],ui=plotdata$XBUB[democrat==0],
         li=plotdata$XBLB[democrat==0],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)")

# Plotting Out-of-sample Predictions:

sim.data<-data.frame(pcthispc=mean(nafta$pcthispc),democrat=rep(0:1,101),
                       cope93=seq(from=0,to=100,length.out=101))
sim.data$DemXCOPE<-sim.data$democrat*sim.data$cope93

OutHats<-predict(NAFTA.GLM.fit,se.fit=TRUE,newdata=sim.data)
OutHatsUB<-OutHats$fit+(1.96*OutHats$se.fit)
OutHatsLB<-OutHats$fit-(1.96*OutHats$se.fit)
OutHats<-cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats<-data.frame(lapply(OutHats,binomial(link="logit")$linkinv))

par(mfrow=c(1,2))
both<-cbind(sim.data,OutHats)
both<-both[order(both$cope93,both$democrat),]

plot(both$cope93[democrat==1],both$fit[democrat==1],t="l",lwd=2,ylim=c(0,1),
       xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(both$cope93[democrat==1],both$OutHatsUB[democrat==1],lty=2)
lines(both$cope93[democrat==1],both$OutHatsLB[democrat==1],lty=2)
text(locator(1),label="Democrats")

plot(both$cope93[democrat==0],both$fit[democrat==0],t="l",lwd=2,ylim=c(0,1),
       xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(both$cope93[democrat==0],both$OutHatsUB[democrat==0],lty=2)
lines(both$cope93[democrat==0],both$OutHatsLB[democrat==0],lty=2)
text(locator(1),label="Republicans")

# Odds Ratios:

lreg.or <- function(model)
       {
        coeffs <- coef(summary(NAFTA.GLM.fit))
        lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2])
        or <- exp(coeffs[ ,1])
        uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2])
        lreg.or <- cbind(lci, or, uci)        
        lreg.or
        }

lreg.or(NAFTA.GLM.fit)

####################
# Goodness of fit:

table(NAFTA.GLM.fit$fitted.values>0.5,nafta$vote==1)
chisq.test(NAFTA.GLM.fit$fitted.values>0.5,nafta$vote==1)

# ROC curves, plots, etc.:

library(ROCR)
NAFTA.GLM.logithats<-predict(NAFTA.GLM.fit,
                       type="response")
preds<-prediction(NAFTA.GLM.logithats,NAFTA$vote)
plot(performance(preds,"tpr","fpr"),lwd=2,lty=2,
       col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)

###############################
# Event counts...
#
# Various Poisson histograms

set.seed(7222009)
N<-1000
LP05<-rpois(N,0.5)
LP1<-rpois(N,1)
LP5<-rpois(N,5)
LP10<-rpois(N,10)

pdf("PoissonHistogramsR.pdf",7,6)
par(mfrow=c(2,2))
hist(LP05,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 0.5")
hist(LP1,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 1.0")
hist(LP5,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 5")
hist(LP10,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 10")
dev.off()

# Get SCOTUS nullifications data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/nulls.csv")
Nulls<-read.csv(text=temp, header=TRUE)
rm(temp)

# Histogram:

pdf("NullsHist.pdf",6,5)
par(mar=c(4,4,2,2))
with(Nulls, 
     hist(nulls,main="",xlab="Number of Nullifications",
          col="grey"))
dev.off()

# Poisson regression:

nulls.poisson<-glm(nulls~tenure+unified,family="poisson",
                   data=Nulls)
summary(nulls.poisson)

# IRRs:

library(mfx)
nulls.poisson.IRR<-poissonirr(nulls~tenure+unified,
                              data=Nulls)
nulls.poisson.IRR

# Predictions:

tenure<-seq(0,20,1)
unified<-1
simdata<-as.data.frame(cbind(tenure,unified))
nullhats<-predict(nulls.poisson,newdata=simdata,se.fit=TRUE)

# NOTE: These are XBs, not predicted counts.
# Transforming:

nullhats$Yhat<-exp(nullhats$fit)
nullhats$UB<-exp(nullhats$fit + 1.96*(nullhats$se.fit))
nullhats$LB<-exp(nullhats$fit - 1.96*(nullhats$se.fit))

# Plot...

pdf("NullsOutOfSampleHatsR.pdf",6,5)
plot(simdata$tenure,nullhats$Yhat,t="l",lwd=3,ylim=c(0,5),ylab=
       "Predicted Count", xlab="Mean Tenure")
lines(simdata$tenure,nullhats$UB,lwd=2,lty=2)
lines(simdata$tenure,nullhats$LB,lwd=2,lty=2)
dev.off()

# Offsets with dyadic data...Aggregated counts
# of conflicts between the countries in each
# dyad, 1950-1985...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/offsetIR.csv")
IR<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(IR)

cor(IR,use="complete.obs")

IR.fit1<-glm(disputes~allies+openness,data=IR,family="poisson")
summary(IR.fit1)

IR.fit2<-glm(disputes~allies+openness,data=IR,family="poisson",
             offset=log(Ndyads))
summary(IR.fit2)

IR.fit3<-glm(disputes~allies+openness+log(Ndyads),data=IR,
             family="poisson")
summary(IR.fit3)

# z-test:
2*pnorm((0.811-1)/.071)

# Wald test:
wald.test(b=coef(IR.fit3),Sigma=vcov(IR.fit3),Terms=4,H0=1)
