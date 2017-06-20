##############################################
# This is some code from Regression III,
# Day 2, afternoon session
#
# File created June 19, 2017
#
# File last updated June 20, 2017
###############################################
# Set working directory as necessary:
#
# setwd("~/Dropbox (Personal)/GSERM/Materials 2017/Notes and Slides")
#
###############################################
# Options:
#
options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places
###############################################
#
# Piecewise Regression plot:

c <- 4
n <- 200
set.seed(7222009)
x <- runif(n,-10,10)
y <- 2*x + rnorm(n,0,3)
y <- ifelse(x > c,15+(-2*x)+rnorm(n,0,3),y)
df <- data.frame(x=x,y=y,qhat=qhat)
qfit <- with(df, lm(y~x+I(x^2))) # quadratic
qhat <- fitted.values(qfit) # hats
df$xL <- ifelse(df$x < c,c-x,0)
df$xH <- ifelse(df$x > c,x-c,0)
pfit <- with(df, lm(y~xL+xH))
df$phat <- pfit$fitted.values
df <- df[order(df$x),] # sort

pdf("Piecewise1.pdf",4,5)
par(mar=c(4,4,2,2))
with(df, plot(x,y,pch=19,xlab="X",ylab="Y"))
dev.off()

pdf("Piecewise2.pdf",4,5)
par(mar=c(4,4,2,2))
with(df, plot(x,y,pch=19,xlab="X",ylab="Y"))
with(df, lines(x,qhat,col="red",lty=2,lwd=3))
with(df, lines(x,phat,lwd=3))
legend("topright",bty="n",col=c("black","red"),lwd=3,lty=c(1,2),
       legend=c("Piecewise Linear","Quadratic"),cex=0.5)
abline(v=c,lty=2,lwd=1)
dev.off()

# Quadratic splines:

df$xL2 <- df$xL^2
df$xH2 <- df$xH^2

p2fit <- with(df, lm(y~x+xL2+xH2))
df$p2hat <- p2fit$fitted.values

pdf("Piecewise3.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(x,y,pch=19,xlab="X",ylab="Y"))
with(df, lines(x,qhat,col="red",lty=2,lwd=3))
with(df, lines(x,p2hat,lwd=3))
legend("topleft",bty="n",col=c("black","red"),lwd=3,lty=c(1,2),
       legend=c("Quadratic Spline","Quadratic"),cex=0.7)
abline(v=c,lty=2,lwd=1)
dev.off()

# # Cubic spline fit:
# 
# df$xL3 <- df$xL^3
# df$xH3 <- df$xH^3
# 
# p3fit <- with(df, lm(y~x+I(x^2)+xL3+xH3))
# df$p3hat <- p3fit$fitted.values
# 
# pdf("Piecewise4.pdf",6,5)
# par(mar=c(4,4,2,2))
# with(df, plot(x,y,pch=19,xlab="X",ylab="Y"))
# with(df, lines(x,qhat,col="red",lty=2,lwd=3))
# with(df, lines(x,p3hat,lwd=3))
# legend("topleft",bty="n",col=c("black","red"),lwd=3,lty=c(1,2),
#        legend=c("Cubic Spline","Quadratic"),cex=0.7)
# abline(v=c,lty=2,lwd=1)
# dev.off()

# Smoothing splines...
# 
# Create an X-Y based on a sin curve:

N<-500
set.seed(7222009)
u <-runif(N,-2,2)
X <-runif(N,-4,4)
Ys<-sin(X)+2*abs(cos(-1*X))
Y <-Ys+u
df<-data.frame(X=X,Ys=Ys,Y=Y)
df<-df[order(df$X),]

pdf("SSplines1.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=19,cex=0.5))
with(df, lines(X,Ys,lwd=3))
dev.off()

# Load library:

require(pspline)

# Lambda illustration:

pdf("SSplines2.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=19,cex=0.5))
with(df, lines(sm.spline(X, Y, spar = 25), lwd=3, col="red",lty=3))
with(df, lines(sm.spline(X, Y, spar = 5), lwd=3, col="darkgreen",lty=2))
with(df, lines(sm.spline(X, Y, spar = 0), lwd=3, col="blue",lty=1))
legend("bottomright",bty="n",col=c("red","darkgreen","blue"),
       lwd=3,lty=c(3,2,1),legend=c("spar = 25","spar = 5","spar = 0"))
dev.off()

# Degrees of freedom illustration:

pdf("SSplines3.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=19,cex=0.5))
with(df, lines(sm.spline(X, Y, df=2), lwd=3, lty=3, col="red"))
with(df, lines(sm.spline(X, Y, df = 5), lwd=3, col="darkgreen",lty=2))
with(df, lines(sm.spline(X, Y, df = 25), lwd=3, col="blue",lty=1))
legend("bottomright",bty="n",col=c("red","darkgreen","blue"),
       lwd=3,lty=c(3,2,1),legend=c("df = 2","df = 5","df = 25"))
dev.off()

####################################
# Additive models

require(mgcv)

# Simulated data:

df$Z <- -df$X + runif(N,-4,4)
df$Y2 <- sin(df$X)+2*abs(cos(-1*df$X)) + 0.1*df$Z^2 + u

with(df, plot(Z,Y2))

simfit <- gam(Y2 ~ s(X,bs="cr") 
                 + s(Z,bs="cr"),data=df)

# Plots:

pdf("Add1.pdf",6,5)
par(mar=c(4,4,2,2))
vis.gam(simfit, color="terrain", plot.type="contour",
        main=" ")
dev.off()

pdf("Add2.pdf",6,5)
par(mar=c(4,4,2,2))
vis.gam(simfit, color="terrain", plot.type="persp",
        main=" ",theta=-45)
dev.off()

pdf("Add3.pdf",6,5)
par(mar=c(4,4,2,2))
vis.gam(simfit, color="terrain", plot.type="persp",
        theta=-45,se=2.58)
dev.off()