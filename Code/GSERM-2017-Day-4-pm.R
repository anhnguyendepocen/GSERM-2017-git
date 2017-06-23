###############################################
# GSERM 2017 Day Four p.m.
#
# File created June 21, 2017
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
require(foreign)

# Toy example

X<-c(1,1,2,2,3,3,4,4,5,5)
Y<-c(0,2,1,3,2,4,3,5,4,6)

linmod<-lm(y~x)
summary(linmod)
linglm<-glm(y~x,family="gaussian")
summary(linglm)

# 2008 NES Data

NES08<-read.dta("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/NES2008.dta",
                convert.factors=FALSE)
summary(NES08[,4:16])

pdf("PolKnowledge.pdf",4,3)
par(mar=c(4,4,2,2))
dotchart(table(NES08$knowledge),pch=20,lcolor="grey",
         xlab="Frequency",xlim=c(0,800),
         ylab="Knowledge Score")
dev.off()

nes08.binom<-glm(cbind(knowledge,4-knowledge)~age+female+white+
                   oftenvote+conservative+prayfreq+heterosexual+married+
                   yrsofschool+income,data=NES08,family=binomial)
summary(nes08.binom)


