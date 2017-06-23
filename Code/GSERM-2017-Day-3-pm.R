###############################################
# GSERM 2017 Day Three p.m.
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
library(RColorBrewer)
library(colorspace)
library(foreign)
library(psych)
library(lme4)
library(plm)
library(gtools)
library(boot)
library(plyr)
library(texreg)
library(statmod)
library(pscl)

#####################
# Tiny TSCS example:

tiny <- read.table("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/tinyTSCSexample.txt",header=TRUE)
tiny

aggXS <- ddply(tiny, .(ID), summarise,
               Female = Female[1],
               Approve = mean(Approve))

aggT <- ddply(tiny, .(Year), summarise,
              pres=PresVote[1],
              approve=mean(Approve))

# SCOTUS tenure data:

scotusURL<-"https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/scotus.csv"
temp<-getURL(scotusURL)
scotus<-read.csv(textConnection(temp))
rm(temp)

with(scotus, describe(service)) # all variation

scmeans <- ddply(scotus,.(justice),summarise,
                 service = mean(service))

with(scmeans, describe(service)) # "between" variation

scotus <- ddply(scotus, .(justice), mutate,
                servmean = mean(service))
scotus$within <- with(scotus, service-servmean)

with(scotus, describe(within))

# Varying intercepts / slopes figures:
#
# ... nah.

##########
# FEs and REs:

RefsURL<-"https://raw.githubusercontent.com/PrisonRodeo/GSERM-2017-git/master/Data/Refugees.csv"
temp<-getURL(RefsURL)
Refugees<-read.csv(textConnection(temp))
rm(temp, RefsURL)

summary(Refugees)

RefFE<-plm(ln_ref_flow~pop_diff+distance+regimedif+
             wardiff, data=Refugees, effect="individual",
           model="within")

RefBE<-plm(ln_ref_flow~pop_diff+distance+regimedif+
             wardiff, data=Refugees, effect="individual",
           model="between")

RefRE<-lmer(ln_ref_flow~pop_diff+distance+regimedif+
              wardiff+(1|dirdyadID), data=Refugees)

AltRefRE<-plm(ln_ref_flow~pop_diff+distance+regimedif+
                wardiff, data=Refugees, effect="individual",
              model="random")

phtest(RefFE, AltRefRE)

