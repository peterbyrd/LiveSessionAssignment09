# Author: Peter Byrd
# Exploratory data analysis

# install packages
install.packages("ggplot2")
install.packages("doBy")
install.packages("plotly")
library(ggplot2)
library("doBy")
library("plotly")
library("reshape2")

# clean and categorize 
chf[chf$Age == 1, "AgeGrp"] = "less than 25"
chf[chf$Age == 2, "AgeGrp"] = "25-44"
chf[chf$Age == 3, "AgeGrp"] = "45-64"
chf[chf$Age == 4, "AgeGrp"] = "65-69"
chf[chf$Age == 5, "AgeGrp"] = "70-74"
chf[chf$Age == 6, "AgeGrp"] = "75-79"
chf[chf$Age == 7, "AgeGrp"] = "80-84"
chf[chf$Age == 8, "AgeGrp"] = "85-89"
chf[chf$Age == 9, "AgeGrp"] = "90 and over"

chf[chf$Sex == 0, "Sex"] = 'Unknown'
chf[chf$Sex == 1, "Sex"] = 'Male'
chf[chf$Sex == 2, "Sex"] = "Female"

chf$Sex <- factor(chf$Sex)
chf$drgcode <- factor(chf$drgcode)

# view
head(chf)
summary(chf)
str(chf)

# mean amount paid by medicare, total accomodation charges, and total department charges
mean.amtreim <- format(mean(chf$AmtReim), big.mark=",",big.interval=3,digits=2,nsmall=2)
mean.TotAccomChg <- format(mean(chf$TotAccomChg), big.mark=",",big.interval=3,digits=2,nsmall=2)
mean.TotDeptChg <- format(mean(chf$TotDeptChg), big.mark=",",big.interval=3,digits=2,nsmall=2)

# standard deviation 
std.amtreim <- format(sd(chf$AmtReim), big.mark=",",big.interval=3,digits=2,nsmall=2)
std.TotAccomChg <- format(sd(chf$TotAccomChg),big.mark=",",big.interval=3,digits=2,nsmall=2)
std.TotDeptChg <- format(sd(chf$TotDeptChg),big.mark=",",big.interval=3,digits=2,nsmall=2)

# use tapply()
meanbysex.amtreim <- format(tapply(chf$AmtReim,chf$Sex,mean),big.mark=",",big.interval=3,digits=2,nsmall=2)
meanbysex.TotAccomChg <- format(tapply(chf$TotAccomChg,chf$Sex,mean),big.mark=",",big.interval=3,digits=2,nsmall=2)
meanbysex.TotDeptChg <- format(tapply(chf$TotDeptChg,chf$Sex,mean),big.mark=",",big.interval=3,digits=2,nsmall=2)

# boxplot
chf.m <- melt(chf,measure=c("AmtReim","TotAccomChg","TotDeptChg"),variable.name="Finance")
ggplot(chf.m,aes(Finance,value)) +geom_boxplot(aes(fill=Sex))

ggplot(chf,aes(Sex,AmtReim,fill=Sex)) +geom_boxplot()
ggplot(chf,aes(Sex,TotAccomChg,fill=Sex)) +geom_boxplot()
ggplot(chf,aes(Sex,TotDeptChg,fill=Sex)) +geom_boxplot()

# histogram
ggplot(chf,aes(TotAccomChg,fill=Sex)) +geom_histogram(binwidth=10000,alpha=.6)
ggplot(chf,aes(Sex,fill=Sex)) +geom_bar(alpha=.7)
ggplot(chf,aes(AgeGrp,fill=AgeGrp)) +geom_bar(alpha=.7)

# bar plot of gender count by admission source
ggplot(chf,aes(admsrc,fill=Sex)) +geom_bar(alpha=.7,position="dodge")

# bar plot of mortality rate for MS-DRG 292 and MS-DRG 293
mortality <- subset(chf,drgcode==292|drgcode==293,select=c(drgcode,dischdest))
mortality[mortality$dischdest == 20, "Expired"] = 1
mortality[mortality$dischdest != 20, "Expired"] = 0
summaryBy(drgcode+Expired~drgcode,data=mortality,FUN=sum)

ggplot(mortality,aes(drgcode)) +geom_bar()

ggplot(mortality,aes(drgcode)) +geom_bar(alpha=.7)
ggplot(subset(chf,drgcode==292|drgcode==293),aes(dischdest,(dischdest==20/length(drgcode)))) +geom_bar(alpha=.7)

#drg292 <- subset(chf,drgcode==292,select=dischdest)
#drg293 <- subset(chf,drgcode==293,select=dischdest)
#drg292mort <- subset(drg292,dischdest==20,select=dischdest)
#drg293mort <- subset(drg293,dischdest==20,select=dischdest)
#numerator <- c(count(drg292mort),count(drg293mort))
#denominator <- c(count(drg292),count(drg293))
#mortality$mort <- c(count(drg292mort)/count(drg292),count(drg293mort)/count(drg293))


# test LOS for patients with MS-DRG 291 versus national average of 6 days
# Ho: MS-DRG 291 LOS = 6 days
# Ha: MS-DRG 291 LOS <> 6 days
t.test(subset(chf,drgcode==291,select=LOS),alternative="two.sided",mu=6,conf.level=0.95)
