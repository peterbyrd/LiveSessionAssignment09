# Author: Peter Byrd
# Exploratory data analysis

# mean amount paid by medicare, total accomodation charges, and total department charges
mean.amtreim <- format(mean(chf$AmtReim), big.mark=",",big.interval=3,digits=2,nsmall=2)
mean.TotAccomChg <- format(mean(chf$TotAccomChg), big.mark=",",big.interval=3,digits=2,nsmall=2)
mean.TotDeptChg <- format(mean(chf$TotDeptChg), big.mark=",",big.interval=3,digits=2,nsmall=2)

# standard deviation 
std.amtreim <- format(sd(chf$AmtReim), big.mark=",",big.interval=3,digits=2,nsmall=2)
std.TotAccomChg <- format(sd(chf$TotAccomChg),big.mark=",",big.interval=3,digits=2,nsmall=2)
std.TotDeptChg <- format(sd(chf$TotDeptChg),big.mark=",",big.interval=3,digits=2,nsmall=2)

# use tapply()
meanbysex.amtreim <- format(tapply(chf$AmtReim,chf$Sex2,mean),big.mark=",",big.interval=3,digits=2,nsmall=2)
meanbysex.TotAccomChg <- format(tapply(chf$TotAccomChg,chf$Sex2,mean),big.mark=",",big.interval=3,digits=2,nsmall=2)
meanbysex.TotDeptChg <- format(tapply(chf$TotDeptChg,chf$Sex2,mean),big.mark=",",big.interval=3,digits=2,nsmall=2)

# boxplot
chf.m <- melt(chf,measure=c("AmtReim","TotAccomChg","TotDeptChg"),variable.name="Finance")
ggplot(chf.m,aes(Finance,value)) +geom_boxplot(aes(fill=Sex2))

# individual plots (not shown)
# ggplot(chf,aes(Sex,AmtReim,fill=Sex)) +geom_boxplot()
# ggplot(chf,aes(Sex,TotAccomChg,fill=Sex)) +geom_boxplot()
# ggplot(chf,aes(Sex,TotDeptChg,fill=Sex)) +geom_boxplot()

# histogram
ggplot(chf,aes(TotAccomChg,fill=Sex2)) +geom_histogram(binwidth=10000,alpha=.6)
ggplot(chf,aes(Sex,fill=Sex2)) +geom_histogram(binwidth=1,alpha=.7)
ggplot(chf,aes(Age,fill=AgeGrp)) +geom_histogram(binwidth=1,alpha=.7)

# bar plot of gender count by admission source
ggplot(chf,aes(admsrc,fill=Sex2)) +geom_bar(alpha=.7,position="dodge")

# bar plot of mortality rate for MS-DRG 292 and MS-DRG 293
mortrate292 <- subset(chf,drgcode==292, select=c(drgcode,Expired))
mortrate293 <- subset(chf,drgcode==293, select=c(drgcode,Expired))

m292 <- nrow(mortrate292[mortrate292$Expired==1,])/nrow(mortrate292)
m293 <- nrow(mortrate293[mortrate293$Expired==1,])/nrow(mortrate293)

x <- c('MS-DRG 292','MS-DRG 293')
y <- c(m292, m293)
data <- data.frame(x,y)

plot_ly(data, x=x, y=y, type='bar') %>% layout(title="Mortality Rate")

# test LOS for patients with MS-DRG 291 versus national average of 6 days
#     Ho: MS-DRG 291 LOS = 6 days
#     Ha: MS-DRG 291 LOS <> 6 days
t.test(subset(chf,drgcode==291,select=LOS),alternative="two.sided",mu=6,conf.level=0.95)
