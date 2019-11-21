## Prepare plots for report

## Before: catage.csv, wcatch.csv effort.csv (data)
#          summary.out (model)
## After:  catage.png, wcatch.png all_effort.png cpue_effort.png
#           effort_fbar.png all_effort_Fbar.png  catch_residuals_1.png 
#           catch_residuals_1.png (report)


#NOTES: Why is Fbar diferent between summary out tables?

library(icesTAF)
library(ggplot2)
library(tidyr)
library(lattice)
source("utilities_sms.R")

mkdir("report")

# The years to be included on X-axis of figures (5 years between each)
Yearlabs=c(1983,1988,1993,1998,2003,2008,2013,2018)

#For use later in the script
years=read.csv("./data/effort.csv")[,1] 


## 1  Data

taf.png("catage")
par(mar=c(5, 4, 4, 6)+0.1,xpd=TRUE)

catage <- read.taf("data/catage.csv")
catage <- aggregate(cbind(`0`,`1`,`2`,`3`,`4+`)~Year, catage, sum)
catage <- prop.table(as.matrix(taf2xtab(catage)), 1)
barplot(t(rev(as.data.frame(catage))), ylim=0:1,
        xlab="Year", ylab="Proportion at age",
        col=c("violet","deepskyblue","seagreen","khaki4","lightcoral"))

legend(legend = colnames(catage),
       title = "Age",
       fill = rev(c("violet","deepskyblue","seagreen","khaki4","lightcoral")),
       x="right",inset=c(-0.15,0))

box()
dev.off()
##################################
taf.png("catage2")
catage <-read.csv("data/catage.csv")
catage <- aggregate(cbind(X0,X1,X2,X3,X4.)~Year,data = catage,sum)
catprop<-as.data.frame(catage[,-1],row.names = catage$Year)
catprop<-as.data.frame(prop.table(as.matrix(catprop),1))

Ages<-c("0","1","2","3","4+")
catprop<-as.data.frame(cbind(rownames(catprop),catprop),row.names = 1:nrow(catprop))
colnames(catprop)<-c("Year",Ages)

catprop<-rev(catprop)
catprop_long<-pivot_longer(catprop,cols = 1:5,names_to = "Age",values_to = "Fmort")


GP<-ggplot(catprop_long, aes(x=Year, y=Fmort))+geom_col(aes(fill=Age),col="black",size=0.2 )+
  ylab("Proportion at age")+ylim(c(0,1.0000001))+
  scale_x_discrete(name="Year", breaks=Yearlabs,labels = Yearlabs)+theme(text = element_text(size=35))

print(GP)
dev.off()
################################

taf.png("wcatch",width = 663,height = 435)
wcatch <- read.taf("data/wcatch.csv")
wcatch <- step2long(wcatch)
wcatch$Label <- factor(wcatch$Step,
                       labels=c("First half year", "Second half year"))


GP<- ggplot(wcatch[!wcatch$Age=="0" | !wcatch$Step==1,],aes(x = Year,y=Value,col=Age))+
   geom_line()+geom_point(cex=0.3,pch=3)+ylab("Mean Weight (g)")+
   facet_wrap(~Label,nrow = 2)+scale_colour_manual(values = palette()[1:5])+theme_bw()+
   theme(panel.grid = element_blank(),panel.background = element_rect(fill="white"),legend.key= element_rect(fill = "white"))+
   theme(strip.background = element_rect("steelblue"),text = element_text(size=18))+
   scale_x_continuous(name="Year", breaks=Yearlabs,labels = Yearlabs)
print(GP)
dev.off()

###################################

taf.png("all_effort")
par(mar=c(5, 4, 4, 5)+0.1,xpd=TRUE)

effort<-read.csv("./data/effort.csv",header=T)
Seff<-effort$Total
eff<-as.matrix(effort[,2:3])

plot(x=effort$Year,y=effort$Total,type="b",ylab='Standardised effort', xlab="Year",
     col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(effort$Total)),main="Sandeel Area 1-R")
lines(effort$Year,effort[,2],type='b',col=2,lty=2,lwd=2, pch=2)
lines(effort$Year,effort[,3],type='b',col=3,lty=3,lwd=2, pch=3)
legend("topright",
       c('Total effort','effort 1st half','effort 2nd half'),
       pch=c(1,2,3),lty=c(1,2,3),lwd=c(3,2,2),col=c(1,2,3))

dev.off()

## 2 Output

taf.png("cpue_effort")
par(mar=c(5, 4, 4, 5)+0.1,xpd=TRUE)

a<-Read.summary.data_TAF()
Yield<-tapply(a$Yield,list(a$Year),sum)
Yield<-Yield[-nrow(Yield)]
cpue<-Yield/Seff

cpue[Seff<10]<-NA

tit<-'Catch per Standardised day fishing'

plot(as.numeric(names(cpue)),cpue,type='b',ylab='CPUE (tonnes per standardised day fishing)',xlab=' ',col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(cpue, na.rm=T)),main=tit)

legend("topright",
       c('CPUE','Effort'),
       pch=c(1,2),lty=c(1,2),lwd=c(3,2),col=c(1,2))

par(new=T)
plot(as.numeric(names(cpue)),Seff,ylim=c(0,max(Seff, na.rm=T)),axes=F,lwd=3,lty=2,type='b',xlab=' ',ylab='',col=2,pch=2)
axis(side=4)
mtext(side=4,line=3.0,"Standardised Effort")
par(xaxs="r")

dev.off()

##############################################
taf.png("effort_fbar")
par(mar=c(5, 4, 4, 5)+0.1,xpd=TRUE)

a<-Read.summary.data_TAF()

a$deadC<-a$N.bar*a$F
a$deadAll<-a$N.bar*a$Z
a<-subset(a,select=c(Year,Age,deadC,deadAll,Z))
a<-aggregate(list(deadC=a$deadC,deadAll=a$deadAll,Z=a$Z),list(Year=a$Year,Age=a$Age),sum)
a$FF<-a$Z*a$deadC/a$deadAll

tab1<-tapply(a$FF,list(a$Year,a$Age),sum)

f<-apply(tab1[,2:3],MARGIN = 1,mean)

tit="Annual Effort and Fbar"
plot(years,Seff,type='b',xlab=' ',ylab="Standardised effort",col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(Seff, na.rm=T)),main=tit)


legend("topright",
       c('Total effort',"Fbar"),
       pch=c(1,2),lty=c(1,2),lwd=c(3,2),col=c(1,2))
par(new=T)
plot(years,f[-length(f)],ylim=c(0,max(f, na.rm=T)),axes=F,lty=2,lwd=2,type='b',xlab=' ',col=2,pch=2,ylab="")
axis(side=4)

mtext(side=4,line=3.0,"Fishing mortality age 1-2")
par(xaxs="r")
dev.off()

###########################
taf.png("all_effort_Fbar")
par(mar=c(5, 4, 4, 5)+0.1,xpd=TRUE)

tit="Effort And Fbar"
plot(years,Seff,type='b',xlab=' ',ylab="Standardised effort",col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(Seff, na.rm=T)),main=tit)
lines(years,eff[,1],type='b',col=2,lty=2,lwd=2, pch=2)
lines(years,eff[,2],type='b',col=3,lty=3,lwd=2, pch=3)

legend("topright",
       c('Total effort','effort 1st half','effort 2nd half','F(1-2)'),
       pch=c(1,2,3,4),lty=c(1,2,3,4),lwd=c(3,2,2,4),col=c(1,2,3,4))

par(new=T)
plot(years,f[-length(f)],ylim=c(0,max(f, na.rm=T)),axes=F,lwd=4,type='b',xlab=' ',col=4,pch=4,ylab="")
axis(side=4)

mtext(side=4,line=3.0,"Fishing mortality age 1-2")
par(xaxs="r")

dev.off()
#################
taf.png("annual_effort_Fbar")
par(mar=c(5, 4, 4, 5)+0.1,xpd=TRUE)

tit<-'Standardised effort and Fbar'

plot(years,Seff,type='b',xlab=' ',ylab='Standardised effort',col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(Seff, na.rm=T)),main=tit)

legend("topright",
       c('Total effort','F(1-2)'),
       pch=c(1,2),lty=c(1,2),lwd=c(3,3),col=c(1,2))

par(new=T)
plot(years,f[-length(f)],ylim=c(0,max(f, na.rm=T)),axes=F,lwd=3,type='b',xlab=' ',col=2,pch=2,ylab="")
axis(side=4)

mtext(side=4,line=3.0,"Fishing mortality age 1-2")
par(xaxs="r")
dev.off()

##################### 
# Catch Residuals Bubble
# This function originally used two nested functions: plot.catch.residuals2 and residplot
# I have combined them in one script (FUNCresidplot_TAF) and stripped out some of the excess functionality 
# search taf.png in function to see the actual plotting line and change dimensions accordingly and include "save" code
# dev.off() is applied at end of the outer funnction
# setwd() is also manually set in residplot_TAF

plot.catch.residuals_TAF()

###########################
#Survey Residuals Bubble
plot.survey.residuals_TAF(nox=1,noy=1,start.year=2000,end.year=2020,over.all.max=1)



