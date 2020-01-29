## Prepare plots for report

## Before: catage.csv, wcatch.csv effort.csv (data)
#          summary.out (model)
## After:  catage.png, wcatch.png, all_effort.png, cpue_effort.png,
#          effort_fbar.png, all_effort_Fbar.png, catch_residuals.png,  
#          annual_effort_Fbar.png, Model_Output.png, SSB_R.png,
#          Summary.png, survey_index_scaled.png, Survey_resids_Fleet_1.png,
#          Survey_resids_Fleet_2.png (report)

#NOTES: Why is Fbar diferent between summary out tables?

if(!"ggplot2" %in% installed.packages()) {install.packages("ggplot2")}
library(ggplot2)
if(!"cowplot" %in% installed.packages()) {install.packages("cowplot")}
suppressPackageStartupMessages(library(cowplot)) 
source("utilities_sms.R")
options(bitmapType = "cairo")

# Make Directory
mkdir("report")

#For use later in the script
years <- read.sms.dat_TAF("first.year"):read.sms.dat_TAF("last.year")

###############################
## Catch at age
taf.png("catage")
catage <-read.taf("data/catage.csv")
catage$`0`[catage$Step==1] = 0
catage <- aggregate(cbind(`0`,`1`,`2`,`3`,`4+`)~Year,data = catage,sum)
catprop<-as.data.frame(catage[,-1],row.names = catage$Year)
catprop<-as.data.frame(prop.table(as.matrix(catprop),1))
catprop<-xtab2taf(catprop)
plus(catprop)
catprop_long <- taf2long(catprop, names=c("Year","Age","Fmort"))

# The years to be included on X-axis of figure (5 years between each)
Yearlabs=c(1983,1988,1993,1998,2003,2008,2013,2018)

GP<-ggplot(catprop_long, aes(x=Year, y=Fmort))+geom_col(aes(fill=Age),col="black",size=0.2 )+
  ylab("Proportion at age")+ylim(c(0,1.0000001))+
  scale_x_continuous(name="Year", breaks=Yearlabs,labels = Yearlabs)+theme(text = element_text(size=35))+
  theme(legend.title = element_text(size=40),legend.text = element_text(size=40),
        legend.key.size = unit(2,"line"))

print(GP)
dev.off()
################################
## Weight of catch
taf.png("wcatch")

par(mar=c(5,7,5,7)+0.1,mgp=c(5,2,0))
wcatch <- read.taf("data/wcatch.csv")
wcatch <- step2long(wcatch)
wcatch$Label <- factor(wcatch$Step,
                       labels=c("First half year", "Second half year"))

GP <- ggplot(wcatch[!wcatch$Age=="0" | !wcatch$Step==1,],aes(x = Year,y=Value,col=Age))+
  geom_line(size=2)+geom_point(cex=0.3,pch=3)+
  facet_wrap(~Label,nrow = 2)+scale_colour_manual(values = palette()[1:5])+theme_bw()+
  theme(panel.grid = element_blank(),panel.background = element_rect(fill="white"),legend.key= element_rect(fill = "white"))+
  theme(strip.background = element_rect("steelblue"),text = element_text(size=35))+
  scale_x_continuous(name="Year", breaks=Yearlabs,labels = Yearlabs)+ylab("Mean Weight (g)")+
  theme(legend.key.size = unit(3,"line"))

print(GP)
dev.off()

###################################
## All effort
taf.png("all_effort")
par(mar=c(5, 4, 4, 5)+0.1,xpd=TRUE)

effort<-read.taf("./data/effort.csv",header=T)
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
###################################
## CPUE effort
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
## Effort and F-bar
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
## All effort and F-bar
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
## Annual Effort and F-bar
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
plot.catch.residuals_TAF(start.year = 1990,end.year = 2020,
                         use.ref.dot=F,over.all.max = 1.5)

###########################
#Survey Residuals Bubble

plot.survey.residuals_TAF(start.year=2000,end.year=2020,
                          use.ref.dot=F,over.all.max=1.5)

########################
# Multi Summary (SAG-like)
include.terminal.year <- T          # plot terminal year (last assessment year +1) as well?
include.last.assess.year.recruit <- T          # plot recruits terminal year as well?

first.year<- -1975                #first year on plot, negative value means value defined by data
last.year<- 2060               #last year on plot
incl.M2.plot<-F
incl.reference.points<-T

OperatingModel<-F
redefine.scenario.manually<-T

palette("default")
nox<-2; noy<-2;
noxy<-nox*noy
ref<-Read.reference.points_TAF()
dat<-Read.summary.data_TAF()
dat<-subset(dat,Year<=last.year )
if (first.year>0) dat<-subset(dat,Year>=first.year )
sp=1
sp.name<-"Area-1r"
discard<-FALSE

taf.png("Summary", width = 1600, height = 1200,units = "px", pointsize = 30, bg = "white")
par(mfrow=c(2,2))
par(mar=c(3,4,3,2))

s<-subset(dat,Species.n==1)
txt<-readLines(file.path("./model","sms.dat"))
av.F.age<-txt[grep(pattern = "option avg.F.ages", x = txt,fixed = F)+1]
av.F.age<-as.numeric(unlist(strsplit(av.F.age," "))[c(1,2)])
names(av.F.age)<-c("first-age","last-age")

s1<-subset(s,s$Age>=av.F.age[1] & s$Age<=av.F.age[2])
FI<-tapply(s1$F,list(s1$Year),sum)/(av.F.age[2]-av.F.age[1]+1)
s1<-subset(s,weca>=0 )

Yield<-tapply(s1$Yield,list(s1$Year),sum)/1000
SOP<-tapply(s1$CWsum,list(s1$Year),sum)/1000
catch<-Yield
s1<-subset(s,Quarter==1)
ssb<-tapply(s1$SSB,list(s1$Year),sum)/1000

#
if(!"fa" %in% ls())  {fa<-read.sms.dat_TAF("first.age")}
# This is first age. 
# It is set in the retrospective script so it will try to inherit.


s2<-subset(s,Age==fa & Quarter==2)
rec<-tapply(s2$N,list(s2$Year),sum)/1000000
year<-as.numeric(unlist(dimnames(ssb)))
year.ssb<-year

if(include.terminal.year){
  #Truncate the final year from key parameters
  year<- year[-length(year)]        
  FI <- FI[-length(FI)]
} 
if(!include.last.assess.year.recruit) rec <- rec[-length(rec)]

barplot(catch,space=1,xlab='',ylab='1000 tonnes',main=paste(sp.name,ifelse(discard,',  Yield and discard',',  Catch'),sep=''),ylim=c(0,max(SOP)))

#plot recruits
barplot(rec,space=1,xlab='',ylab='billions',main=paste('Recruitment age',fa),ylim=c(0,max(rec)))
F.max<-max(FI,ref[sp,"Flim"])

plot(year,FI,type='b',lwd=3,xlab='',ylab='',main="Fishing mortality",ylim=c(0,F.max))

if (incl.reference.points) if (ref[sp,"Flim"]>0) abline(h=ref[sp,"Flim"],lty=2,lwd=2)
if (incl.reference.points) if (ref[sp,"Fpa"]>0) abline(h=ref[sp,"Fpa"],lty=3,lwd=2)
grid()

Blim<-ref[sp,"Blim"]/1000; Bpa<-ref[sp,"Bpa"]/1000
SSB.max<-max(ssb,Bpa)
plot(year.ssb,ssb,type='b',lwd=3,xlab='',ylab='1000 tonnes',main='SSB',ylim=c(0,SSB.max))
if (incl.reference.points) if (Blim>0) abline(h=Blim,lty=2,lwd=2)
if (incl.reference.points) if (Bpa>0) abline(h=Bpa,lty=3,lwd=2)
grid()

dev.off()

#######################################################
#Dredge survey index timeline (scaled)
taf.png("survey_index_scaled")

dat1=read.csv(file.path("data","survey.csv"))
dat11=taf2long(dat1)

#it is scaling for both ages seperately!
dat11$scaled=NA
dat11$scaled[dat11$Age=="X0"]<-scale(dat11$Value[dat11$Age=="X0"])
dat11$scaled[dat11$Age=="X1"]<-scale(dat11$Value[dat11$Age=="X1"])
dat11$Age<-factor(dat11$Age,labels=c("0","1"))
dat<-dat11

#format like previous program
miny =min(dat["Year"])
maxy =max(dat["Year"])
if(maxy-miny<10) by=2 else by=4

GP<-ggplot(dat, aes(x=Year, y=scaled, color=Age))+geom_line(size=2.1)+theme_bw()+ylab("Survey index (scaled)")+
  scale_x_continuous(breaks=seq(miny, maxy, by=by))
GP<-GP+theme(text = element_text(size=35))+theme(legend.key.size = unit(3,"line"))+
  theme(panel.grid.minor = element_line(size = 0.8), panel.grid.major = element_line(size = 1.5))

print(GP)
dev.off()
########################################
#SSB _ R relationship
source("SSB_R_plot.R")

######################
#Model Output
taf.png("Model_Output.png")

#Arith log is either "Arithmetric" or "Log values". This is originally set at the beginning of output.
#print(Arith_log)

par(mfrow=c(3,1),
    mar=c(5,12,1.5,1.5)+0.1,
    mgp=c(3,2,0))

include.TAC.year.SSB<-T                      # should the SSB plot include the SSB for the fisrt year after the assessment year (default TRUE)

VAR.NAMES=c('avg_sumF','hist_SSB','rec_sd') 
ylabs=c("Average F","SSB (1000 t)","Recruitment (10^6)")


for(i in 1:3){
  
  var.name=VAR.NAMES[i]
  ytitl<-ylabs[i]
  
  ref<-Read.reference.points_TAF()
  tmp<-Read.SMS.std_TAF()
  tmp$name[tmp$name=="next_SSB"]<-'hist_SSB'
  a<-subset(tmp,name==var.name & species>0 ,drop=TRUE)
  a$Species<-"Area-1r";sp <-1
  
  if (var.name=='rec_sd') {
    a$value<-a$value/1000
    a$std<-a$std/1000
    if (include.last.assessment.year.recruit==F) {
      a<-subset(a,year!=read.sms.dat_TAF("last.year.model"))
    }
  }
  
  if(var.name=="hist_SSB"){
    a$value <- a$value/1000
    a$std <- a$std/1000
  }
  
  minval<-min(a$value-2*a$std*2,0)  # plotting y-axis range
  maxval<-max(a$value+1.25*a$std*2) # plotting y-axis range
  
  a$max <- a$value+two*a$std
  a$min <- a$value-two*a$std
  
  GP<-ggplot(a,aes(x=year,y=value))+geom_line()+ylab(ytitl)+xlab("Year")+
    geom_ribbon(aes(ymax=max,ymin=min),fill="steelblue",alpha=0.35)+
    theme(text = element_text(size=35))
  
  #add ref lines where needed
  if (var.name == 'avg_sumF') {
    if (ref[sp,"Flim"]>0) GP <- GP+geom_hline(yintercept =ref[sp,"Flim"],lty=2)
    if (ref[sp,"Fpa"]>0)  GP <- GP+geom_hline(yintercept =ref[sp,"Fpa"],lty=3)
  }
  
  if (var.name == 'hist_SSB') {
    Blim<-ref[sp,"Blim"]; Bpa<-ref[sp,"Bpa"]
    if (Blim>0) GP<- GP+geom_hline(yintercept =Blim/1000,lty=2)
    if (Bpa>0)  GP<- GP+geom_hline(yintercept =Bpa/1000,lty=3)
  }
  
  assign(x=paste0("GP",i),GP)
  
}

print(plot_grid(GP1,GP2,GP3,ncol = 1))
dev.off()

#######################################
# Model Uncertainties
taf.png("Model_Uncertainties")

#Arith log is either "Arithmetric" or "Log values". This is originally set at the beginning of output.
#print(Arith_log)

tmp<-Read.SMS.std_TAF()
tmp$Species<-"Area-1r"

if (Arith_log == "Arithmetric") {
  tmp$name[tmp$name=="next_SSB"]<-'hist_SSB'
  a<-tmp[tmp$name %in% c("hist_SSB","avg_sumF","rec_sd"),]
} else if (Arith_log == "Log values"){
  tmp$name[tmp$name=="next_log_SSB"]<-'hist_log_SSB'
  a<-tmp[tmp$name %in% c("hist_log_SSB","avg_log_sumF","log_recsd"),]
}

if (Arith_log == "Arithmetric") a<-data.frame(Species="Area-1r",Year=a$year, variable=a$name,value=a$value,std=a$std,CV=a$std/a$value*100)
if (Arith_log == "Log values")  a<-data.frame(Species="Area-1r",Year=a$year, variable=a$name,value=a$value,std=a$std,CV=a$std)

if (include.last.assessment.year.recruit==F) {
  a<-a[a$Year != read.sms.dat_TAF("last.year.model") | a$variable != "rec_sd",]
} 
a$titl<- ifelse (a$variable %in% c('hist_SSB','hist_log_SSB'),"SSB", ifelse(a$variable %in% c('avg_sumF','avg_log_sumF'),"Average F",ifelse(a$variable %in% c('rec_sd','log_recsd'), "Recruitment",'error')))

if (Arith_log == "Arithmetric") ylab<-'Coefficient of Variation (%)'  else ylab<-"standard deviation of log values (sigma)"             

for(i in 1:3){
  a0<-a[a$titl==unique(a$titl)[i],]
  GP <- ggplot(a0,aes(x=Year,y=CV))+geom_line()+geom_point(size=4,shape=16)+
    theme(text = element_text(size=35))+
    ggtitle(unique(a$titl)[i])+ylab("")
  
  #Title
  title_vertical_displacement<-35
  if(i==3){
    if(Arith_log == "Log values"){
      tit<- capture.output(cat(rep("\t",title_vertical_displacement),"Standard Deviation of Log Values (sigma)"))
      GP <- GP+ylab(tit)
    }else{
      tit<- capture.output(cat(rep("\t",title_vertical_displacement),"Standard Deviation of Values (sigma)"))
      GP <- GP+ylab(tit)
    }
  }
  assign(x = paste0("GP",i),GP)
}

print(plot_grid(GP1,GP2,GP3,ncol = 1))

dev.off()


#########################################
## Internal Consistancy
taf.png("internal_consistancy")

d = read.csv(file.path('./data','survey.csv')) 
d0 = taf2long(x=d, names = c("Year","Age","CPUE"))
d0$Age = as.numeric(gsub("X", "", d0$Age))

d0$CPUEnext=d0$CPUE[pmatch(paste(d0$Year+1, d0$Age+1), paste(d0$Year, d0$Age))]
d1 = na.omit(d0)
d1$Age=factor(d1$Age)

#Calculate R2 values
r2=summary(lm(log(CPUEnext)~log(CPUE),data = d1))$r.squared
minx=min(log(d1$CPUE), na.rm=TRUE)
maxy=max(log(d1$CPUEnext), na.rm=TRUE)

p1<-ggplot(d1, aes(x=log(CPUE), y=log(CPUEnext)))+
  geom_point(size=8)+geom_smooth(method="lm", se=FALSE, colour="black",size=3)+
  xlab("log(CPUE Age 0)")+ylab("log(CPUE Age 1)")+
  geom_point(data=d1[d1$Year==max(d1$Year),], colour="red",size=8)+
  geom_text(data=d1, check_overlap="true",  
            aes(x=minx+1.5, y=maxy*.99, label=paste0("Rsquared = ", round(r2,2))),size=15)+
  theme(text=element_text(size = 42))

print(p1)
dev.off()

######################################
## Retrospective
cp(from = "model/retro/retro_output/retro.png","report/")


##               END                ##