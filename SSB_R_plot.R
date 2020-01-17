## SSB - Recruit plot Obsevations and regression line 

include.terminal.year <- FALSE    ###needed!!      # plot terminal year as well?
include.CV<-TRUE            # include CV on estimated relation 
include.CV2<-TRUE           # include 2*CV on estimated relation 
include.year.labels<-TRUE   # put year label on each point
include.excluded.years<-T   # exclude years not included in the SSB-R fit
scale.yaxis.include<-T

nsp=1 
first.VPA=1

SSB.R.year.first<-read.sms.dat_TAF("first.year")
SSB.R.year.last <-read.sms.dat_TAF("last.year")
SSB.R.year.first[SSB.R.year.first==-1]<-read.sms.dat_TAF("first.year.model")
SSB.R.year.last[SSB.R.year.last==-1]<-read.sms.dat_TAF("last.year.model")

first.year.model <- read.sms.dat_TAF("first.year.model")
last.year.model <- read.sms.dat_TAF("last.year.model")
first.age<-read.sms.dat_TAF("first.age",printlabel=F)

# extract SSB and Recruits
s<-Read.summary.data_TAF(extend=include.terminal.year)

s1<-subset(s,Quarter==1 & Year<=SSB.R.year.last- first.age)
ssb<-tapply(s1$SSB,list(s1$Species.n,s1$Year),sum)/1000

rec.season<-read.sms.dat_TAF("rec.season")

rec<-subset(s,Quarter==rec.season,select=c(Year,Species.n,Age,N))
rec<-subset(rec,Age==first.age & Year>=SSB.R.year.first+first.age)
rec<-subset(rec,Year<=read.sms.dat_TAF("last.year.model"))

rec<-tapply(rec$N,list(rec$Species.n,rec$Year),sum)/1000000

# read recruiment years used
recruit.years<-matrix(scan(file=file.path("./model",'recruitment_years.in'),comment.char='#',quiet=T),nrow=dim(rec)[1],byrow=T)
colnames(recruit.years)<-c(as.character(seq(SSB.R.year.first,SSB.R.year.last)))

#read SSB/R parameters
p<-Read.SSB.Rec.data_TAF()

model.name<-c('Ricker','Bev. & Holt','Geom. mean','Hockey stick')

SSB_R<-function(x) {
  delta<-0.5 
  y<-x  # copy structure
  if (model==1) y<-alfa*x*exp(-beta*x)
  else if (model==51 | model==52) y<-alfa*x*exp(-beta*x+info1*info2)
  else if (model==2) y<-alfa*x/(1+x*beta)
  else if (model==3) y<-rep(exp(alfa),length(x))
  else if (model==4) {for (ii in (1:length(x))) y[i]<-exp(alfa)*min(x[i],exp(beta))}
  else if (model==5) {for (ii in (1:length(x))) {
    if (x[ii]<=(exp(beta)*(1-delta))) y[ii]<-exp(alfa)*x[ii]
    else if (x[ii]< exp(beta)*(1+delta)) y[ii]=exp(alfa)*(x[ii]-(((x[ii]-exp(beta)*(1+delta))^2)/(4*delta*exp(beta))))
    else if (x[ii]>=exp(beta)*(1+delta)) y[ii]<=exp(alfa+beta)
    else x[i]<-NA
  }}
  else if (model==100) {for (ii in (1:length(x))) y[ii]<-exp(alfa)*min(x[ii],beta)}
  return(y)
}

yy<-as.character(seq(SSB.R.year.first,SSB.R.year.last))
ss<-as.character(seq(first.VPA,nsp))

taf.png("SSB_R")

i<-0
dot.size<-1

s.index<-0
s.index<-s.index+1
model<-p[s.index,'model']
alfa<-p[s.index,'alfa']
beta<-p[s.index,'beta'] 
info1<-p[s.index,'info1']
info2<-p[s.index,'info2']
CV<- sqrt(exp(p[s.index,'std']^2) - 1)

colors<-seq(first.year.model,max(s$Year))
if (include.excluded.years) {
  for (j in (1:length(colors))) {
    if (colors[j] %in% (SSB.R.year.first[s.index]:SSB.R.year.last[s.index])) colors[j]<-1 else colors[j]<-2
  }
  ry<-recruit.years[s.index,]
  ry<-ry[as.character(seq(first.year.model,max(s$Year)))]
  colors[ry==0]<-2
}

sp=1
recPlot<-rec[sp,]
if (scale.yaxis.include) recPlot[colors!=1]<-NA
SSBplot<-ssb[sp,]
if (scale.yaxis.include) SSBplot[colors!=1]<-NA

modelName<-model.name[min(model,4)]
if (model==51 | model==52) modelName<-paste("Ricker, Temp=",info2,sep='')

#plot points
plot(ssb[sp,1:dim(rec)[2]],rec[sp,],xlab="SSB (1000t)",ylab="Recruits (10^9)",
     main=paste("Area 1-r",': ',modelName,', ',SSB.R.year.first[s.index],":",SSB.R.year.last[s.index],sep=''),
     ylim=c(0,max(recPlot*1.2,na.rm=T)),type='p',col=colors,pch=19,cex=dot.size,
     xlim=c(0,max(SSBplot,na.rm=T)))

############### latest recruitment marked in red #######
xx <- ssb[sp,1:dim(rec)[2]]
yy <- rec[sp,]
points(xx[length(xx)],yy[length(yy)], pch = 21, bg = "red")       
########################################################

#year labels
if (include.year.labels==1) text(x=ssb[sp,],y=rec[sp,],labels=as.character(sprintf("%02.0f",seq(SSB.R.year.first,max(s$Year))%%100)),pos=3)

x<-seq(0,max(ssb[sp,]), by=max(ssb[sp,])/100)*1000
y<-SSB_R(x)
x<-x/1000
y<-y/1000000
lines(x,y,col=2,lwd=3)#segmented regression

if (include.CV) {#confidence intervals
  lines(x,y*exp(CV),col=4,lwd=3,lty=2)
  lines(x,y*exp(-CV),col=4,lwd=3,lty=2)
  if (include.CV2==1) {
    lines(x,y*exp(2*CV),col=5,lwd=3,lty=2)
    lines(x,y*exp(-2*CV),col=5,lwd=3,lty=2)
  }
}

if (model==100) abline(v=beta/1000,lty=2,lwd=2)#vertical regression breakpoint

dev.off()
