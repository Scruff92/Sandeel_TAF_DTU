# unfinished script for SSB_R plot (can still be cut down further maybe?)
#################################


include.terminal.year <- FALSE    ###needed!!      # plot terminal year as well?
dev<-"screen"
Portrait <- F
nox<-1
noy<-1
noxy<-nox*noy
#par(ask=TRUE)

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

##########################  SSB - Recruit plot Obsevations and regression line  #############

include.CV<-TRUE            # include CV on estimated relation 
include.CV2<-TRUE           # include 2*CV on estimated relation 
include.year.labels<-TRUE   # put year label on each point

include.MCMC.hist<-F    # include MCMC values of historical assessment SSB- R obs
include.MCMC.pred<-F    # include MCMC values of prediction SSB- R obs

include.excluded.years<-T   # exclude years not included in the SSB-R fit
scale.yaxis.include<-T

SSB_R<-function(x) {
  delta<-0.5 
  y<-x  # copy structure
  #print(c(alfa,beta,exp(alfa+beta)))
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


noxy<-nox*noy
taf.png("SSB_R")
i<-0
dot.size<-1

if ((include.MCMC.hist) || (include.MCMC.pred)) { #read MCMC.output
  #ssb.r<-Read.MCMC.SSB.rec.data()  I havent adapted this function  to TAF yet
  dot.size<-2
}

s.index<-0
for (sp in ss) {
  s.index<-s.index+1
  model<-p[s.index,'model']
  alfa<-p[s.index,'alfa']
  beta<-p[s.index,'beta'] 
  info1<-p[s.index,'info1']
  info2<-p[s.index,'info2']
  CV<- sqrt(exp(p[s.index,'std']^2) - 1)
  if (i==noxy) {x11(); i<-0 }
  
  colors<-seq(first.year.model,max(s$Year))
  if (include.excluded.years) {
    for (j in (1:length(colors))) {
      if (colors[j] %in% (SSB.R.year.first[s.index]:SSB.R.year.last[s.index])) colors[j]<-1 else colors[j]<-2
    }
    ry<-recruit.years[s.index,]
    ry<-ry[as.character(seq(first.year.model,max(s$Year)))]
    colors[ry==0]<-2
  }
  
  
  recPlot<-rec[sp,]
  if (scale.yaxis.include) recPlot[colors!=1]<-NA
  SSBplot<-ssb[sp,]
  if (scale.yaxis.include) SSBplot[colors!=1]<-NA
  
  modelName<-model.name[min(model,4)]
  if (model==51 | model==52) modelName<-paste("Ricker, Temp=",info2,sep='')
  
  plot(ssb[sp,1:dim(rec)[2]],rec[sp,],xlab="SSB (1000t)",ylab="recruits 10^9",
       main=paste("Area 1-r",': ',modelName,', ',SSB.R.year.first[s.index],":",SSB.R.year.last[s.index],sep=''),
       ylim=c(0,max(recPlot*1.2,na.rm=T)),type='p',col=colors,pch=19,cex=dot.size,
       xlim=c(0,max(SSBplot,na.rm=T)))
  
  ############### latest recruitment marked in red #######
  xx <- ssb[sp,1:dim(rec)[2]]
  yy <- rec[sp,]
  points(xx[length(xx)],yy[length(yy)], pch = 21, bg = "red")       
  ########################################################
  
  # print(ssb);print(rec)  ;print(as.character(seq(SMS.control@first.year,max(s$Year))%%100))
  if ((noxy <5) && (include.year.labels==1)) text(x=ssb[sp,],y=rec[sp,],labels=as.character(sprintf("%02.0f",seq(SSB.R.year.first,max(s$Year))%%100)),pos=3)
  
  x<-seq(0,max(ssb[sp,]), by=max(ssb[sp,])/100)*1000
  y<-SSB_R(x)
  x<-x/1000
  y<-y/1000000
  lines(x,y,col=2)
  if (include.CV) {
    lines(x,y*exp(CV),col=4)
    lines(x,y*exp(-CV),col=4)
    if (include.CV2==1) {
      lines(x,y*exp(2*CV),col=5)
      lines(x,y*exp(-2*CV),col=5)
    }
  }
  if (model==100) abline(v=beta/1000,lty=2)
  
  if (include.MCMC.hist) {
    a<-subset(ssb.r,(Year<=last.year.model & Species.n==sp))
    x<-a$SSB/1000
    y<-a$recruit/1000000
    points(x,y,pch=25,col=2)
  }
  if (include.MCMC.pred) {
    a<-subset(ssb.r,(Year>last.year.model & Species.n==sp))
    x<-a$SSB/1000
    y<-a$recruit/1000000
    points(x,y,pch=24,col=3)
    
  }
  
  # re-draw
  if ((include.MCMC.hist) || (include.MCMC.pred)) {
    
    points(ssb[sp,1:dim(rec)[2]],rec[sp,],
           type='p',col=colors,pch=19,cex=dot.size )
    if ((noxy <5) && (include.year.labels==1)) text(x=ssb[sp,],y=rec[sp,],
                                                    labels=as.character(seq(SSB.R.year.first,max(s$Year))%%100),pos=3)
  }
  i<-i+1
}

if (FALSE) {
  ########################
  ## Histogram of residuals
  s.index<-0
  
  if (dev=="print") cleanup()
  
  #nox<-3; noy<-4; 
  nox<-2; noy<-2;
  noxy<-nox*noy
  newplot(dev,filename='SSB_R_c',nox,noy,Portrait);
  i<-0
  
  for (sp in ss) {
    s.index<-s.index+1
    model<-p[s.index,'model']
    alfa<-p[s.index,'alfa']
    beta<-p[s.index,'beta'] 
    info1<-p[s.index,'info1']
    info2<-p[s.index,'info2']
    CV<- sqrt(exp(p[s.index,'std']^2) - 1)
    modelName<-model.name[min(model,4)]
    if (model==51 | model==52) modelName<-paste("Ricker, Temp=",info2,sep='')
    
    if (i==noxy) {newplot(dev,filename=paste('SSB_R_c',s.index,sep=''),nox,noy,Portrait); i<-0 }
    x<-ssb[sp,as.character(SSB.R.year.first[s.index]:SSB.R.year.last[s.index])]*1000
    y<- 1000000*rec[sp,as.character(SSB.R.year.first[s.index]:SSB.R.year.last[s.index])] /SSB_R(x)   # residuals: observed / predicted
    
    ry<-recruit.years[s.index,]
    ry<-ry[names(y)]
    x<-x[ry==1]
    y<-y[ry==1]
    
    #cat("SSB:\n",x,"\n")
    #cat('obs rec:\n',rec[sp,],"\n")
    #cat("calc rec:\n",SSB_R(x),'\n')
    #cat('resid:\n',log(y),'\n')
    cat(sp.names[as.numeric(sp)],'resid min:',log(min(y)),'  resid max:',log(max(y)),'\n')
    x<-seq(-3,3,by=0.01)
    hist(log(y),freq=F,main=paste("Area 1-r",': ',modelName," ",SSB.R.year.first[s.index],':',SSB.R.year.last[s.index],sep=''),xlab='Residual (log(obs/est))')
    
    SD<-p[s.index,'std']
    curve(dnorm(x,mean=0,sd=SD), col = 2, lty = 2, lwd = 3, add = TRUE)
    i<-i+1
  }
  
  
  ## range in noise function
  s.index<-0
  
  if (dev=="print") cleanup()
  
  newplot(dev,filename='SSB_R_d',nox,noy,Portrait);
  i<-0
  
  for (sp in ss) {
    s.index<-s.index+1
    model<-p[s.index,'model']
    alfa<-p[s.index,'alfa']
    beta<-p[s.index,'beta'] 
    info1<-p[s.index,'info1']
    info2<-p[s.index,'info2']
    CV<-p[s.index,'std']
    modelName<-model.name[min(model,4)]
    if (model==51 | model==52) modelName<-paste("Ricker, Temp=",info2,sep='')
    
    if (i==noxy) {newplot(dev,filename=paste('SSB_R_d',s.index,sep=''),nox,noy,Portrait);; i<-0 }
    x<-ssb[sp,as.character(SSB.R.year.first[s.index]:SSB.R.year.last[s.index])]*1000
    y<- 1000000*rec[sp,as.character(SSB.R.year.first[s.index]:SSB.R.year.last[s.index])] /SSB_R(x)   # residuals: observed / predicted
    ry<-recruit.years[s.index,]
    ry<-ry[names(y)]
    x<-x[ry==1]
    y<-y[ry==1]
    
    cat(sp.names[as.numeric(sp)],'norm min:',log(min(y))/CV,'  norm max:',log(max(y))/CV,'\n')
    x<-seq(-3,3,by=0.01)
    hist(log(y)/CV,freq=F,main=paste("Area 1-r",': ',modelName," ",SSB.R.year.first[s.index],':',SSB.R.year.last[s.index],sep=''),xlab='Noise')
    
    CV<-p[s.index,'std']
    curve(dnorm(x,mean=0,sd=1), col = 2, lty = 2, lwd = 3, add = TRUE)
    i<-i+1
  }
  if (dev=='png') cleanup()
  #exp(sqrt(SSB_R_s2(s))*noise)
  
  ##########################  SSB - cummulated Recruit probability  plot #############
  if (F) {
    nox<-1; noy<-1
    include.MCMC.hist<-T    # include line for MCMC values of historical assessment SSB- R obs
    include.MCMC.pred<-T    # include line for MCMC values of prediction SSB- R obs
    
    ref<-Read.reference.points()
    lower.SSB.limit.on.plot<-ref[,'Blim']/1000
    
    ss<-(seq(first.VPA,nsp))
    s<-Read.summary.data()
    
    for (sp in ss) {
      # extract SSB and Recruits
      
      s1<-subset(s,Quarter==1 & Year<=SMS.control@last.year.model-SMS.control@first.age & Species.n==sp )
      s1<-subset(s1,Year %in% (SSB.R.year.first[sp]:SSB.R.year.last[sp]))
      ssb<-tapply(s1$SSB,list(s1$Year),sum)/1000
      
      rec<-subset(s,Quarter==SMS.control@rec.season &Species.n==sp,select=c(Year,Age,N))
      rec<-subset(rec,Year %in% (SSB.R.year.first[sp]:SSB.R.year.last[sp]) )
      rec<-subset(rec,Age==SMS.control@first.age & Year>=SMS.control@first.year+SMS.control@first.age)
      rec<-tapply(rec$N,list(rec$Year),sum)/1000000
      
      
      incl<-ssb>lower.SSB.limit.on.plot[sp]
      
      ssb<-ssb[incl]
      rec<-rec[incl]
      
      if (dev=="print") cleanup()
      newplot(dev,nox,noy);
      i<-0
      
      sp.rec<-sort(rec)
      prob.rec<-seq(1,length(sp.rec))
      prob.rec<-prob.rec/length(prob.rec)
      
      if ((include.MCMC.hist==1) || (include.MCMC.pred==1)) { #read MCMC.output
        ssb.r<-Read.MCMC.SSB.rec.data()
      }
      
      if (i==noxy) {newplot(dev,nox,noy); i<-0 }
      max.x<-max(sp.rec)
      if (include.MCMC.pred==1) {
        a<-subset(ssb.r,(Year>(SMS.control@last.year.model) & Species.n==sp ))
        agem<<-a
        max.x<-max(a$recruit/1000000,max.x)
      }
      
      if (lower.SSB.limit.on.plot[sp]>0) {
        if (nsp>1) titl<-paste("Area 1-r","SSB>",lower.SSB.limit.on.plot[sp]) else titl<-' '
      } else titl<-SMS.control@species.names[sp]
      
      plot(sp.rec,prob.rec ,ylab="probability",xlab="recruits 10^9",main=titl,
           xlim=c(0,max.x), ylim=c(0,1),type='p',col=1,pch=24 )
      
      if (include.MCMC.hist) {
        a<-subset(ssb.r,Year<=SMS.control@last.year.model & Species.n==sp & SSB>1000*lower.SSB.limit.on.plot[sp] &
                    Year %in% (SSB.R.year.first[sp]:SSB.R.year.last[sp]))
        x<-sort(a$recruit/1000000)
        y<-seq(1,length(x))
        y<-y/length(y)
        # points(x,y,pch=25,col=2)
        lines(x,y,col=1,lwd=2)
      }
      if (include.MCMC.pred) {
        a<-subset(ssb.r,Year>(SMS.control@last.year.model) & Species.n==sp & SSB>1000*lower.SSB.limit.on.plot[sp])
        x<-sort(a$recruit/1000000)
        y<-seq(1,length(x))
        y<-y/length(y)
        lines(x,y,col=2,lwd=2)
      }
      i<-i+1
    }
  }
  ##################################
}

dev.off()