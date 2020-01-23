###
## Script to run Retrospecitve run of Sandeel Area-1r
###
library(FLCore)

# user options
# first and last year for retro analysis of the most recent period
number_of_peels = 5
l.year <- read.sms.dat_TAF("last.year")
f.year <- l.year - number_of_peels

fa<-read.sms.dat_TAF("first.age")

addYear<-c(1,0,0); names(addYear)<-c("SSB","REC","FI")  # add year (to terminal year) for presentation for SSB, Recruitment and mean F

make.hist.retro<-F  # make retrospective analysis by cutting of the first years in the time series

hf.year <- read.sms.dat_TAF("first.year.model")+1 # fistoric first model year +1, should not be changed
hl.year<-hf.year+0 


# use survey obs for the year after the last assessment year (assuming the survey is conducted the 1. January.)
# 0=no, 1=yes, use extended survey series
extend.survey<-0

fleet.no<-"all"    # specification of fleets to be used in retrospective analysis
# "all"  all availeble fleets area used
#  fleet number, e.g. fleet.no<-1 or fleet.no<-c(1,2)
#fleet.no<-c(2)

######   end user options  ###################
#cleanup()

bio.interact<-FALSE

# retrospective runs are made in a separate dirictory
retro.dir<-"model/retro"

dir.create(retro.dir,showWarnings = FALSE)

SMS.files.single<-c("effort.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in","fleet_names.in","fleet_info.dat",
                    "zero_catch_season_ages.in","zero_catch_year_season.in","proportion_m_and_f_before_spawning.in","just_one.in","proportion_landed.in",'recruitment_years.in')


for (from.file in SMS.files.single) {
  to.file<-file.path(retro.dir,from.file)
  cp(paste0("model/",from.file), to.file)
}

## Get software
exefile <- if(os.linux()) "sms" else "sms.exe"
cp(file.path("model",exefile), "model/retro")


# read data and options into FLR objects
control<-read.FLSMS.control_TAF()

#stock<-SMS2FLStocks(read.input=TRUE,read.output=FALSE,bio.interact=FALSE,control=control)
indices<-SMS2FLIndices_TAF(control) 

old.dir<-getwd()
setwd(retro.dir)

#FLStocks2SMS(FLStock=stock,control=control,bio.interact=bio.interact)
FLIndices2SMS_TAF(indices=indices,control=control)

write.FLSMS.control_TAF(control,write.multi=bio.interact) 

retro.years<-l.year:f.year
if (make.hist.retro) retro.years<-c(-hl.year:-hf.year,retro.years)

for (y in (retro.years)){
  #y=retro.years[1]
  if (y>0) {
    for (j in 1:length(indices))
    {
      min.yr <- min(as.numeric(dimnames(indices[[j]]@index)$year))
      max.yr <- max(as.numeric(dimnames(indices[[j]]@index)$year))
      if (y < min.yr) stop("year.range is outside indices year range")
      indices[[j]] <- trim(indices[[j]],year=min.yr:(min(max.yr,y+extend.survey)))
      
    }
    
    outIndi<-indices
    if (fleet.no=="all") outIndi<-indices
    if (fleet.no!="all") {
      outIndi <- FLIndices()
      used.f<-1
      for (f in fleet.no) {  outIndi[[used.f]]<-indices[[f]]; used.f<-used.f+1}
    }
    
    FLIndices2SMS_TAF(indices=outIndi,control=control)
    
    #ontrol@last.year<-y
    control@last.year.model<-y
    control@read.HCR<-0
    #if (control@species.names== "Area-2" & y==2012) control@phase.F.a=2
  } else {  # cut of from the beginning of the time series
    control@first.year.model<-  -y
    control@catch.sep.year[[1]][1]<- -y
    control@catch.spline.year<- -y
  }
  
  write.FLSMS.control_TAF(control,write.multi=bio.interact) 
  # 
  # shell(paste( file.path(data.path,'retro',"sms.exe"),"-nox -nohess",sep=" "), invisible = TRUE)
  # if (!file.copy("summary.out", paste("summary",abs(y),".out",sep=""), overwrite = TRUE)) stop(paste("Retro stopped: something went wrong in copying summary.dat for year:",y))
  # file.remove("summary.out")
  # 
  # if (!file.copy("summary_table.out", paste("summary_table.out",abs(y),".out",sep=""), overwrite = TRUE)) stop(paste("Retro stopped: something went wrong in copying summary.dat for year:",y))
  # file.remove("summary_table.out")
  # 
  #print(getwd())
  system("./sms")
  if (!file.copy("summary.out", paste("summary",abs(y),".out",sep=""), overwrite = TRUE)) stop(paste("Retro stopped: something went wrong in copying summary.dat for year:",y))
  
  
}


#########################################################
av.F.age<-control@avg.F.ages 

lab<-suppressWarnings(paste('Retrospective anlysis:',f.year,'-',l.year,ifelse (min(retro.years)<0,paste(' and ', min(abs(retro.years[retro.years<0])),' - ',max(abs(retro.years[retro.years<0])),sep=''),' ')))
if (fleet.no!="all") lab<-paste(lab," fleet:",list(fleet.no))

i<-0
for (y in(retro.years)) {
  i<-i+1
  file<-paste('summary',abs(y),'.out',sep='')
  s<-read.table(file,header=TRUE)
  s<-data.frame(Species=control@species.names[s$Species.n],s)
  
  s1<-subset(s,Z>-1)
  rec<-subset(s1,Quarter==control@rec.season & Age==control@first.age )
  rec<-tapply(rec$N,list(rec$Species.n,rec$Year),sum)/1000
  
  if (addYear['SSB']>0) s1<-subset(s,Quarter==1) else  s1<-subset(s,Quarter==1 & Z > -1)
  ssb<-tapply(s1$SSB,list(s1$Species.n,s1$Year),sum)/1000
  if (i==1) {
    SSB<-array(NA,dim=c(nsp,control@last.year-control@first.year+1+addYear[1],length(retro.years)),   
               dimnames=list(control@species.names,seq(control@first.year,control@last.year+addYear[1]),abs(retro.years)))
    FI<- array(NA,dim=c(nsp,control@last.year-control@first.year+1+addYear[3],length(retro.years)),   
               dimnames=list(control@species.names,seq(control@first.year,control@last.year+addYear[3]),abs(retro.years)))
    REC<-array(NA,dim=c(nsp,control@last.year-control@first.year+1+addYear[2],length(retro.years)),   
               dimnames=list(control@species.names,seq(control@first.year,control@last.year+addYear[2]),abs(retro.years)))
  }
  SSB[,dimnames(ssb)[[2]],i]<-ssb
  REC[,dimnames(rec)[[2]],i]<-rec
  
  s1<-subset(s,Z>-1)
  F.at.age<-tapply(s1$F,list(s1$Year,s1$Age,s1$Species.n),sum,na.rm=T)
  for (sp in (1:nsp)){
    FI[,dimnames(rec)[[2]],i]<-apply(F.at.age[ ,(av.F.age[1]-fa+1):(av.F.age[2]-fa+1),sp],c(1),mean)  ### kun med en art-- fejl
  }
}  


i<-0
#newplot(dev,nox,noy,Portrait=T)
#par(oma=c(2.5,0.5,1.5,0.5),cex=1,las=1,mar=c(1,6,2,1))
par(mar=c(1,6,2,2))

plot.retro<-function(x,label,main=' ',legend.x=0.15,legend.y=1) { 
  
  rho<-function(x) {
    ny<-dim(x)[[2]] 
    n<-dim(x)[[3]]-1 
    
    rho<-0
    cat('\n\n')
    for (i in (1:n)) {
      cat('i:',i,' ')
      cat("(",  x[1,ny-i,i+1],'-',x[1,ny-i,1],') / ',x[1,ny-i,1])
      rhotmp <-(x[1,ny-i,i+1] -   x[1,ny-i,1])   /   x[1,ny-i,1]
      cat(" =",rhotmp,'\n')
      rho <-rho+rhotmp
    }
    cat("rho=",rho/n,"  n=",n,'\n\n')
    return(rho/n)
  }
  
  yr<-as.numeric(dimnames(x)[[2]])
  nyr<-length(retro.years)
  
  #if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
  
  plot(yr,x[1,,1],xlab='',ylab=label,main=main,type='b', ylim=c(0,max(x[sp,,],na.rm=T)),lwd=2,pch=1,col=ifelse(retro.years[1]<0,2,1))
  #legend(x=max(yr)-legend.x*(max(y)-min(yr)),y=max(x)*legend.y,legend=as.character(abs(retro.years)),pch=seq(1,nyr),col=1)
  if (nyr>1)  { mrho<-rho(x);  legend("topright", legend=paste("Mohn's rho: ",round(mrho,2),'  '), bty="n",cex=1.5) }
  
  grid()
  i<<-i+1
  for(yy in (2:length(retro.years))) {
    lines(yr,x[1,,yy],type='b',pch=yy,col=ifelse(retro.years[yy]<0,yy+1,1),lwd=2)
  }
} 

dir.create("retro_output")
setwd("retro_output")

taf.png("retro")
par(mfrow=c(3,1))
par(mar=c(3,5,1.5,1)+0.1)
plot.retro(SSB,'SSB',main=lab,legend.x=0.15)
plot.retro(FI,expression(bar(F)),legend.x=0.4)
plot.retro(REC,"recriuts 10^6",legend.x=0.4)
dev.off()

write.taf(as.data.frame(as.matrix(ftable(SSB[1,,]))),file = "retro_SSB.csv",row.names = T)
write.taf(as.data.frame(as.matrix(ftable(FI[1,,]))),file = "retro_F.csv",row.names = T)
write.taf(as.data.frame(as.matrix(ftable(REC[1,,]))),file = "retro_REC.csv",row.names = T)

setwd(old.dir)
