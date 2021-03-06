

Portrait<-T                 # graphical output orientation
include.terminal.year <- T          # plot terminal year (last assessment year +1) as well?
include.last.assess.year.recruit <- T          # plot recruits terminal year as well?

first.year<- -1975                #first year on plot, negative value means value defined by data
last.year<- 2060               #last year on plot
incl.M2.plot<-F
incl.reference.points<-T

OperatingModel<-F; redefine.scenario.manually<-T

output.dir<-data.path
op.dir<-data.path
if (OperatingModel==T & redefine.scenario.manually==T)  {
   scenario<-"test"; 
   output.dir<-data.path 
   #op.dir<-file.path(data.path,scenario)
   op.dir<-data.path
} else if (OperatingModel==T & redefine.scenario.manually==F) {
     output.dir<-scenario.dir 
   op.dir<-scenario.dir
} 


##########################################################################


#My.device<-'screen'   # output device:  'screen', 'wmf', 'png', 'pdf'

#My.device<-'png'
  
palette("default")
                
file.name<-'summary'


#dev<-"dummy"
if (incl.M2.plot) { nox<-3; noy<-2;} else { nox<-2; noy<-2;}
noxy<-nox*noy

ref<-Read.reference.points()

Init.function()

if (!OperatingModel) dat<-Read.summary.data(extend=include.terminal.year,read.init.function=F)
if (OperatingModel) {
  dat1<-Read.summary.data(extend=F,read.init.function=F)

  dat<-Read.summary.data(dir=op.dir,infile="OP_summary.out",read.init.function=F)
  if (SMS.control@no.areas >1) {
    dat$N.bar<-dat$N*(1-exp(-dat$Z))/dat$Z
    dat$DM<-dat$M*dat$N.bar
    dat$DM1<-dat$M2*dat$N.bar
    dat$DM2<-dat$M2*dat$N.bar
    dat$DF<-dat$F*dat$N.bar
    dat$DZ<-dat$Z*dat$N.bar
    dat$Nwest<-dat$N*dat$west
    dat$Cweca<-dat$C*dat$weca

    dat<-aggregate(cbind(DM,DM1,DM2,DF,DZ,N,C,Nwest,Cweca,Yield,CWsum,BIO,SSB)~Species+Year+Quarter+Species.n+Age,sum,na.rm=T,data=dat)
    dat$Z<- -log((dat$N-dat$DZ)/dat$N)
    dat$M<-dat$DM/dat$DZ*dat$Z
    dat$M1<-dat$DM1/dat$DZ*dat$Z
    dat$M2<-dat$DM2/dat$DZ*dat$Z
    dat$F<-dat$DF/dat$DZ*dat$Z

    dat$weca<-dat$Cweca/dat$C
    dat[is.na(dat$weca),'weca']<-0
    dat$west<-dat$Nwest/dat$N
   }

  dat$N.bar<-dat$N*(1-exp(-dat$Z))/dat$Z
  dat$C<-NULL
  dat$N_dist<-NULL
  dat$Area<-NULL
  dat1<-subset(dat1,select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,BIO,SSB))
  dat <-subset(dat, select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,BIO,SSB))

  dat<-rbind(dat1,dat)
}
#tapply(dat$Yield,list(dat$Year,dat$Species),sum)


dat<-subset(dat,Year<=last.year )
#(subset(dat,Year==2011 & Species=='Cod'))
if (first.year>0) dat<-subset(dat,Year>=first.year )
if (incl.M2.plot) {
  dat<-data.frame(dat,deadM1=dat$M1*dat$N.bar*dat$west,deadM2=dat$M2*dat$N.bar*dat$west,deadM=dat$M*dat$N.bar*dat$west)
 }

 

plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=11, height=8, pointsize=12)
  if (dev=='wmf') win.metafile(filename = file.path(output.dir,paste(out,'.wmf',sep='')), width=8, height=10, pointsize=12)
  if (dev=='png') png(filename =file.path(output.dir,paste(out,'.png',sep='')), width = 1600, height = 1200,units = "px", pointsize = 30, bg = "white")
  if (dev=='pdf') pdf(file =file.path(output.dir,paste(out,'.pdf',sep='')), width = 8, height = 10,pointsize = 12,onefile=FALSE)
}


for (sp in (first.VPA:nsp)){
    sp.name<-sp.names[sp]
    discard<-SMS.control@discard[sp-first.VPA+1]==1

    plotfile(dev=My.device,out=paste(file.name,'_',sep=''));
    par(mfcol=c(nox,noy))

    
    par(mar=c(3,4,3,2))

    s<-subset(dat,Species.n==sp)
    av.F.age<-SMS.control@avg.F.ages[sp-first.VPA+1,]
    s1<-subset(s,s$Age>=av.F.age[1] & s$Age<=av.F.age[2])
    FI<-tapply(s1$F,list(s1$Year),sum)/(av.F.age[2]-av.F.age[1]+1)
    s1<-subset(s,weca>=0 )

    Yield<-tapply(s1$Yield,list(s1$Year),sum)/1000
    SOP<-tapply(s1$CWsum,list(s1$Year),sum)/1000
    if (discard)  catch<-rbind(SOP-Yield,Yield) else catch<-Yield
    s1<-subset(s,Quarter==1)
    ssb<-tapply(s1$SSB,list(s1$Year),sum)/1000
    s2<-subset(s,Age==fa & Quarter==SMS.control@rec.season)
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
    #plot(year,rec,type='h',lwd=5,xlab='',ylab='billions',main=paste('Recruitment age',fa),ylim=c(0,max(rec)))
    barplot(rec,space=1,xlab='',ylab='billions',main=paste('Recruitment age',fa),ylim=c(0,max(rec)))
    F.max<-max(FI,ref[sp,"Flim"])

    #cat(sp.name,round(min(FI),2),round(quantile(FI,0.1),2),round(max(FI),2),'\n')
    #print(year)
    #print(FI)
    plot(year,FI,type='b',lwd=3,xlab='',ylab='',main="Fishing mortality",ylim=c(0,F.max))
   # tmp<-paste('(',av.F.age[1],'-',av.F.age[2],')',sep='')
   # plot(year,FI,type='l',lwd=3,xlab='',ylab='',main=expression(bar(F)),ylim=c(0,F.max))

    if (incl.reference.points) if (ref[sp,"Flim"]>0) abline(h=ref[sp,"Flim"],lty=2,lwd=2)
    if (incl.reference.points) if (ref[sp,"Fpa"]>0) abline(h=ref[sp,"Fpa"],lty=3,lwd=2)
    grid()
    
    Blim<-ref[sp,"Blim"]/1000; Bpa<-ref[sp,"Bpa"]/1000
    SSB.max<-max(ssb,Bpa)
    plot(year.ssb,ssb,type='b',lwd=3,xlab='',ylab='1000 tonnes',main='SSB',ylim=c(0,SSB.max))
    if (incl.reference.points) if (Blim>0) abline(h=Blim,lty=2,lwd=2)
    if (incl.reference.points) if (Bpa>0) abline(h=Bpa,lty=3,lwd=2)
    grid()
    if (incl.M2.plot) {
      deadM1<-tapply(s$deadM1,list(s$Year),sum)/1000
      deadM2<-tapply(s$deadM2,list(s$Year),sum)/1000
      deadM1<-deadM1[1:length(SOP)]
      deadM2<-deadM2[1:length(SOP)]
      barplot(rbind(SOP,deadM1,deadM2),space=1,main='Biomass removed\ndue to F, M1 and M2',ylab='1000 tonnes')

      b<-tapply(s$M2,list(s$Year,s$Age),sum)
      b<-b[1:length(SOP),]
      if (sum(b,na.rm=T)>=0.01) {
        y<-as.numeric(dimnames(b)[[1]])
        plot(y,b[,1],main=paste("M2 at age"),xlab="",ylab='M2',
                type='l',lwd=1.5,ylim=c(0,max(b,na.rm=T)))
        for (a in (2:(dim(b)[2]))) if(max(b[,a],na.rm=T)>0.001) lines(y,b[,a],lty=a,col=a,lwd=2)
      }
     }
   if (My.device =='screen') savePlot(filename= file.path(mdDir,"Summary_.png"),type="png")
     if (My.device %in% c('png','wmf','pdf')) cleanup()
}
