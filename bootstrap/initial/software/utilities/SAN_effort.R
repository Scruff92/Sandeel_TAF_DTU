eff<-scan(file.path(data.path,'effort.in'),comment.char = "#", quiet =T)

stopifnot(length(eff) == ((SMS.control@last.year-SMS.control@first.year+1)*SMS.control@last.season))

eff<-matrix(eff,ncol=SMS.control@last.season)
years<-SMS.control@first.year:SMS.control@last.year
rownames(eff)<-years

tit<-paste(SMS.control@species.names,"Sandeel")
Seff<-apply(eff,1,sum)
#cleanup()
fil<-'standardised_effort'
newplot(dev=My.device,filename=fil,nox=1,noy=1,Portrait=F);
plot(years,Seff,type='b',ylab='Standardised effort',col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(Seff)),main=tit)
lines(years,eff[,1],type='b',col=2,lty=2,lwd=2, pch=2)
lines(years,eff[,2],type='b',col=3,lty=3,lwd=2, pch=3)
legend("topright",
    c('Total effort','effort 1st half','effort 2nd half'),
    pch=c(1,2,3),lty=c(1,2,3),lwd=c(3,2,2),col=c(1,2,3))

if (My.device=='screen'){
  savePlot(filename = file.path(mdDir,fil),type = "png")
} else cleanup()

tab1<-cbind(eff,Seff)
ynames<-rownames(tab1)
colnames(tab1)<-c("1st half year","2nd half year", "Sum")
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(ynames,'arith. mean')
xtab(tab1, caption=paste("Table 1.",SMS.control@species.names,"Sandeel. Standardised effort (fishing days for a XX GT vessels)"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_effort.html'), dec=rep(0,dim(tab1)[2]), width='"100%"')

# total yield and CPUE
a<-Read.summary.data()
Yield<-tapply(a$Yield,list(a$Year),sum)
cpue<-Yield/Seff

# total effort and CPUE
cpue[Seff<10]<-NA

fil<-'tonnes_per_stand_day_fishing'
if (My.device!='screen') cleanup()
newplot(dev=My.device,filename=fil,nox=1,noy=1,Portrait=F);
par(mar=c(5,4,4,5)+.1)
plot(as.numeric(names(cpue)),cpue,type='b',ylab='CPUE (tonnes per standardised day fishing)',xlab=' ',col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(cpue, na.rm=T)),main=tit)

legend("topright",
   c('CPUE','Effort'),
   pch=c(1,2),lty=c(1,2),lwd=c(3,2),col=c(1,2))

par(new=T)
plot(as.numeric(names(cpue)),Seff,ylim=c(0,max(Seff, na.rm=T)),axes=F,lwd=3,lty=2,type='b',xlab=' ',ylab='',col=2,pch=2)
axis(side=4)
mtext(side=4,line=3.0,"Standardised Effort")
par(xaxs="r")
if (My.device=='screen'){
  savePlot(filename = file.path(mdDir,fil),type = "png")
} else cleanup()


# annual mean F
a<-Read.summary.data()

a$deadC<-a$N.bar*a$F
a$deadAll<-a$N.bar*a$Z
a<-subset(a,select=c(Year,Age,deadC,deadAll,Z))
a<-aggregate(list(deadC=a$deadC,deadAll=a$deadAll,Z=a$Z),list(Year=a$Year,Age=a$Age),sum)
a$FF<-a$Z*a$deadC/a$deadAll

tab1<-tapply(a$FF,list(a$Year,a$Age),sum)
f<-apply(tab1[,as.character(SMS.control@avg.F.ages[1,1]:SMS.control@avg.F.ages[1,2])],1,mean)

fil<-'Standardised effort'
newplot(dev=My.device,filename=fil,nox=1,noy=1,Portrait=F);

par(mar=c(5,4,4,5)+.1)

plot(years,Seff,type='b',xlab=' ',ylab='Standardised effort',col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(Seff, na.rm=T)),main=tit)
lines(years,eff[,1],type='b',col=2,lty=2,lwd=2, pch=2)
lines(years,eff[,2],type='b',col=3,lty=3,lwd=2, pch=3)

legend("topright",
   c('Total effort','effort 1st half','effort 2nd half','F(1-2)'),
   pch=c(1,2,3,4),lty=c(1,2,3,4),lwd=c(3,2,2,4),col=c(1,2,3,4))

par(new=T)
plot(years,f,ylim=c(0,max(f, na.rm=T)),axes=F,lwd=4,type='b',xlab=' ',col=4,pch=4)
axis(side=4)

mtext(side=4,line=3.0,"Fishing mortality age 1-2")
par(xaxs="r")

if (My.device=='screen'){
  savePlot(filename = file.path(mdDir,fil),type = "png")
} else cleanup()

fil<-'Standardised effort2'
newplot(dev=My.device,filename=fil,nox=1,noy=1,Portrait=F);

par(mar=c(5,4,4,5)+.1)

plot(years,Seff,type='b',xlab=' ',ylab='Standardised effort',col=1,lty=1,lwd=3,pch=1,ylim=c(0,max(Seff, na.rm=T)),main=tit)

legend("topright",
   c('Total effort','F(1-2)'),
   pch=c(1,2),lty=c(1,2),lwd=c(3,3),col=c(1,2))

par(new=T)
plot(years,f,ylim=c(0,max(f, na.rm=T)),axes=F,lwd=3,type='b',xlab=' ',col=2,pch=2)
axis(side=4)

mtext(side=4,line=3.0,"Fishing mortality age 1-2")
par(xaxs="r")
if (My.device=='screen'){
  savePlot(filename = file.path(mdDir,fil),type = "png")
} else cleanup()