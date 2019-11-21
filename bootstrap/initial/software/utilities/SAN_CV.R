Arith_log <- c("Arithmetric","Log values")[2]  #select assumed distribution of variables
include.last.assessment.year.recruit<-T       # should be T when the dregde survey data are availeble
include.TAC.year.SSB<-T                      # should the SSB plot include the SSB for the fisrt year after the assessment year (default TRUE)

confidence<- 0.90  # 90% confidence limits

two<-qt(1-(1-confidence)/2, df = 10000)  # plus minus factor to get confidence interval
#cleanup()

plot.CV<-function(paper=FALSE,nox=1,noy=1,var.name='hist_SSB',newPlot=T,tit=T) {
  tit<-F
  if (paper) dev<-"png" else dev<-"screen"
  noxy<-nox*noy
  
  if (plot.ref.lines) ref<-Read.reference.points()

  tmp<-Read.SMS.std()
  #sort(unique(tmp$name))
  if (Arith_log == "Arithmetric") tmp$name[tmp$name=="next_SSB"]<-'hist_SSB'  else tmp$name[tmp$name=="next_log_SSB"]<-'hist_log_SSB'

  a<-subset(tmp,name==var.name & species>0 ,drop=TRUE)
  a$Species<-sp.names[a$species]
  
  if (var.name %in% c('hist_SSB','hist_log_SSB'))  ytitl<-"SSB (1000 t)"  else if (var.name %in% c('avg_sumF','avg_log_sumF')) ytitl<-"Average F"  else if (var.name %in% c('rec_sd','log_recsd')) ytitl<-"Recruitment (10^6)" else if (var.name=='M2_sd0' | var.name=='M2_sd1') ytitl<-"M2"  else stop("name must be hist_SSB or avg_F")
  
  if (var.name=='rec_sd') {
      a$value<-a$value/1000
      a$std<-a$std/1000
      if (include.last.assessment.year.recruit==F) {
        a<-subset(a,year!=SMS.control@last.year.model)
      } 
  }
  
  if (var.name=='log_recsd') {
    a$value<- a$value-log(1000)
    if (include.last.assessment.year.recruit==F) {
      a<-subset(a,year!=SMS.control@last.year.model)
    } 
  }
  
  if (var.name=='hist_SSB') {
      a$value<-a$value/1000
      a$std<-a$std/1000
      if (!include.TAC.year.SSB) {
        maxy<-max(a$year)
        a<-subset(a,year<=maxy)
      }
  }
 
  if (var.name=='hist_log_SSB') {
    a$value<-a$value-log(1000)
    if (!include.TAC.year.SSB) {
      maxy<-max(a$year)
      a<-subset(a,year<=maxy)
    }
  }
  
  if (newPlot) newplot(dev=dev,nox,noy,filename=var.name,w8=10,w11=8);
       par(mar=c(3,5,1,2))  #c(bottom, left, top, right)
 
  by(a,list(a$species),function(x) {
    if (Arith_log == "Arithmetric") {
      minval<-min(x$value-2*x$std*2,0)  # plotting y-axis range
      maxval<-max(x$value+1.25*x$std*2) # plotting y-axis range
      plot( x$year,x$value,xlab='',ylab=ytitl,ylim=c(minval,maxval),type='b',xlim=c(1980,2020))
      if (tit) title(main=x[1,]$Species)
      lines(x$year,x$value-two*x$std,lty=2)
      lines(x$year,x$value+two*x$std,lty=2)
    } else if (Arith_log == "Log values") {
      print(x)
      minval<-min(exp(x$value-x$std*two),0)
      maxval<-1.1*max(exp(x$value+x$std*two))
      cat(minval,maxval,'\n')
      plot( x$year,exp(x$value),xlab='',ylab=ytitl,ylim=c(minval,maxval),type='b',xlim=c(1980,2020))
      if (tit) title(main=x[1,]$Species)
      lines(x$year,exp(x$value-two*x$std),lty=2)
      lines(x$year,exp(x$value+two*x$std),lty=2)
    }
     if (plot.ref.lines) {
       sp<-x$species[1]
       if (var.name %in% c('avg_sumF','avg_log_sumF')) {
        if (ref[sp,"Flim"]>0) abline(h=ref[sp,"Flim"],lty=2,lwd=2)
        if (ref[sp,"Fpa"]>0) abline(h=ref[sp,"Fpa"],lty=3,lwd=2)
       }
       if (var.name %in% c('hist_SSB','hist_log_SSB')) {
          Blim<-ref[sp,"Blim"]/1000; Bpa<-ref[sp,"Bpa"]/1000
          if (Blim>0) abline(h=Blim,lty=2,lwd=2)
          if (Bpa>0) abline(h=Bpa,lty=3,lwd=2)
       }
     }  
  })  
  if (dev!="screen") cleanup();
}

plot.ref.lines<-T
if (Arith_log == "Arithmetric") {
  plot.CV(paper=FALSE,nox=1,noy=3,var.name='avg_sumF',newPlot=T,tit=T) 
  plot.CV(paper=FALSE,nox=1,noy=1,var.name='hist_SSB',newPlot=F,tit=F)  
  plot.CV(paper=FALSE,nox=1,noy=1,var.name='rec_sd',newPlot=F,tit=F) 
  savePlot(filename= file.path(mdDir,"CV_plot1_arihtm.png"),type="png")
} else if (Arith_log == "Log values") {
  plot.CV(paper=FALSE,nox=1,noy=3,var.name='avg_log_sumF',newPlot=T,tit=T) 
  plot.CV(paper=FALSE,nox=1,noy=1,var.name='hist_log_SSB',newPlot=F,tit=F)  
  plot.CV(paper=FALSE,nox=1,noy=1,var.name='log_recsd',newPlot=F,tit=F) 
  savePlot(filename= file.path(mdDir,"CV_plot1.png"),type="png")
}

####################################
tmp<-Read.SMS.std()
tmp$Species<-sp.names[tmp$species]

# tmp[grepl('s2',tmp$name),]

# subset(tmp,name=="GMminOne")
# sort(unique(tmp$name))




if (Arith_log == "Arithmetric") {
  tmp$name[tmp$name=="next_SSB"]<-'hist_SSB'
  a<-subset(tmp,name %in% c("hist_SSB","avg_sumF","rec_sd") ,drop=TRUE) 
} else if (Arith_log == "Log values"){
 # tmp$name[tmp$name=="GMminOne"]<-'log_recsd'
  tmp$name[tmp$name=="next_log_SSB"]<-'hist_log_SSB'
  a<-subset(tmp,name %in% c("hist_log_SSB","avg_log_sumF","log_recsd")) 
}

if (Arith_log == "Arithmetric") a<-data.frame(Species=sp.names[a$species],Year=a$year, variable=a$name,value=a$value,std=a$std,CV=a$std/a$value*100)
if (Arith_log == "Log values")  a<-data.frame(Species=sp.names[a$species],Year=a$year, variable=a$name,value=a$value,std=a$std,CV=a$std)

if (include.last.assessment.year.recruit==F) {
        a<-subset(a,!(Year==SMS.control@last.year.model & variable=='rec_sd'))
} 
               
a$titl<- ifelse (a$variable %in% c('hist_SSB','hist_log_SSB'),"SSB", ifelse(a$variable %in% c('avg_sumF','avg_log_sumF'),"Average F",ifelse(a$variable %in% c('rec_sd','log_recsd'), "Recruitment",'error')))

if (Arith_log == "Arithmetric") ylab<-'Coefficient of Variation (%)'  else ylab<-"standard deviation of log values (sigma)"             
# cleanup() 
 trellis.device(device = "windows",
               color = F, width=8, height=8,pointsize = 12,
               new = TRUE, retain = FALSE)


print(xyplot( CV~Year|titl,groups=Species, data=a,  subset=Year>=1983,
  type='b',lwd=1 , layout=c(1,3), ylab=ylab,
   between = list(y = c(1, 1),x = c(1, 1)),
   strip = strip.custom( bg='white'),par.strip.text=list(cex=0.9, lines=1.7),

    scales = list(x = list( cex=0.8,relation='same'), y= list(cex=0.8),alternating = 1,relation='free')
))

savePlot(filename= file.path(mdDir,"CV_plot2.png"),type="png")
if (My.device=='png') {
 cleanup()
}

if (Arith_log == "Arithmetric") {
 a$upper<-a$value+two*a$std
 a$lower<-a$value-two*a$std
 years<-sort(unique(a$Year))
 b<-matrix(NA,ncol=9,nrow=length(years))
 rownames(b)<-as.character(years)
 colnames(b)<-c('Recruitment','High','Low','SSB','High','Low','F ages 1-2','High','Low')
 
 aa<-aggregate(cbind(value,upper,lower)~Year+titl,data=a,sum)
 a2<-as.matrix(subset(aa,titl=='Recruitment',select=c(value,upper,lower)),ncol=3)
 b[1:dim(a2)[[1]],1:3]<- a2
 a2<-as.matrix(subset(aa,titl=='SSB',select=c(value,upper,lower)),ncol=3)
 b[1:dim(a2)[[1]],4:6]<- a2
 a2<-as.matrix(subset(aa,titl=='Average F',select=c(value,upper,lower)),ncol=3)
 b[1:dim(a2)[[1]],7:9]<- a2
 b
}


if (Arith_log == "Log values") {
  a$upper<-exp(a$value+two*a$std)
  a$lower<-exp(a$value-two*a$std)
  a$value<-exp(a$value)
  years<-sort(unique(a$Year))
  b<-matrix(NA,ncol=9,nrow=length(years))
  rownames(b)<-as.character(years)
  colnames(b)<-c('Recruitment','High','Low','SSB','High','Low','F ages 1-2','High','Low')
  
  aa<-aggregate(cbind(value,upper,lower)~Year+titl,data=a,sum)
  a2<-as.matrix(subset(aa,titl=='Recruitment',select=c(value,upper,lower)),ncol=3)
  b[1:dim(a2)[[1]],1:3]<- a2
  a2<-as.matrix(subset(aa,titl=='SSB',select=c(value,upper,lower)),ncol=3)
  b[1:dim(a2)[[1]],4:6]<- a2
  a2<-as.matrix(subset(aa,titl=='Average F',select=c(value,upper,lower)),ncol=3)
  b[1:dim(a2)[[1]],7:9]<- a2
  b
}


units<-c("(thousands)"," "," ","(tonnes)"," "," ","(per year)"," "," ")
xtab(b, caption=paste("Table 1.",SMS.control@species.names,":   Assessment summary. Recruiment at age 0, SSB and Mean F (sum of half-yearly F).", round(confidence*100),'% confidence limit'), cornername='Year',
     file=file.path(data.path,paste0('_SAN_ICES_',Arith_log,'.html')), dec=c(rep(0,6),rep(3,3)), width='"100%"',units=units)



