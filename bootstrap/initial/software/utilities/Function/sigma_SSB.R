sigma_SSB<-function() {
  a<-Read.SMS.std()
  a1<-subset(a,name %in% c('next_log_SSB') | (name=='hist_SSB' & y==year),select=c( name,value,CV.round,std,year))
  a1$sigma<-0
  
  a1[a1$name=='next_log_SSB','sigma']<-a1[a1$name=='next_log_SSB','std']
  a1[a1$name=='hist_SSB','sigma']<-a1[a1$name=='hist_SSB','CV.round']/100
  a1$std<-NULL; a1$CV.round<-NULL; a1$value<-NULL
  a1
  
  a2<-subset(a,name %in% c('log_short_term_SSB'),select=c( name,value,CV.round,std,year,area))
  mean.FF<-my.scale.options*mean.F
  b<-data.frame(area=1:length(mean.FF),FF=mean.FF)
  ab<-merge(a2,b)
  
  ab$sigma<-0
  
  ab[ab$name=='log_short_term_SSB','sigma']<-ab[ab$name=='log_short_term_SSB','std']
   head(ab)
  
  ab<-subset(ab,select=c(name ,value,sigma,year,FF))
  sigma.min<-ab[which(round(b$FF,2)==0),'sigma']
  sigma.max<-ab[which(round(ab$FF,2)==Fcap),'sigma']
  
  aa<-data.frame(name=c('1. Last assessment year','2. TAC year','3. After TAC year, F=0',
                        paste('4. After TAC year, F=',round(Fcap,2),sep='')),TAC.year=y+1,Year=c(y:(y+2),y+2))
  aa$sigma<-c(a1[a1$name=='hist_SSB','sigma'],a1[a1$name=='next_log_SSB','sigma'],sigma.min,sigma.max)
  write.table(aa,file=SSBsigmaFile,append=TRUE,col.names=FALSE,row.names=FALSE)
 
}