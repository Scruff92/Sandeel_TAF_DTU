
determenistic_forecast_F<-function(TAC.year=2016,scale.options=c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2),wr.resul=FALSE,rescale.F=FALSE,roundTAC=4, SSBescapement= SSBescapement) {
 
  
  Recruit.in.Assess.year<- 0    #   0 = Use value estimated from assessment (default)
  
    #  user options according to stock annex, Please keep unchanged. 
    PM.TAC.year<-TAC.year 
    #PM.after.TAC.year<-seq(1983,2000,1)                       # Year or year Range for Proportion Mature for the TAC year
    PM.after.TAC.year<-seq(1983,TAC.year-1,1)         # Year or year Range for Proportion Mature for the year after TAC year  (default long term average)
    west.TAC.year<-seq(TAC.year-3,TAC.year-1,1) # Year or year Range for weight in the Sea  for the TAC year  (default the 3 most recent years ~ seq(TAC.year-3,TAC.year-1,1))
    weca.TAC.year<-seq(TAC.year-3,TAC.year-1,1) # Year or year Range for weight in the Catch  for the TAC year (default the 3 most recent years ~seq(TAC.year-3,TAC.year-1,1))
    NatMor.year<-TAC.year-1                      # Year or year Range for Natural mortality (M) (default the most recent year)
    
    ## end user options,
    
    
    ############
    
    # read assessment summary file
    s<-Read.summary.data(extend=TRUE)
    
    #recruitment geometric mean
    tmp<-subset(s,Year < (TAC.year-1) & Quarter==SMS.control@rec.season & Age==SMS.control@first.age,select=c(Year,N))
    recruit.TAC.year<-exp(mean(log(tmp$N)))
    rec.Y.min<-min(tmp$Year); if (rec.Y.min>=2000) rec.Y.min<-rec.Y.min-2000 else rec.Y.min<-rec.Y.min-1900  
    rec.Y.max<-max(tmp$Year);if (rec.Y.max>=2000) rec.Y.max<-rec.Y.max-2000 else rec.Y.max<-rec.Y.max-1900 
    rec.string<-paste(formatC(rec.Y.min,format='d',flag='0',width=2),'-',formatC(rec.Y.max,format='d',flag='0',width=2),sep='')
    
    Yield.assess<-sum(subset(s,Year %in% (TAC.year-1),select=c(Yield)))/1000
    
    tmp<-subset(s,Year %in% PM.TAC.year & Quarter==1,select=c(Year,Age,propmat))
    PM.TAC<-c(0,tapply(tmp$propmat,list(Age=tmp$Age),mean))
    
    tmp<-subset(s,Year %in% PM.after.TAC.year & Quarter==1,select=c(Year,Age,propmat))
    PM.after.TAC<-c(0,tapply(tmp$propmat,list(Age=tmp$Age),mean))
    
    tmp<-subset(s,Year %in% west.TAC.year,select=c(Year,Age,Quarter,west))
    west.TAC<-tapply(tmp$west,list(Age=tmp$Age,season=tmp$Quarter),mean)
    
    tmp<-subset(s,Year %in% weca.TAC.year,select=c(Year,Age,Quarter,weca))
    weca.TAC<-tapply(tmp$weca,list(Age=tmp$Age,season=tmp$Quarter),mean)
    
    tmp<-subset(s,Year %in% NatMor.year,select=c(Year,Age,Quarter,M))
    M<-tapply(tmp$M,list(Age=tmp$Age,season=tmp$Quarter),mean)
    
    tmp<-subset(s,Year %in% (TAC.year-1),select=c(Year,Age,Quarter,F))
    FF<-tapply(tmp$F,list(Age=tmp$Age,season=tmp$Quarter),mean)
    
    if (rescale.F) {  
      a<-SMS.control@avg.F.ages
      avgF<-sum(FF[as.character(a[1]:a[2]),])/(a[2]-a[1]+1)
      FF<-FF/avgF
    }
    tmp<-subset(s,Year %in% TAC.year & Quarter==1,select=c(Year,Age,N))
    N.TAC<-tapply(tmp$N,list(Age=tmp$Age),sum)
    
    if (my.stock.dir=='SAN-area-3-2015-retro') N.TAC[1]<-N.TAC[1]*0.695
    N.TAC<-c(recruit.TAC.year,N.TAC)
   
    No.recruits.in.assessment.year<-subset(s,Year %in% (TAC.year-1) & Quarter==2 & Age==0,select=c(N))$N
    
    outTab<-cbind(N.TAC/1000,FF,west.TAC[,1]*1000,weca.TAC*1000,PM.TAC,PM.after.TAC,M) 
    dimnames(outTab)[1]<-list(paste("Age",seq(0,4,1)))
    dimnames(outTab)[2]<-list(c(paste('Stock numbers(',TAC.year,')',sep=''),'Exploitation pattern 1st half', 'Exploitation pattern 2nd half','Weight in the stock 1st half','Weight in the catch 1st half','weight in the catch 2nd half',
                                 paste('Proportion mature(',TAC.year,')',sep=''),paste('Proportion mature(',TAC.year+1,')',sep=''),'Natural mortality 1st half','Natural mortality 2nd half'))
     write.csv(t(outTab),file=file.path(data.path,paste('forecast_input_determenistic_',TAC.year,'.csv',sep='')))
  
    
    if (FALSE) {
      colnames(outTab)<-paste(colnames(outTab),c('(millions)',' ',' ','(gram)','(gram)','(gram)',' ',' ',' ',' '))
      xtab3(t(outTab), caption=paste("Table XXX.  ",SMS.control@species.names," Sandeel. input to forecast",sep=''),
           cornername='Age',
         file=file.path(data.path,'_forcast_input_determenistic.html'), dec=c(0,3,3,2,2,2,2,2,2,2), width='"100%"',units=NULL)
    }
    

    f.age<-1   # f.age is always 1
    l.age<-SMS.control@max.age.all+1  
    plus.group<-1   #1=yes, 0=no
    
    f.season<-1
    l.season<-SMS.control@last.season
    rec.season<-SMS.control@rec.season      
    
    
    N.TAC<-cbind(N.TAC,N.TAC)
    N.TAC[1,1]<-0
    N.TAC[(f.age+1):l.age,2]<-0
    dimnames(N.TAC)<-list(paste("Age",seq(f.age-1,l.age-1,1)),c('1','2'))
    
    west.TAC[is.na(west.TAC)]<-0
    weca.TAC[is.na(weca.TAC)]<-0
    FF[is.na(FF)]<-0
    M[is.na(M)]<-0
    
  
    ####################### Functions #############################
    
    # move the N forward to the end of the year
    move.on<-function(N,M,f){
        Z<-M+f
        C<-Z   #copy structure
        for (q in (f.season:l.season)) {
          for (a in (f.age:l.age)){
            if (Z[a,q]>0) C[a,q]<-f[a,q]*N[a,q]*(1-exp(-Z[a,q]))/Z[a,q]
            if (q<l.season) { 
               if (!(q<rec.season & a==f.age)) N[a,q+1]<-N[a,q]*exp(-Z[a,q]) 
            }
          }
        }
       list(N,C)
    }
    
     
    birthday<-function(N,Z,PM) {
      N[,1]<-0
      for (a in (l.age:(f.age+1))){
       if (a==l.age && plus.group==1) N[a,f.season]<-N[a-1,l.season]*exp(-Z[a-1,l.season])+N[a,l.season]*exp(-Z[a,l.season])
       if (a<l.age || plus.group==0) N[a,f.season]<-N[a-1,l.season]*exp(-Z[a-1,l.season])
      }
      N[,(f.season+1):l.season]<-0  
      SSB0<-sum(N*PM*WEST)
      list(N,SSB0)
    }
    
    #
    do.prediction<-function(fmult,RTM,F.default){   
        f<-F.default*fmult
        N<-N.ini
        if (RTM>0) N[2,1]<-RTM*exp(-M[1,rec.season])  # ignore Fishing mortality
        res<-move.on(N,M,f)
        N<-res[[1]]
        C<-res[[2]]
        TAC<<-sum(C*WECA,rm.na=T)/1000
        # longterm data 
        res<-birthday(N,M+f,PM.after.TAC)
        res[[2]]
    }
    
    #
    calc.fterm<-function(RTM,target.SSB,F.default) {
      mini <- function(x) {
        a<-do.prediction(fmult=x,RTM,F.default) 
        (a[[1]]-target.SSB)^2
      }
      res <- optimize(f=mini,interval=c(0,10) )
      res
    }
    
    ######## end functions #############################
    
    n.age<-l.age-f.age+1
    n.year<-2
    
    # initialize data input
    N.ini<-N.TAC
    WECA<-weca.TAC
    WEST<-west.TAC
    
    SSB0<-sum(N.ini[,1]*WEST[,1]*PM.TAC/1000,na.rm=T)
    mean.f<-sum(FF[(SMS.control@avg.F.ages[1,1]+1):(SMS.control@avg.F.ages[1,2]+1),]) /(SMS.control@avg.F.ages[1,2]-SMS.control@avg.F.ages[1,1]+1)
   
    for (fmult in scale.options) {    
        SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000
    
        if (fmult==scale.options[1]) string<-"F=0," else string<-paste('Fsq*',round(fmult,2),',',sep='')
        if (fmult==scale.options[1]) cat(paste('F multiplier,Basis,F(',TAC.year,'),Catch(',TAC.year,'),SSB(',TAC.year+1,'),%SSB change*,%TAC change**\n',sep=''))
        cat(paste(round(fmult,2),',',string,round(mean.f*fmult,3),',',round(TAC,roundTAC),',',
                  round(SSB1),',', round((SSB1-SSB0)/SSB0*100),'%,', round((TAC-Yield.assess)/Yield.assess*100),'%','\n'))
     }
    
     # MSY, find F 
      res<-calc.fterm(RTM=Recruit.in.Assess.year,target.SSB=SSBescapement,F.default=FF)
      fmult<-res$minimum
      obj<-res$objective
      if (obj<10) {
          SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000  
          #cat(paste(round(fmult,3),',',"MSY",',',round(mean.f*fmult,3),',',round(TAC,roundTAC),',',  round(SSB1),',', round((SSB1-SSB0)/SSB0*100),'%,', round((TAC-Yield.assess)/Yield.assess*100),'%','\n'))
          if (wr.resul) cat(TAC.year,TAC*1000,' NA ',SSB1*1000, mean.f*fmult,paste('Met_1_SSB.escapement=',round(SSBescapement/1000),sep=''),'\n',file=resulFile,append=TRUE)
          if (mean.f*fmult>Fcap) {
            fmult<-Fcap/mean.f
            SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000  
            if (wr.resul) cat(TAC.year,TAC*1000,' NA ',SSB1*1000, mean.f*fmult,paste('Met_1_capped_SSB.escapement=',round(SSBescapement/1000),sep=''),'\n',file=resulFile,append=TRUE)
            
          }
          
      } else {
        cat('No conversion for calculation of MSY catch')
        SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)
        if (wr.resul) cat(TAC.year,0,' NA ',SSB1, 0,paste('Met_1_SSB.escapement=',round(SSBescapement/1000),sep=''),'\n',file=resulFile,append=TRUE)
        
      }
  }
    

  #determenistic_forecast_F(TAC.year=2017 )
