
forecast_multivariate_TAC<-function(TAC.year=2016,n=1000,wr.resul=FALSE,Fvar="log_exp_pattern",read.NF=FALSE,rec.mode='SR',burn.in=0,target.TAC=0) {
  # extract covariance or correlation matrix
  
  Blim<-Read.reference.points()[,'Blim']
  la<-SMS.control@species.info[1,1]
  nage<-la-fa+1
  
 
  if (!read.NF) {
   fit<-read.fit()
  
       getCorCov<-function(vname,fit){
          idx<-which(fit$names %in% vname)
          COR<-fit$cor[idx,idx]
          COV<-fit$cov[idx,idx]
          labels<-paste(fit$names[idx],paste("sp",fit$species[idx],sep=''),paste("Age",fit$age[idx],sep=''),sep='.')
          dimnames(COR)<-list(labels,labels)
          dimnames(COV)<-list(labels,labels)
          val<-fit$est[idx]
          return(list(cov=COV,cor=COR,val=val,species=fit$species[idx],age=fit$age[idx],vari=fit$names[idx]))
       }
  
   #a<-getCorCov(vname=c("term_logN_next","exploi_pattern"),fit=fit)
   if (Fvar=="log_exp_pattern") a<-getCorCov(vname=c("term_logN_next","log_exp_pattern"),fit=fit) else    a<-getCorCov(vname=c("term_logN_next","exploi_pattern"),fit=fit)
   
   COV<-a$cov
   est<-a$val  # estimate used to for COV

   
  if (rec.mode=='Fixed') {
    COV<-COV[-1,-1]  # delete N age 0
    est<-est[-1]
    
    # read assessment summary file
    s<-Read.summary.data(extend=TRUE)

    #recruitment geometric mean
    tmp<-subset(s,Year < (TAC.year-1) & Quarter==SMS.control@rec.season & Age==SMS.control@first.age,select=c(Year,N))
    recruit.TAC.year<-mean(log(tmp$N))  # GM
  }
  
  
    trellis.device(device = 'png',file = file.path(data.path,paste('Met_3_covariance_',Fvar,rec.mode,TAC.year,'.png',sep='')))
    print(levelplot(a$cor,xlab='',ylab='',main=paste(SMS.control@species.names," Sandeel. ",TAC.year),at=seq(-1,1,0.05)))
    #savePlot(filename = file.path(data.path,paste('Met_3_covariance_',Fvar,rec.mode,TAC.year,sep='_')), type = "png")
    cleanup()
  
    # get the values of F at age in the terminal year, and N  at age in the Terminal Year+1
    set.seed(2)
    x<-mvrnorm(n=n, mu=est, Sigma=COV,tol=1e-5)
    if (rec.mode=='Fixed') x<-cbind(rep(recruit.TAC.year,dim(x)[[1]]),x)
    lab.burn.in<-'_'
  
  } else {  # read MCMC data from SMS
   a<-matrix(scan(file=file.path(data.path,'short_term_NF.out'), comment.char = "#"),byrow=TRUE,ncol=2+nage*3)
   x<-a[,3:dim(a)[[2]]]
   if (burn.in>0) {
     dim.x<-dim(x)[[1]]
     if (dim.x>burn.in) x<-x[burn.in:dim.x,]
     lab.burn.in<-'Burn_in'
   } else lab.burn.in<-'_'
   
   n<-dim(x)[[1]]
   #apply(x,2,summary)
  }
  
  ####
  if(read.NF) met<-4 else met<-3
  a<-read.csv(file=file.path(data.path,paste('forecast_input_determenistic_',TAC.year,'.csv',sep='')))
  
   
  a<-as.matrix(a[,2:(1+nage)])
  a[is.na(a)]<-0
  
  ws<-a[4,]/1000
  wc<-a[5:6,]/1000
  M<-a[9:10,]
  PM<-a[8,]
  
  N.ini<-matrix(x[,(fa:la)+1],ncol=nage,byrow=FALSE)
  if (!read.NF) N.ini<-exp(N.ini)
  cat(paste('\n\nMet',met,Fvar,rec.mode,TAC.year,'\n',sep='_'))
  print(apply(N.ini,2,summary)/1000)
  
  
  exploi.1<-matrix(x[,nage+(fa:la)+1],ncol=nage,byrow=FALSE)
  exploi.1[,1]<-0
  exploi.2<-matrix(x[,2*nage+(fa:la)+1],ncol=nage,byrow=FALSE)
  
  if (Fvar=="log_exp_pattern" & read.NF==FALSE) {
    exploi.1<-exp(exploi.1)
    exploi.2<-exp(exploi.2)
  }
  
  Fbar<-apply(exploi.1[,2:3]+exploi.2[,2:3],1,sum)/2
  exploi.1<-exploi.1/Fbar
  exploi.2<-exploi.2/Fbar
  
  NF<-cbind(N.ini,exploi.1,exploi.2)
  colnames(NF)<-c(paste('N',fa:la,sep='_'),paste('Ffirst',fa:la,sep='_'),paste('Flast',fa:la,sep='_'))
  
  Nrange<-(fa:la)+1
  F1range<-max(Nrange)+Nrange
  F2range<-max(F1range)+Nrange
  
  if (Fvar!="log_exp_pattern") {
    incl<-(apply(NF[,c(F1range,F2range)],1,min)>=0)
    NF<-NF[incl,]
  }
  n<-dim(NF)[[1]]
  
  #apply(NF,2,summary)
  
  quickAndDirty<-function(mul=1,NF) {
    FF1<-mul*NF[F1range]
    Z<-FF1+M[1,]
    
    Nbar<-NF[Nrange]*(1-exp(-Z))/Z
    Nbar[1]<-0 # not recruited yet
    
    catch1<-sum(Nbar*FF1*wc[1,])
  
    # second half year
    N<-NF[Nrange]*exp(-Z)
    N[1]<-NF[1]  # recruits comming in
    FF2<-mul*NF[F2range]
    Z<-FF2+M[2,]
    Nbar<-N*(1-exp(-Z))/Z
    catch2<-sum(Nbar*FF2*wc[2,])
    
    #new year
    N<-N*exp(-Z) #before birthday
    N[nage]<-N[nage-1]+N[nage] #plusgroup
    N[2:(nage-1)]<-N[1:(nage-2)]
    N[1]<-0  # recruits, not born yet 
    
    SSB<-sum(N*ws*PM)
    return(c(mul,catch1+catch2,SSB))
  }
  
  # quickAndDirty(mul=0.5,NF=NF[1,])
  
  
  SSB.from.TAC<-function(TAC=1000) {
    
    res<-matrix(0,ncol=3,nrow=n)
    colnames(res)=c('F','Yield','SSB')
    
    calc.fmul<-function(iter=1,target) {
    
        mini <- function(x) {
          a<-quickAndDirty(mul=x,NF=NF[iter,]) 
         (a[2]-target)^2
      }
      res <- optimize(f=mini,interval=c(0,2) )
      quickAndDirty(mul=res$minimum,NF=NF[iter,]) 
    }
  
    for (iter in (1:n)) res[iter,]<-calc.fmul(iter=iter,target=TAC) 
    return(res)
  }
  
  if (FALSE) {
    b<-SSB.from.TAC(TAC=100000)
    apply(b,2,summary)
    quantile(b[,'SSB'],0.05)
    hist(b[,'SSB'])
    hist(b[,'Yield'])
    hist(b[,'F'])
  }
  
  mini2 <- function(x) {
    a<-SSB.from.TAC(TAC=x)
    (quantile(a[,'SSB'],0.05)-Blim)^2
  }
  
  b<-SSB.from.TAC(TAC=0)
  cat('5 th Quantile of SSB: ',quantile(b[,'SSB'],0.05),'  Blim:',Blim, ' ~TAC:',target.TAC,'\n')
  
  if (target.TAC>0) max.TAC<-max(target.TAC*3,100000) else max.TAC<-min((quantile(b[,'SSB'],0.05)-Blim)*5,1E6)
  
  if (quantile(b[,'SSB'],0.05) >Blim) {
    cat('Max TAC: ',max.TAC,'\n')
    a <- optimize(f=mini2,interval=c(0,max.TAC) )
    res<-SSB.from.TAC(TAC=a$minimum)
  } else {
     res<-b
     res[,'Yield']<-0  # to avoid very small catches
     res[,'F']<-0
  }

  newplot(dev="screen",nox=1,noy=3,Portrait=T);
  par(mar=c(5,4,4,1)+.1)
  d<-density(res[,'SSB']/1000); 
  plot(d,main=paste(SMS.control@species.names," Sandeel. ",TAC.year,'\nSSB'),xlab='SSB (1000 t)')
  abline (v=quantile(res[,'SSB']/1000,0.05),lty=2,lwd=2,col='red' )
  abline (v=Blim/1000,lty=3,lwd=2,col='blue' )
  if (var(res[,'F'])>0) { d<-density(res[,'F']); plot(d,main='Fishing mortality', xlab=('F')) }
  if (var(res[,'Yield'])>0) { d<-density(res[,'Yield']/1000); plot(d,main='Yield', xlab=('Yield (1000 t)'))}
  if (read.NF) Fvar<-"MCMC_NF"
  
  savePlot(filename = file.path(data.path,paste('Met',met,Fvar,rec.mode,lab.burn.in,TAC.year,sep='_')), type = "png")
  

  #cat(file=resulFile,'TACyear TAC ProbBlim SSB F Method\n')
  if (wr.resul) cat(TAC.year,median(res[,'Yield']),sum(res[,'SSB']<Blim)/dim(res)[[1]],median(res[,'SSB']),median(res[,'F']) ,paste('Met',met,'TAC',Fvar,rec.mode,lab.burn.in,sep='_'),'\n',file=resulFile,append=TRUE)
}
# forecast_multivariate_TAC(TAC.year=2016,n=1000,wr.resul=FALSE) 
#forecast_multivariate_TAC(TAC.year=2015,n=1000,wr.resul=FALSE,read.NF=TRUE) 