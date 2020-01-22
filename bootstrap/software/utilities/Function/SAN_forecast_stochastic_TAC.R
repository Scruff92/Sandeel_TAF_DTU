# run first the  SAN_forecast_stochastic_F.r  scrip to  create the nesesary extensions of the input file west.in, natmor.in, promat.in and weca.in

stochastic_forecast_TAC<-function(TAC.year=2016,scale.options=scale.options<-seq(0,25000,1000),MCMC=10000,MCMC.save=100,wr.resul=FALSE,rec.mode='Fixed',do.detail.plot=FALSE  ) {

  ### make the SMS assessment that produces MCMC parameters (file sms.psv)  
  
  control<-read.FLSMS.control()
  control@read.HCR<-0
  write.FLSMS.control(control,write.multi=F) 
  
  do.a.full.SMS.run(label="mcmc_",                   # label for output
    do.single=T,                    # run SMS in single species mode
    do.hessian=T,                   # Make the Hessian matrix and estimate uncertanties 
    do.MCMC=T,                      # Prepare for MCMC analysis
    mcmc=MCMC,mcsave=MCMC.save,      # Options for MCMS analysis
    do.prediction=F,                # Make a prediction
    pause=F,                        # Make a pause between each stage
    Screen.show=T,                  # show the output on screen, or save it in file
    do.run=T                       # Make the run immidiatly, or just make the batch file for the run
  )               
  
  
  
  # read assessment summary file
  s<-Read.summary.data(extend=TRUE)
  #recruitment geometric mean
  tmp<-subset(s,Year < (TAC.year-1) & Quarter==SMS.control@rec.season & Age==SMS.control@first.age,select=c(Year,N))
  recruit.TAC.year<-exp(mean(log(tmp$N)))
  
  #
  # write the short-term-configuration.dat
  out<-file.path(data.path,'short-term-configuration.dat')
  cat(file=out, paste(length(scale.options),'\n# no of TAC\n'))
  cat(file=out,scale.options,append=T)
  if (rec.mode!='Fixed') recruit.TAC.year<- -1
  cat(file=out,"\n# Recruitment TAC year\n",recruit.TAC.year,'\n',append=T)
    
  ### change the SMS.dat to make stochastic forecast  
  
  fc<-file.copy(from=file.path(root.prog,"program",'sms_October_2016.exe'), to=file.path(data.path,'sms.exe'), overwrite = TRUE)
  cat("\nprog.path:",prog.path,"  data.path:",data.path,' File copied:',fc,'\n')
  
  control<-read.FLSMS.control()
  control@read.HCR<-3
  write.FLSMS.control(control,write.multi=F) 
  do.a.full.SMS.run(label="mcmc2_",  # label for output
    do.single=T,                    # run SMS in single species mode
    do.hessian=T,                   # Make the Hessian matrix and estimate uncertanties 
    do.MCMCeval=T,                  # Make the mceval 
    do.MCMC=F,                      # Prepare for MCMC analysis
    pause=F,                        # Make a pause between each stage
    Screen.show=T,                  # show the output on screen, or save it in file
    do.run=T                       # Make the run immidiatly, or just make the batch file for the run
  )               
  
  a<-readLines(file.path(data.path,'short_term.out'))
 
  excl<-grep('INF',a)
  if (length(excl)>0) {
    a<-a[-excl]
    cat('\nObservations have been deleted\n')
    writeLines(a,con=file.path(data.path,'short_term.out'))
  }

  
  a<-read.table(file.path(data.path,'short_term.out'),header=T)
  #summary(a)
  if (FALSE) {
 
      fc<-file.copy(from=file.path(root.prog,"program",'sms_October_2016.exe'), to=file.path(data.path,'sms.exe'), overwrite = TRUE)
      cat("\nprog.path:",prog.path,"  data.path:",data.path,' File copied:',fc,'\n')
 
    
    
    control@read.HCR<-4   # just output MCMC N and F
    write.FLSMS.control(control,write.multi=F) 
    do.a.full.SMS.run(label="mcmc3_",  # label for output
                      do.single=T,                    # run SMS in single species mode
                      do.hessian=T,                   # Make the Hessian matrix and estimate uncertanties 
                      do.MCMCeval=T,                  # Make the mceval 
                      do.MCMC=F,                      # Prepare for MCMC analysis
                      pause=F,                        # Make a pause between each stage
                      Screen.show=T,                  # show the output on screen, or save it in file
                      do.run=T                       # Make the run immidiatly, or just make the batch file for the run
    )               
  }
  control@read.HCR<-0
  write.FLSMS.control(control,write.multi=F) 
  
  ###########
  
  Blim<-Read.reference.points()[,'Blim']/1000
  
  a$SSB<-a$SSB/1000
  a$TAC<-round(a$TAC/1000)
  
  #a<-a[order(a$species,a$TAC,a$mcmc),]
  
  prbBlim<-function(x) {
    return(sum(x>=Blim)/length(x))
  }
   summ<-function(x)c(mean(x),quantile(x,0.05),quantile(x,0.50),prbBlim(x))
  q5<-function(x)quantile(x,c(0.05))
 
   b<-aggregate(SSB~TAC,data=a,summ)
  b2<-aggregate(FF~TAC,data=a,median)
  
  b3<-merge(b,b2)
  b4<-data.frame(TAC=b3$TAC,FishMort=b3$FF,SSB5Pct=b3[[2]][,2],SSB50Pct=b3[[2]][,3],ProbBlim=1-b3[[2]][,4])
  write.csv(b4,row.names = FALSE, file=file.path(data.path,paste('forecast_output_stochastic_TAC_',TAC.year,'.csv',sep='')))
  
  pmin<-which.min(b4$ProbBlim<=0.05)
  if (wr.resul) cat(TAC.year,b4[pmin,'TAC']*1000,b4[pmin,'ProbBlim'], b4[pmin,'SSB50Pct']*1000, b4[pmin,'FishMort'],paste(' Met_4_SMS',rec.mode,'\n',sep='_'),file=resulFile,append=TRUE)
  cat(TAC.year,b4[pmin,'TAC']*1000,b4[pmin,'ProbBlim'], b4[pmin,'SSB50Pct']*1000, b4[pmin,'FishMort'],paste(' Met_4_SMS',rec.mode,'\n',sep='_'))
  
  target.TAC<-b4[pmin,'TAC']
  
  if (do.detail.plot) {
    cleanup()
    nox<-2; noy<-2;
    nox<-1; noy<-1;
    dev='screen';
    newplot(dev,nox,noy);
    i<-0
    
    unique(a$TAC)

    aa<-a
    
    nox<-4; noy<-4;
    noxy<-nox*noy; i<-noxy
    dev<-'png';pic<-1
    by(aa,list(aa$TAC),function(x) {
        if (i==noxy) {newplot(dev,nox,noy,filename=paste('dist',TAC.year,pic,sep='_')); i<<-0;pic<<-pic+1 }
        p5<-quantile(x$SSB,0.05)
        rBlim<-sum(x$SSB<Blim)/dim(x)[[1]]*100
        hist(x$SSB,main=paste("TAC:",x[1,]$TAC,'\n5% percentile=',round(p5),'\nprob(SSB<Blim)=',round(rBlim),'%',sep=''),freq=F,nclass=20,xlab='SSB (1000 t)')
        den <- density(x$SSB)
        lines(den, col = "blue",lwd=1.5)
        abline(v=Blim,col='red',lwd=2)
        abline(v=p5,col='blue',lwd=2)
        i<<-i+1
    })
    cleanup()
  }
  
  aa<-subset(a,TAC==target.TAC)
  
  newplot(dev="screen",nox=1,noy=3,Portrait=T);
  par(mar=c(5,4,4,1)+.1)
  d<-density(aa[,'SSB']); 
  plot(d,main=paste(SMS.control@species.names," Sandeel. ",TAC.year,'\nSSB'),xlab='SSB (1000 t)')
  abline (v=quantile(aa[,'SSB'],0.05),lty=2,lwd=2,col='red' )
  if (var(aa[,'FF'])>0) { d<-density(aa[,'FF']); plot(d,main='Fishing mortality', xlab=('F')) }
  if (var(aa[,'Yield'])>0) { d<-density(aa[,'Yield']/1000); plot(d,main='Yield', xlab=('Yield (1000 t)'))} #else hist(aa[,'Yield']/1000,main='Yield', xlab='Yield (1000 t)')
  savePlot(filename = file.path(data.path,paste('Met_4_SMS_density',rec.mode,TAC.year,sep='_')), type = "png")
  
  
  
  a$rBlim<-a$SSB<Blim
  b<-aggregate(rBlim~round(TAC),data=a,sum)
  b$rBlim<-b$rBlim/dim(a)[[1]]*100*length(unique(a$TAC))
  X11()
  plot(b,xlab='TAC (1000 t)',ylab='Prob(SSB<Blim) %',type='b',lwd=2,ylim=c(0,50),main=paste(SMS.control@species.names,"Sandeel ",TAC.year))
  abline(h=5,lty=2,col='red',lwd=3)
  savePlot(filename = file.path(data.path,paste('Met_4_SMS_prob',TAC.year,sep='_')), type = "png")
return(target.TAC*1000)
}

#  stochastic_forecast_TAC(TAC.year=2016,scale.options=scale.options<-seq(0,25000,1000),MCM=1000,MCMC.save=100  ) 
  