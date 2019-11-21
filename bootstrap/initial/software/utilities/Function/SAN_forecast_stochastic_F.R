stochastic_forecast_F<-function(TAC.year=2016,do.likelihood.profile=FALSE,scale.options=seq(0,1,0.025),extend_data=TRUE,wr.resul=FALSE,rec.mode='Fixed') {
    # this script will 
    #  1. add data for TAC year and the year after TAC year for WECA.in, WEST.in, propmat.in and natmor.in
    #  2. add data after the TAC year for propmat.in
    #  3. change the SMS.dat such that the stochactis forecast can be run  (read_HCR=2)
    #  4. Run the stochastic forecast
    #  5. change the SMS.dat such that the stochactis forecast is not done in the next run(read_HCR=0) 
    #  6. Make output tables and graphs from the short-term forecast
    #
    save.on.file<-TRUE   #  output on file or on screen only
    
    # do not normally change these options below
    do.prsave<-T      # save MCMC results for later use
    
    LowerProbBelowBlim<-0.1 # for output on graphs 
     
    #  user options according to stock annex, Please keep unchanged. 
    PM.after.TAC.year<-seq(2000,2003,1)         # Year or year Range for Proportion Mature for the year after TAC year  (default long term average)
    west.TAC.year<-seq(TAC.year-3,TAC.year-1,1) # Year or year Range for weight in the Sea  for the TAC year  (default the 3 most recent years ~ seq(TAC.year-3,TAC.year-1,1))
    weca.TAC.year<-seq(TAC.year-3,TAC.year-1,1) # Year or year Range for weight in the Catch  for the TAC year (default the 3 most recent years ~seq(TAC.year-3,TAC.year-1,1))
    NatMor.year<-TAC.year-1                      # Year or year Range for Natural mortality (M) (default the most recent year)
    
    ## end user options, do not change in the code below
    
    ############
    firstY<-SMS.control@first.year
    lastY<-TAC.year-1
    SMS.control@last.year.model<-TAC.year-1
    Nyear<- lastY-firstY+1
    n.season<-SMS.control@last.season
    
    # make a nice table with forecast input
    
    tab<-matrix(0,ncol=5,nrow=13)
    rownames(tab)<-c(paste("Stock numbers(",TAC.year,')',sep=''),'CV stock numbers',
               'Exploitation patttern 1st half','CV exploitation pattern 1st',
               'Exploitation patttern 2nd half','CV exploitation pattern 2nd',
               'Weight in the stock 1st half',
               'Weight in the catch 1st half','Weight in the catch 2nd half',
               paste('Proportion mature(',TAC.year,')',sep=''),
               paste('Proportion mature(',TAC.year+1,')',sep=''),
               'Natural mortality 1st half',
               'Natural mortality 2nd half')
    colnames(tab)<-paste('Age',0:4)
    
    
    add.data<-function(fileName='weca.in',roundN=5,addY=0,YearSelected=1998){
      s<-matrix(scan(file.path(data.path,fileName),comment.char='#'),ncol=5,byrow=T)
      s<-s[1:((Nyear+addY)*2),]
      s<-data.frame(s)
      colnames(s)<-paste('Age',0:4,sep='')
      s$Year<-rep(firstY:(lastY+addY),each=n.season)
      s$Season<-rep(1:n.season,(Nyear+addY))
      
      r<-subset(s,Year %in% YearSelected)
      r<-aggregate(list(Age0=r$Age0,Age1=r$Age1,Age2=r$Age2,Age3=r$Age3,Age4=r$Age4),list(Season=r$Season),mean)
      r$Year<-TAC.year+addY
                               
      if (addY==0) {
        rr<-r
        rr$Year<-TAC.year+1
        r<-rbind(r,rr) 
      }
      
      s<-rbind(s,r)
      s<-s[order(s$Year,s$Season),]
      s$Age0[s$Season==1]<-0
      
      s$Age0<-round(s$Age0,roundN)
      s$Age1<-round(s$Age1,roundN)
      s$Age2<-round(s$Age2,roundN)
      s$Age3<-round(s$Age3,roundN)
      s$Age4<-round(s$Age4,roundN)
       
      out<-file.path(data.path,fileName)
      unlink(out)
      for (y in(firstY:(lastY+2))) {
        cat(paste("# year:",y), file=out,append=TRUE)
        if (y>=(TAC.year+addY)) { cat(" average from:", file=out,append=TRUE);  cat(YearSelected, file=out,append=TRUE); }
        cat("\n",file=out,append=TRUE)
    
          for (q in(1:n.season)){ 
           aa<-subset(s,Year==y & Season==q)
           cat(format(c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4) ,format='fg',width=12,justify='right'),file=out,append=TRUE)
           cat("\n",file=out,append=TRUE)
          }
      }
      if (fileName=='weca.in') {
        aa<-subset(s,Year==TAC.year & Season==1)
        tab['Weight in the catch 1st half',]<<-c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4)*1000
        aa<-subset(s,Year==TAC.year & Season==2)
        tab['Weight in the catch 2nd half',]<<-c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4)*1000
      }  
      if (fileName=='west.in') {
        aa<-subset(s,Year==TAC.year & Season==1)
        tab['Weight in the stock 1st half',]<<-c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4)*1000
      }  
      if (fileName=='propmat.in') {
        aa<-subset(s,Year==TAC.year & Season==1)
        tab[paste('Proportion mature(',TAC.year,')',sep=''),]<<-c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4)
        aa<-subset(s,Year==TAC.year+1 & Season==1)
        tab[paste('Proportion mature(',TAC.year+1,')',sep=''),]<<-c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4)
      } 
      if (fileName=='natmor.in') {
        aa<-subset(s,Year==TAC.year & Season==1)
        tab['Natural mortality 1st half',]<<-c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4)
        aa<-subset(s,Year==TAC.year & Season==2)
        tab['Natural mortality 2nd half',]<<-c(aa$Age0,aa$Age1, aa$Age2, aa$Age3, aa$Age4)
      }  
    
            
    }
    if (extend_data) {
      add.data(fileName='weca.in',roundN=5,YearSelected=weca.TAC.year)
      add.data(fileName='west.in',roundN=5,YearSelected=west.TAC.year)
      add.data(fileName='natmor.in',roundN=2,YearSelected=NatMor.year)
      add.data(fileName='propmat.in',roundN=2,YearSelected=PM.after.TAC.year,addY=1)
    }
    
    
    # read assessment summary file
    s<-Read.summary.data(extend=TRUE)
    head(s)
    
    #recruitment geometric mean
    tmp<-subset(s,Year < (TAC.year-1) & Quarter==SMS.control@rec.season & Age==SMS.control@first.age,select=c(Year,N))
    recruit.TAC.year<-exp(mean(log(tmp$N)))
    
    Yield.assess<-sum(subset(s,Year %in% (TAC.year-1),select=c(Yield)))/1000
    
    
    #
    # write the short-term-configuration.dat
    out<-file.path(data.path,'short-term-configuration.dat')
    cat(file=out, paste(length(scale.options),'\n# no of F multipliers\n'))
    cat(file=out,scale.options,append=TRUE)
    if (rec.mode!='Fixed') recruit.TAC.year<- -1
    cat(file=out,"\n# Recruitment TAC year\n",recruit.TAC.year,'\n',append=TRUE)
      
    ### change the SMS.dat to make stochastic forecast  
    control<-read.FLSMS.control()
    control@read.HCR<-2
    write.FLSMS.control(control,write.multi=F) 
    
    
    ### make the SMS assessment including Forecast  
    if (do.likelihood.profile) {
        fc<-file.copy(from=file.path(root.prog,"program",'sms_likelihood.exe'), to=file.path(data.path,'sms.exe'), overwrite = TRUE)
        cat("\nprog.path:",prog.path,"  data.path:",data.path,' File copied:',fc,'\n')
    } else {
        fc<-file.copy(from=file.path(root.prog,"program",'sms_October_2016.exe'), to=file.path(data.path,'sms.exe'), overwrite = TRUE)
        cat("\nprog.path:",prog.path,"  data.path:",data.path,' File copied:',fc,'\n')
    }
      
    
    do.a.full.SMS.run(label="run_",                   # label for output
      cleanup=FALSE,
      do.single=T,                    # run SMS in single species mode
      do.multi.1=F,                   # Make preliminary estimate of "predation parameters"
      do.multi.2=F,                   # Run the full model, with simulainiously estimation af all parameters except the stomach variance parameter
      do.multi.2.redo=F,              # Run the full model, with simulainiously estimation af all parameters
      do.multi.2.redo.Nbar=F,         # Run the full model, with simulainiously estimation af all parameters, Use mean stock numbers (Nbar) for predation
      shake.ms2.par=F,                # Add noise (factor 1.0 to 1.10) to parameter estimate from phase 2. Usefull to handle SMS error message "step size too small" when do.multi.2.redo.Nbar=T
      do.hessian=T,                   # Make the Hessian matrix and estimate uncertanties 
      do.MCMC=F,                      # Prepare for MCMC analysis
      mcmc=1,mcsave=1,                # Options for MCMC analysis
      do.likelihood.profile=do.likelihood.profile,
      do.prsave=do.prsave, 
      do.prediction=F,                # Make a prediction
      pause=F,                        # Make a pause between each stage
      Screen.show=T,                  # show the output on screen, or save it in file
      do.run=T,                       # Make the run immidiatly, or just make the batch file for the run
      deleteFiles=NA,                 # clean up in files before the run is made
      new.version=F)                  # copy a (new) version of the sms program from the program directory (default=FALSE)
    
    control@read.HCR<-0
    write.FLSMS.control(control,write.multi=F) 
    
    rec.old<-subset(Read.summary.data(),Year==TAC.year-1 & Age==0 & Quarter==2,select=N)
    
    a<-Read.SMS.std()
    b<-droplevels(subset(a,name %in% c('term_N_next','exploi_pattern'),select=c(name,index,value,std,species,quarter,age )))
    
    bb<-tapply(b$value,list(b$name,b$age,b$quarter),sum)
    bb[is.na(bb)]<-0
    tab['Exploitation patttern 1st half',]<-bb['exploi_pattern',,1]
    tab['Exploitation patttern 2nd half',]<-bb['exploi_pattern',,2]
    
    exp.pat<-bb['exploi_pattern',,]
    mean.f<-sum(exp.pat[2:3,])/2
    
    
    
    bbb<-tapply(b$std,list(b$name,b$age,b$quarter),sum)
    bbb[is.na(bbb)]<-0
    cv<-bb  # copy structure
    cv[bb>0]<-bbb[bbb>0]/bb[bb>0]
    tab['CV exploitation pattern 1st',]<-cv['exploi_pattern',,1]
    tab['CV exploitation pattern 1st',1]<-0
    tab['CV exploitation pattern 2nd',]<-cv['exploi_pattern',,2]
    
    tab[1,]<-bb['term_N_next',,1]
    tab['CV stock numbers',]<-cv['term_N_next',,1]
    tab[1,1]<-recruit.TAC.year
    tab[2,1]<-0
    
    if (save.on.file ) write.csv(tab,file=file.path(data.path,paste('forecast_input_stochastic_',TAC.year,'.csv',sep='')))
    
    
    tab[1,]<-tab[1,]/1000
    rownames(tab)<-paste(rownames(tab),c('(million)',' ',' ',' ',' ',' ','(gram)','(gram)','(gram)','  ',' ',' ',' '))
    xtab3(tab, caption=paste("Table XXX.  ",SMS.control@species.names," Sandeel. input to stochastic forecast",sep=''),
       cornername='Variable',
       file=file.path(data.path,paste('_forcast_input-stochastic_',TAC.year,'.html',sep='')), dec=c(0,2,3,2,3,2,2,2,2,2,2,2,2), width='"100%"',units=NULL)
    
    a<-read.fit('SMS')
    ran<-min(grep("term_N_next" ,a[['names']])):max(grep("exploi_pattern" ,a[['names']]))
    ran<-c(grep("term_N_next" ,a[['names']]),grep("exploi_pattern" ,a[['names']]))
    cols<-a$names[ran]
    
    cols<-NULL
    for (i in ran) {
       bb<-subset(b,index==i)
      if (bb[1,'name']=="term_N_next") cols<-c(cols,paste('N age',bb[1,'age']))
      if (bb[1,'name']=="exploi_pattern") if (bb[1,'quarter']==1) cols<-c(cols,paste('F 1st age',bb[1,'age'])) else cols<-c(cols,paste('F 2nd age',bb[1,'age']))
    }
    
    
    cor<-a[['cor']][ran,ran]
    colnames(cor)<-cols
    rownames(cor)<-colnames(cor)
    
    #print(round(cor,2))
    
    xtab(cor, caption=paste("Table XXX.  ",SMS.control@species.names," Sandeel. input to stochastic forecast, correlation matrix",sep=''),
       cornername='Variable',
       file=file.path(data.path,paste('_forcast_input-stochastic-correlation_',TAC.year,'.html',sep='')), dec=rep(2,ncol(cor)), width='"100%"',units=NULL)
    
    if (save.on.file ) write.csv(cor,file=file.path(data.path,paste('forecast_input_stochastic-correlation_',TAC.year,'.csv',sep='')))
    
    
    ###########
    
    
    a<-readLines(file.path(data.path,'short_term.out'))
    if (any(grepl('#IND',a))) print('Please note: Invalid output have ben deleted')
        
    a<-a[!grepl('#IND',a)]
    a<-a[!grepl('#INF',a)]
    
    writeLines(a,file.path(data.path,'short_term_clean.out'))
    
    a<-read.table(file.path(data.path,'short_term_clean.out'),header=T)
    b<-subset(Read.SMS.std(dir=data.path) ,(name== 'short_term_SSB' |name== 'log_short_term_SSB'),select=c(name,value,std,species,area ))
    b$n.Fmulti<-b$area
    b$area<-NULL
    
    a<-merge(a,b)
    Blim<-Read.reference.points()[,'Blim']
    #Blim<-125000
    
    a[a$name=='short_term_SSB','ProbBelowBlim']<-pnorm(Blim,a[a$name=='short_term_SSB','SSB'],a[a$name=='short_term_SSB','std'])
    a[a$name=='log_short_term_SSB','ProbBelowBlim']<-pnorm(log(Blim),a[a$name=='log_short_term_SSB','log_SSB'],a[a$name=='log_short_term_SSB','std'])
    
    a<-a[order(a$species,a$Fmulti),]
    write.csv(a,file.path(data.path,'short_term_clean.csv'),row.names = FALSE)
    
    
    a$SSB0<-a$SSB0/1000
    a$SSB<-a$SSB/1000
    a$std<-a$std/1000
    a$Yield<-a$Yield/1000
    aa<-a
    
    
    cleanup()
     newplot(dev="screen",nox=2,noy=2,Portrait=F);
    
      par(mar=c(5,4,4,5)+.1)
    
    san_out<-function(dist='norm') {
    
      if (dist=='norm' | dist=='noDist') a<-subset(aa,name=='short_term_SSB',select=c(Fmulti,FF,Yield,SSB0,SSB,std,ProbBelowBlim))
      if (dist=='log_norm') a<-subset(aa,name=='log_short_term_SSB',select=c(Fmulti,FF,Yield,SSB0,SSB,std,ProbBelowBlim))
      a$SSBchange<-(a$SSB-a$SSB0)/a$SSB0*100
      a$YieldChange<-(a$Yield-Yield.assess)/Yield.assess*100
      SSB.TAC.year<-round(a$SSB0[1])
      a$SSB0<-NULL
      
      #cat(paste(SMS.control@species.names,"Sandeel\n"))
      #cat(paste("\nBasis: Fsq=F(",TAC.year-1,")=",round(mean.f,3),";  Yield(",TAC.year-1,")=",round(Yield.assess),sep=''))
      #cat(paste("; Recruitment(",TAC.year,")= Geometric mean=",formatC(recruit.TAC.year/1000000,format='f',digits=0),' billion',sep=''))
      
      head<-paste(SMS.control@species.names," Sandeel              ",
            "\nBasis: Fsq=F(",TAC.year-1,")=",round(mean.f,3),
            ";  Yield(",TAC.year-1,")=",round(Yield.assess), "; SSB(",TAC.year,")=",SSB.TAC.year,"; Recruitment(",TAC.year-1,")=",round(rec.old/1000000), 
             " billion; Recruitment(",TAC.year,")= Geometric mean=",formatC(recruit.TAC.year/1000000,format='f',digits=0),' billion',sep='')
      
      tab<-a
      if (dist=='log_norm') gem<<-a
      pmin<-which.min(a$ProbBelowBlim<=0.05)
      #a[pmin,]
      
      if (wr.resul) cat(TAC.year,a[pmin,'Yield']*1000,a[pmin,'ProbBelowBlim'], a[pmin,'SSB']*1000, a[pmin,'FF'],paste(' Met_2',dist,rec.mode,sep='_'),'\n',file=resulFile,append=TRUE)
      
      
      tab$Fmulti<-NULL
      tab<-as.matrix(tab)
      
      rownames(tab)<-paste('Fsq*',scale.options)
      a<-subset(a,ProbBelowBlim<=LowerProbBelowBlim)
      cv.dig<-0
      ssb.dig<-0
      if (dist=='norm' |  dist=='noDist') {
        colnames(tab)<-c(paste("F(",TAC.year,")",sep=''),paste("Yield(",TAC.year,")",sep=''),paste("SSB(",TAC.year+1,")",sep=''),
                           "std SSB",paste("Prob SSB(",TAC.year+1,") below Blim",sep=''), "%SSB change","%Yield change") 
      }
      if (dist=='log_norm') {
        colnames(tab)<-c(paste("F(",TAC.year,")",sep=''),paste("Yield(",TAC.year,")",sep=''),paste("log(SSB)(",TAC.year+1,")",sep=''),
                           "std log(SSB)",paste("Prob SSB(",TAC.year+1,") below Blim",sep=''), "%SSB change","%Yield change") 
        tab[,3]<-log(tab[,3]*1000)
         tab[,4]<-tab[,4]*1000
        cv.dig<-3 ;ssb.dig<-2
       }            
      my.digits<- c(3,0,ssb.dig,cv.dig,4,0,0,0)
      
      if ( dist=='noDist') {
        tab<-tab[,c(1:3,6,7)]
        my.digits<-c(3,0,0,0,0)
      }
  
      xtab(tab, caption=head,
         cornername='Basis',
         file=file.path(data.path,paste('_forcast_output-stochastic_',TAC.year,'_',dist,'.html',sep='')), dec=my.digits, width='"100%"',units=NULL)
      
      if (dist !='noDist') {
        aa<-subset(a,ProbBelowBlim<=0.05)
        if (dim(aa)[[1]]>0) {
          aa<-aa[dim(aa)[[1]],]
          bb<-subset(a,ProbBelowBlim>=0.05)[1,]
          
          aa<-rbind(aa,bb)
          aaa<-coef(lm(aa$Yield~aa$ProbBelowBlim))
          TAC<-aaa[1]+0.05*aaa[2]
          cat('\nTAC:',round(TAC),'\n')
          
          aaa<-coef(lm(aa$FF~aa$ProbBelowBlim))
          FF<-aaa[1]+0.05*aaa[2]
          cat('\nF:',round(FF,2),'\n')
          cat('Input files: forecast_input_stochastic.csv and forecast_input_stochastic-correlation.csv\n',
              ' and as html files: forecast_input_stochastic.html and forecast_input_stochastic-correlation.html\n\n',
              'Output files: forecast_output_stochastic.csv and forecast_output_stochastic.html\n')
              
        
        
           
          plot(a$FF,a$Yield,type='b',xlab='F(1-2)',ylab='Yield (1000 tonnes)',col=1,lty=1,lwd=3,pch=1)
          abline(v=FF,lty=3,lwd=2)
          abline(h=TAC,lwd=2)
          
          legend("topleft",
             c('Yield','Probability'),
             pch=c(1,2),lty=c(1,2),lwd=c(3,3),col=c(1,2))
          
          par(new=T)
          plot(a$FF,a$ProbBelowBlim,ylim=c(0,max(a$ProbBelowBlim)),axes=F,lwd=3,type='b',xlab=' ',ylab=' ',col=2,pch=2,main=paste(SMS.control@species.names,"Sandeel ",TAC.year,'\n',ifelse(dist=='norm','normal distributed','log-normal distributed')))
          abline(h=0.05,col='red',lwd=2)
          axis(side=4)
          mtext(side=4,line=3.0,"Prob. SSB<Blim")
          par(xaxs="r")
          
          #newplot(dev="screen",nox=1,noy=1,Portrait=F);
          
          plot(a$Yield,a$ProbBelowBlim,type='b',ylab='Prob(SSB<Blim)',xlab='Yield (1000 tonnes)',col=2,lty=1,lwd=3,pch=1,main=paste(SMS.control@species.names,"Sandeel ",TAC.year,'\n',ifelse(dist=='norm','normal distributed','log-normal distributed')))
          abline(h=0.05)
          abline(v=TAC)
        } else cat('\n\nNo forecast produces prob(SSB>Blim) > 95%\n')
      }
    }
    
    if (!do.likelihood.profile) {
      san_out(dist='norm')
      san_out(dist='log_norm')
      savePlot(filename = file.path(data.path,paste('Met_2_risk_Blim_',rec.mode,TAC.year,sep='_')), type = "png")
      #san_out(dist='noDist')
    }
    
    profileLikelihood<-function(infile='SSB_like.plt') {
    
      newplot(dev="screen",nox=1,noy=1,Portrait=F);
      par(mar=c(5,4,4,5)+.1)
    
    
      a<-readLines(con=file.path(data.path,infile))
      fc<-substr(a,1,1)
      
      if (infile=='SSB_like.plt') {
        fivePct<-a[grep('The probability is    0.95 that SSB_likeprof is greater than',a)]
        Xlab<-'SSB (1000 t)'
      }
      if (infile=='stock_N_.plt') {
        fivePct<-a[grep('The probability is    0.95 that stock_N_likeprof is greater than',a)]
        Xlab<-'N at age'
      }
      ssb5<-c(as.numeric(strsplit(fivePct,' ')[[1]][[13]]),as.numeric(strsplit(fivePct,' ')[[2]][[13]]))/1000
      
      b<-read.table(file.path(data.path,infile),skip=1,header=T,nrows=which(fc=='M')[1]-3 )
    #  plot(x=b$Profile/1000,b$likelihood,xlab='SSB (1000 t)',main=paste(SMS.control@species.names,"Sandeel ",TAC.year,'Profile likelihood'))
    #  savePlot(filename = file.path(data.path,paste('likelihood_profile_1',TAC.year,sep='_')), type = "png")
      
      a<-(b$Profile[-1]- b$Profile[1:dim(b)[[1]]-1]) *b$likelihood[1:dim(b)[[1]]-1] 
       
      
      aa<-0
      for (i in 1:(dim(b)[[1]])) {
        aa<-aa+a[i]
        b[i,'comprob']<-aa
      }
      
      # Normal approx
      
      skip<-which(fc=='N')
      nrows<- which(fc=='M')[2]-skip-1
      bb<-read.table(file.path(data.path,infile),skip=skip,header=F,nrows=nrows )
      names(bb)<-c('Profile','likelihood')
      plot(x=bb$Profile/1000,bb$likelihood,xlab=Xlab,main=paste(SMS.control@species.names,"Sandeel ",TAC.year,' Normal Approximation'))
      savePlot(filename = file.path(data.path,paste('Met_2_likelihood_profile_normal',rec.mode,TAC.year,sep='_')), type = "png")
      
      a<-(bb$Profile[-1]- bb$Profile[1:dim(bb)[[1]]-1]) *bb$likelihood[1:dim(bb)[[1]]-1] 
      aa<-0
      for (i in 1:(dim(b)[[1]])) {
        aa<-aa+a[i]
        bb[i,'cumprob']<-aa
      }
       
      if (infile=='SSB_like.plt') {
        bbb<-subset(Read.SMS.std(dir=data.path) ,name== 'log_short_term_SSB',select=c(name,value,std ))
        bbbb<-subset(Read.SMS.std(dir=data.path) ,name== 'short_term_SSB',select=c(name,value,std ))
      } else if (infile=='stock_N_.plt') {
        bbb<-subset(Read.SMS.std(dir=data.path) ,name== 'term_logN_next' & age==my.N.age,select=c(name,value,std ))
        bbbb<-subset(Read.SMS.std(dir=data.path) ,name== 'term_N_next' & age==my.N.age,select=c(name,value,std ))
        
      }
      
      #plot(x=bb$Profile/1000,bb$cumprob)
      lwd<-3 
      xx<-bb$Profile[bb$Profile>0]
      maxDensity<-max(dlnorm(xx, mean = bbb[1,'value'], sd =  bbb[1,'std']))
      
      plot(x=b$Profile/1000,b$likelihood,ylab='density',xlab=Xlab,ylim=c(0,max(maxDensity,max(b$likelihood))), xlim=c(min(b$Profile,bb$Profile)/1000,max(b$Profile,bb$Profile)/1000),type='l',lwd=3,col='red',main=paste(SMS.control@species.names,"Sandeel ",TAC.year,"\n F=",round(scale.options,2)))
      if (infile=='SSB_like.plt') abline(v=ssb5[1],lty=3,col='red',lwd=lwd)
      points(x=bb$Profile/1000,bb$likelihood,xlab='SSB (1000 t)',type='l',lwd=lwd,col='blue')
      if (infile=='SSB_like.plt') abline(v=ssb5[2],lty=3,col='blue',lwd=lwd)
      legend(x="topright",legend=c('Profile Likelihood','Normal approximation','log normal dist.') ,  col = c('red','blue','green'), lty=c(1,1,1),lwd=c(lwd,lwd,lwd))

      points(x=xx/1000,dlnorm(xx, mean = bbb[1,'value'], sd =  bbb[1,'std'], log = FALSE), bb$likelihood,xlab='SSB (1000 t)',type='l',lwd=lwd,col='green')
      savePlot(filename = file.path(data.path,paste('Met_2_likelihood_profile_2',rec.mode,TAC.year,sep='_')), type = "png")
      
      #almost the same plot
      plot(x=b$Profile/1000,b$likelihood,ylab='density',xlab=Xlab,type='l',ylim=c(0,max(maxDensity,max(b$likelihood))),lwd=lwd,col='red',main=paste(SMS.control@species.names,"Sandeel ",TAC.year,"\n F=",round(scale.options,2)))
      if (infile=='SSB_like.plt') abline(v=ssb5[1],lty=3,col='red',lwd=lwd)
      legend(x="topright",legend=c('Profile likelihood','log normal distribution','Normal distribution') ,  col = c('red','green','blue'), lty=c(1,1,1),lwd=c(lwd,lwd,lwd))
      points(x=xx/1000,dlnorm(xx, mean = bbb[1,'value'], sd =  bbb[1,'std'], log = FALSE), bb$likelihood,xlab='SSB (1000 t)',type='l',lwd=lwd,col='green')
      abline(v=exp(qnorm(0.05, mean = bbb[1,'value'], sd =  bbb[1,'std'], log = FALSE))/1000,lty=3,lwd=lwd,col='green')
      points(x=xx/1000,dnorm(xx, mean = bbbb[1,'value'], sd =  bbbb[1,'std'], log = FALSE), bb$likelihood,xlab='SSB (1000 t)',type='l',lwd=lwd,col='blue')
      abline(v=qnorm(0.05, mean = bbbb[1,'value'], sd =  bbbb[1,'std'], log = FALSE)/1000,lty=3,lwd=lwd,col='blue')
      savePlot(filename = file.path(data.path,paste('Met_2_likelihood_profile_3',rec.mode,TAC.year,sep='_')), type = "png")
      
    }
    
    if (do.likelihood.profile) {
      my.N.age<-2
      profileLikelihood(infile='SSB_like.plt')
      #profileLikelihood(infile='stock_N_.plt')
      file.copy(from=file.path(root.prog,"program",'sms.exe'), to=file.path(data.path,'sms.exe'), overwrite = TRUE)
      
       
    }
}

#stochastic_forecast_F(TAC.year=2016,do.likelihood.profile=TRUE,scale.options=0.3 )
#  stochastic_forecast_F(TAC.year=2016,do.likelihood.profile=FALSE,scale.options=seq(0,2,0.1),extend_data=TRUE) 
