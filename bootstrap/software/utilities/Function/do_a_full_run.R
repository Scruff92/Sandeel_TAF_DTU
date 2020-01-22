# Function to make runs with SMS
# label:        a text string included in the file name for sceen output (if Scrren.show=TRUE)
#do.single:     do a single species SMS
#do.multi.1     do a sms run in multispecies mode 1
#do.multi.2     do a sms run in multispecies mode 2
#do.hessian     Create the Hessian matrix and parameter uncertanities in sms.std file
#do.mcmc        perform Markov Chain Monte Carlo with a number (=mcmc) of simulations and save (=mcsave) a set of parameters for MCMC analysis
#mcmc           create a number (=mcmc) number of MCMC simulations
#mcsave         save the parameters for every mcsave simultaions
#do.likelihood.profil  # Make a likelihood profile of SSB 
#do.prsave      # save results of likelihood profile
#do.prediction  perform a single species prediction
#Screen.show    Show results from SMS run on the R-console screen (TRUE) or write results to file ud*.*
#pause          pause after each SMS run
#do.run         Run the batch file (do.run=TRUE) or just create batch file (do.run=FALSE)

do.a.full.SMS.run<-function(label="",rundir=data.path,outdir=data.path,do.single=TRUE,do.multi.1=FALSE,do.multi.2=FALSE,do.multi.2.redo=FALSE, do.multi.2.redo.Nbar=FALSE,
                            do.hessian=FALSE,do.MCMC=FALSE,mcmc=1,mcsave=1,do.MCMCeval=F,shake.ms2.par=F, cleanup=F,SSB.R.seperate=F, do.likelihood.profile=F,do.prsave=F,
                            do.prediction=FALSE,do.run=TRUE, Screen.show=TRUE,pause=FALSE,ADMB.options="-gbs 1500000000 ",deleteFiles=NA,format.options=T,run.local=T,new.version=F) {



file<-file.path(outdir,'SMS.dat')

dos.root<-gsub(.Platform$file.sep,"\\",root,fixed=TRUE)
outdir<-paste(gsub(.Platform$file.sep,"\\",outdir,fixed=TRUE),"\\",sep="")
sms.do<-paste(outdir,label,"do.bat",sep="")
iter<-0
append<-FALSE

if (run.local) pgm<-"sms " else pgm<-paste('"',dos.root,"\\program\\sms",'"',sep=T)
if (run.local) {
   if (!file.exists(file.path(rundir,"sms.exe")) | new.version) {
     file.copy(file.path(root,"program","sms.exe"),file.path(data.path,"sms.exe"),overwrite =TRUE)
     file.copy(file.path(root,"program","sms.tpl"),file.path(data.path,"sms.tpl"),overwrite =TRUE)
     cat("\nSMS program is copied to the working directory\n")
   }
}

control<-read.FLSMS.control()   # read option file, SMS.dat

if (do.single) {

   #delete requested files
   if (!is.na(deleteFiles[1])) {
     cat(paste(dosDrive,"\n","cd ",'"',outdir,'"',"\n",sep=""),file=sms.do,append=append)
     for  (dels in deleteFiles) {
        cat(paste("del /Q ", dels,"\n",sep=""),file=sms.do,append=TRUE)
     }
     append<-TRUE
   }

    #run SMS in single species mode
    control@VPA.mode<-0          # be sure that the the run is made in single species mode
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-2 
       control@phase.SSB.R.beta<-2 
    }
    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms0.dat',sep="")),write.multi=F,nice=format.options,writeSpNames=F)

    if (do.hessian) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave, ' -mcseed 100')
    if (do.MCMCeval) hes.string<-paste(" -mceval ")
    if (do.likelihood.profile)  hes.string<-paste(" -lprof ")
    if (do.likelihood.profile & do.prsave)  hes.string<-paste(" -lprof -prsave ")
   
    if (do.multi.1 | do.multi.2 | do.multi.2.redo) hes.string<-' -nohess '

    cat(paste(dosDrive,"\n","cd ",'"',outdir,'"',"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (Screen.show) cat(paste(pgm," -nox -ind ",label,"ms0.dat",hes.string,"\n",sep=""),file=sms.do,append=TRUE)
    if (!Screen.show) cat(paste(pgm," -nox -ind ",label,"ms0.dat",hes.string," >",label,"out0_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.par",'"'," ",'"',outdir,label,"ms0.par",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.rep",'"'," ",'"',outdir,label,"ms0",".rep",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian & !(do.multi.1 | do.multi.2 | do.multi.2.redo)) cat(paste("copy /Y ",'"',outdir,"sms.std",'"'," ",'"',outdir,label,"ms0",".std",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.1) {
    #run SMS in multi species mode 1, start to change options
    control@VPA.mode<-1                    # set multispecies mode =1
    if (control@stomach.variance != 33) control@phase.stom.var<-2              # stomach variance
    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms1.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)

    #run in musltispecies mode 1
     if (do.hessian & do.multi.2==F & do.multi.2.redo==F)  hes.string<-' ' else hes.string<-' -nohess '
    cat(paste(dosDrive,"\n","cd ",'"',outdir,'"',"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms1.dat  -ainp ",label,"ms0.par -phase 2 >",label,"out1_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms1.dat   -ainp ",label,"ms0.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.par",'"'," ",'"',outdir,label,"ms1.par",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.rep",'"'," ",'"',outdir,label,"ms1",".rep",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.2) {
    #run SMS in multi species mode 2, start to change options
    control@VPA.mode<-2                    # set multispecies mode =2
    control@phase.stom.var<- -1            # stomach variance
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-3 
       control@phase.SSB.R.beta<-3 
    }

    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms2.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)

    #run in multispecies mode 2

    if (do.hessian &  do.multi.2.redo==F & do.multi.2.redo.Nbar==F) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC & do.hessian & do.multi.2.redo==F & do.multi.2.redo.Nbar==F) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave)
    cat(paste(dosDrive,"\n","cd ",'"',outdir,'"',"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms2.dat -ainp ",label,"ms1.par -phase 2 >",label,"out2_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms2.dat -ainp ",label,"ms1.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.par",'"'," ",'"',outdir,label,"ms2.par",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.rep",'"'," ",'"',outdir,label,"ms2",".rep",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian) cat(paste("copy /Y ",'"',outdir,"sms.std",'"'," ",'"',outdir,label,"ms2",".std",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.2.redo) {
    #run SMS in multi species mode 2 with estimation of all parameters, start to change options
    control@VPA.mode<-2                    # set multispecies mode =2
    control@phase.stom.var<-2              # stomach variance
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-3 
       control@phase.SSB.R.beta<-3 
    }

    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms3.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)

    #run in multispecies mode 2 using parameter file from previous multi=2 run
    if (do.hessian &  do.multi.2.redo.Nbar==F) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC & do.hessian & do.multi.2.redo.Nbar==F) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave)
    cat(paste(dosDrive,"\n","cd ",'"',outdir,'"',"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("del /f ",'"',outdir,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3.dat -ainp ",label,"ms2.par -phase 2 >",label,"out3_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3.dat -ainp ",label,"ms2.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.par",'"'," ",'"',outdir,label,"ms3.par",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.rep",'"'," ",'"',outdir,label,"ms3",".rep",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian) cat(paste("copy /Y ",'"',outdir,"sms.std",'"'," ",'"',outdir,label,"ms3",".std",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.2.redo.Nbar) {
    #run SMS in multi species mode 2 with estimation of all parameters, start to change options
    control@VPA.mode<-2                    # set multispecies mode =2
    control@phase.stom.var<- 2              # stomach variance.
    control@use.Nbar<-1                    # Use mean stock number 
    control@M2.iterations<-6
    control@max.M2.sum2<-0 
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-3 
       control@phase.SSB.R.beta<-3 
    }

    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms3Nbar.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)
    
    if (shake.ms2.par) {
       p<-scan(file.path(rundir,paste(label,"ms2.par",sep="")),comment.char = "#")
       p<-p*runif(n=length(p),min=1.0,max=1.15)
       cat(p,file=file.path(rundir,paste(label,"ms2.par",sep="")))
    }
    #run in multispecies mode 2 using parameter file from previous multi=2 run
    if (do.hessian) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC & do.hessian) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave)
    cat(paste(dosDrive,"\n","cd ",'"',outdir,'"',"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste("rm -f ",'"',outdir,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("rm -f ",'"',outdir,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("rm -f ",'"',outdir,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3Nbar.dat -ainp ",label,"ms2.par -phase 2 >",label,"out4_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3Nbar.dat -ainp ",label,"ms2.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.par",'"'," ",'"',outdir,label,"ms3Nbar.par",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste("copy /Y ",'"',outdir,"sms.rep",'"'," ",'"',outdir,label,"ms3Nbar",".rep",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian) cat(paste("copy /Y ",'"',outdir,"sms.std",'"'," ",'"',outdir,label,"ms3Nbar",".std",'" ',"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}


if (do.prediction) {
   cat(paste(dosDrive,"\n","cd ",'"',outdir,'"',"\n",sep=""),file=sms.do,append=append)
    control@read.HCR<-1
     ll<-"ms3.dat"
    if (do.single) ll<-"ms0.dat"
    if (do.multi.2) ll<-"ms2.dat"
    if (do.multi.2.redo) ll<-"ms3.dat"

    if (!Screen.show) cat(paste(pgm," -ind ",label,ll," -mceval >",label,"out5_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -ind ",label,ll," -mceval \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
}

if (do.run) {
    command<-paste('"',sms.do,'"',sep='')
    #  shell(command, invisible = TRUE)
    system(command,show.output.on.console =Screen.show)
} else cat(paste("batch file,",sms.do,"for SMS run is made\n"))
}

