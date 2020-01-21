## Read SMS output from details.out
## Labels: Stock numbers, Fishing mortality, etc.

read.output <- function(file, label)
{
  txt <- readLines(file)
  beg <- which(txt==label) + 2
  end <- c(which(txt=="")-1, length(txt))
  end <- min(end[end > beg])
  txt <- txt[beg:end]
  year <- grep("# Year", txt, value=TRUE)
  year <- as.integer(sub("# Year:", "", year))
  age <- grep("# age", txt, value=TRUE)[1]
  age <- scan(text=age, what="", quiet=TRUE)[-(1:2)]
  out <- read.table(text=txt)
  out <- data.frame(rep(year,each=2), 1:2, out)
  names(out) <- c("Year", "Step", age)
  out
}


## Convert a "step" table to long format
## Input is a data frame like this: Year Step 0 1 2 3 4+
## Output is a data frame like this: Year Step Age Value
## For convenience, user can pass a name for the last column:
##   step2long(catage, names="Catch")

step2long <- function(x, names=c("Year","Step","Age","Value"))
{
  if(length(names) == 1)
    names <- c("Year", "Step", "Age", names)
  row.names(x) <- paste(x[[1]], x[[2]], sep=":")
  x <- x[-(1:2)]
  y <- as.data.frame(as.table(as.matrix(x)))
  y1 <- sub(":.*", "", y[[1]])
  y2 <- sub(".*:", "", y[[1]])
  z <- data.frame(y1, y2, y[-1])
  z$y1<-as.numeric(as.character(z$y1)) # I have tried to mimic type.convert()
  z$y2<-as.numeric(as.character(z$y2)) # I have tried to mimic type.convert()
  #z <- type.convert(z, as.is=TRUE)
  names(z) <- names
  z <- z[order(z[[1]], z[[2]], z[[3]]),]
  row.names(z) <- NULL
  z
}


## Convert a "step" table to crosstab format
## Input is a data frame like this: Year Step 0 1 2 3 4+
## Output is a data frame like this:
##   Year 0.1 0.2 1.1 1.2 2.1 2.2 3.1 3.2 4+.1 4+.2

step2xtab <- function(x)
{
  x <- step2long(x)
  y <- xtabs(x[[4]] ~ x[[1]] + paste(x[[3]],x[[2]],sep="."))
  z <- xtab2taf(y)
  z
}


## Convert sandeel table:
## - crosstab
## - remove age 0.1
## - calculate means
## - round numbers
## - full column names
## Note that colnames have commas, so write.taf with quotes=TRUE

sandeel.table <- function(x, digits)
{
  x <- step2xtab(x)
  x <- x[names(x) != "0.1"]
  x <- rbind(x, colMeans(x))
  x[nrow(x),1] <- "arith. mean"
  x <- rnd(x, -1, digits)
  age <- c("0", "1", "1", "2", "2", "3", "3", "4+", "4+")
  half <- c("2nd", "1st")
  names(x) <- c("Year", paste0("Age ", age, ", ", half, " half"))
  x
}


## Increase text size of lattice plot
## This function has been added to the icesTAF package (25 Mar 2019)
## and will soon be a part of the icesTAF 2.3-0 version on CRAN.

zoom <- function(obj, cex=1.8, cex.main=1.3*cex, cex.lab=1.1*cex, cex.axis=cex,
                 cex.strip=cex, cex.symbol=cex, cex.sub=0.7*cex,
                 cex.legend=0.7*cex)
{
  if(class(obj) != "trellis")
    stop("'obj' must be a trellis object")
  suppressWarnings({
    obj$main$cex <- cex.main
    obj$xlab$cex <- cex.lab
    obj$ylab$cex <- cex.lab
    obj$x.scales$cex <- rep(cex.axis, length(obj$x.scales$cex))
    obj$y.scales$cex <- rep(cex.axis, length(obj$y.scales$cex))
    obj$par.strip.text$cex <- cex.strip
    obj$par.settings$superpose.symbol$cex <- cex.symbol
    obj$sub$cex <- cex.sub
    if(!is.null(obj$legend))
      obj$legend$right$args$cex <- cex.legend
  })
  print(obj)
}

###########################
# RG

read.fleet_TAF<-function()
{
  file<-file.path("./model",'fleet_names.in')
  s<-readLines(file, n=1000)
  s<-gsub('_',' ',s)
  s<-sub('[[:space:]]+$', '', s)
  
  file<-file.path("./model",'fleet_info.dat')
  finfo<-scan(file,skip=3,comment.char = "#",quiet =TRUE) 
  years<-rep(0,2)
  ages<-rep(0,3)
  
  
  nf<-finfo[1] #no. of fleets
  fl=NA
  
  for (f in (1:nf)) {
    fl[f]<-s[f]
  }
  
  fl<-cbind(fl[1],fl[2]) #change format to make compatible with previous sandeel script (multispecies therefore multiple rows)
  
}

###############################
#note my default is extend=TRUE
Read.summary.data_TAF<-function(infile='summary.out',extend=T)
  
{
  file<-file.path("./model/",infile)
  s<-read.table(file,header=TRUE)
  if (!extend) s<-subset(s,Z>-1)
  data.frame(Species="Area-1r",s)
}
############################
read.sms.dat_TAF <- function(label="eg.first.year/last.year.model",printlabel=F)
{
  label=paste0("option ",label,")")
  txt <- readLines("model/sms.dat")
  beg <- grep(label,txt,fixed = F) + 1
  if(length(beg)>0){
    end <- c(grep("####",txt,fixed=F), length(txt))
    end <- end[end>beg][1] -1
    title<-txt[beg-1]
    txt <- txt[beg:end]
    if(printlabel){     print(paste0(title," = ",txt))}
    
    if(grepl(pattern = "[A-Za-z]",txt)){return(txt)}else{ return(as.numeric(txt))}
  }else{
    warning(paste0("Label not found. Look in sms.dat file for appropriate label names (preceeded by option).
            Alternatively, try reversing the order of the label (ie. \"year.first\" becomes \"first.year\")"))}
}

##################################
Read.reference.points_TAF<-function(){
  
  a<-scan(file.path("./model","reference_points.in"),comment.char = "#",quiet = TRUE)
  b<-matrix(a,nrow=1,ncol=4,byrow=TRUE)
  colnames(b)<-c("Flim","Fpa","Blim","Bpa")   
  rownames(b)<-"Area 1-r"
  b
}
############
#Function to read parameters for SSB recruit relation model
Read.SSB.Rec.data_TAF<-function()
{
  file<-file.path("./model",'ssb_r.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species="Area-1r",s)
  }


##################
Read.SMS.std_TAF<-function() {
a<-read.table(file=file.path("./model","sms.std"),skip=1) 
tmp<-data.frame(index=a$V1,name=a$V2, value=a$V3, CV.round=round(a$V4/a$V3*100), std=a$V4)

b<-read.table(file.path("./model","par_exp.out"),comment.char = "#",header=T) 
tmp<-merge(tmp,b,by.x="index",by.y="parNo",all.x=T)
tmp<-subset(tmp,select=-par)
tmp<-subset(tmp,select=c(-prey,-predator))

return(tmp)
}

#####################
## Convert TAF csvs to mark down

taf2md <- function(taf_file_in,dir_in="report",dir_out,md_file_out=NA){
  
  taf<-read.csv(paste0(dir_in,"/",taf_file_in),header = T)
  md<-paste0('|', paste(names(taf), collapse = '|'), '|\n|', 
         paste(rep('---', length(taf)), collapse = '|'), '|\n|', 
         paste(Reduce(function(x, y){paste(x, y, sep = '|')}, taf), collapse = '|\n|'), '|')

  if(is.na(md_file_out)){md_file_out <- paste0(substr(taf_file_in,start = 1,stop = nchar(taf_file_in)-3),"md")}

  md_file_out<-paste0(dir_out,"/",md_file_out)
  writeLines(md,md_file_out)
}


###########################################################
## Adapted from the Sandeel utilities
#########################################################

SMS2FLIndices_TAF<-function(control,fleet.inf="fleet_info.dat",fleet.index="fleet_catch.in",
                            fleet.name="fleet_names.in") {
  control=control
  fleet.inf="fleet_info.dat"
  fleet.index="fleet_catch.in"
  fleet.name="fleet_names.in"
  old.wd<-getwd()
  
  nsp<-slot(control,"no.species")
  nq<-slot(control,"last.season")
  
  #count number of other predators    
  info<-slot(control,"species.info")[,"predator"]
  no.oth<-sum(info==2) 
  nsp<-nsp-no.oth  
  
  s<-readLines(paste0("model/",fleet.name), n=1000)
  s<-gsub('_',' ',s)
  fl.names<-sub('[[:space:]]+$', '', s)
  
  info<-scan(paste0("model/",fleet.inf),comment.char = "#",quiet=TRUE) 
  minCV<-info[1]
  i<-2
  n.fleet<-as.vector(info[i:(i-1+nsp)])
  i<-i+nsp
  sum.fleet<-sum(n.fleet)
  fl.info<-matrix(info[i:(i-1+sum.fleet*10)],ncol=10,nrow=sum.fleet,byrow=TRUE)
  i<-i+sum.fleet*10
  
  sum.var.age<-sum(fl.info[,10])
  fl.var<-as.vector(info[i:(i-1+sum.var.age)])
  
  CE<-scan(paste0("model/",fleet.index),comment.char = "#",quiet=TRUE) 
  
  # creates empty FLIndices object
  library(FLCore)
  FLIndices. <- FLIndices()       
  i<-1
  v<-1
  sp.fl<-0
  for (sp in 1:nsp) {
    for (fl in 1:n.fleet[sp]) {
      sp.fl<-sp.fl+1
      fy<-fl.info[sp.fl,1]
      ly<-fl.info[sp.fl,2]
      alfa<-fl.info[sp.fl,3]
      beta<-fl.info[sp.fl,4]
      fa<-fl.info[sp.fl,5]
      la<-fl.info[sp.fl,6]
      la.q<-fl.info[sp.fl,7]
      la.p<-fl.info[sp.fl,8]
      seas<-fl.info[sp.fl,9]
      n.var<-fl.info[sp.fl,10]
      
      nyr<-ly-fy+1
      nages<-la-fa+1
      
      
      # template for input to quant
      
      dim<-c(nages,nyr,1,1,1,1) 
      dim2<-c(1,nyr,1,1,1,1) 
      dimnames<-list(age=fa:la,year=fy:ly,unit="all",season=seas,area="all",iter="none")
      dimnames2<-list(age="all",year=fy:ly,unit="all",season=seas,area="all",iter="none")  
      
      tmp<-matrix(CE[i:(nyr*(nages+1)+i-1)],ncol=nages+1,nrow=nyr,byrow=TRUE)
      
      effort<-array(tmp[,1],dim=dim2,dimnames=dimnames2)
      catch<-matrix(tmp[,2:(nages+1)],ncol=nyr,nrow=nages,byrow=TRUE)
      #print(catch)
      catch<-array(catch,dim=dim,dimnames=dimnames)
      #print(catch)
      index<-catch/rep(effort,each=nages)
      
      indx<-FLIndex.SMS_TAF(index=as.FLQuant(index),effort=as.FLQuant(effort),catch.n=as.FLQuant(catch),
                            name = fl.names[sp.fl], desc = fl.names[sp.fl])
      
      
      indx@range<-unlist(list("min"=fa,"max"=la,"plusgroup"=NA,"minyear"=fy,"maxyear"=ly,"startf"=alfa,"endf"=beta))
      
      indx@range.SMS<-list("season"=seas, "power.age"=la.p, 
                           "q.age"=la.q,"var.age.group"=as.vector(fl.var[v:(v-1+n.var)]),"minCV"=minCV)
      
      v<-v+n.var
      i<-i+nyr*(nages+1)
      FLIndices.[[sp.fl]]<-indx 
    } 
  } 
  setwd(old.wd)
  FLIndices.
}



##################################

write.FLSMS.control_TAF <- function(control,file="sms.dat",path=NULL,write.multi=TRUE,nice=TRUE,writeSpNames=T) {
  
  wr.matrix<-function(m,text){
    cat("# ",text,"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(m[j,],"\n",file=file,append=TRUE)
  }
  
  wr.matrix.nice<-function(m,sp){
    for (j in (1:dim(m)[1])) cat(m[j,]," #",sp[j],"\n",file=file,append=TRUE)
  }
  
  wr.vector.nice<-function(m,sp){
    cat("# ",formatC(sp,width=11),"\n  ",formatC(m,width=11),"\n",file=file,append=TRUE)
  }
  
  wr.list<-function(l,text1,text2){
    for (j in 1:length(l)) cat(length(l[[j]])," ",file=file,append=TRUE) 
    cat("\t#",text1,"\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\n",file=file,append=TRUE)    
  }
  
  wr.list.nice<-function(l,text1,text2,sp){
    cat("#",text1,"\n#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    for (j in 1:length(l)) cat(formatC(length(l[[j]]),width=12),file=file,append=TRUE) 
    cat("\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\t# ",sp[j],"\n",file=file,append=TRUE)    
  }
  
  wr.list2<-function(l,text1,text2){
    for (j in 1:length(l)) {
      ifelse(l[[j]]==0, out<-0,out<-length(l[[j]]))
      cat(out," ",file=file,append=TRUE) 
    }
    cat("\t#",text1,"\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\n",file=file,append=TRUE)    
  }
  wr.list2.nice<-function(l,text1,text2,sp){
    cat("#",text1,"\n#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    for (j in 1:length(l)) {
      ifelse(l[[j]]==0, out<-0,out<-length(l[[j]]))
      cat(formatC(out,width=12),file=file,append=TRUE) 
    }
    cat("\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\t# ",sp[j],"\n",file=file,append=TRUE)    
  }
  
  if (!inherits(control, "FLSMS.control"))
    stop(paste("control" ,"must be an 'FLSMS.contol' object!")) 
  
  old.path<-getwd()
  if (!is.null(path)) setwd(path) else path<-old.path
  
  sepLine<-"########################################\n"
  last.pred<-1
  sp.names<-slot(control,"species.names")
  nsp<<-control@no.species
  for (ii in (1:nsp)) if (control@species.info[ii,'predator']!=2) {first.VPA<-ii; break;} #first VPA  species number
  VPA.species<-sp.names[first.VPA:length(sp.names)]
  for (ii in (1:nsp)) if (control@species.info[ii,'predator']==0) {last.pred<-ii-1; break;} #first VPA  species number
  pred.species<-sp.names[1:last.pred]
  
  
  cat("# sms.dat option file\n",file=file)
  cat('# the character "#" is used as comment character, such that all text and numbers\n# after # are skipped by the SMS program\n#\n',file=file, append=TRUE)
  n.<-slotNames(control)
  for (x in n.) {
    switch(x,
           "test.output"        ={ if (nice)  {
             cat(sepLine,file=file,append=TRUE) 
             cat("# Produce test output (option test.output)\n",
                 "#  0 no test output\n",
                 "#  1 output file sms.dat and  file fleet.info.dat as read in\n",
                 "#  2 output all single species input files as read in\n",
                 "#  3 output all multi species input files as read in\n",
                 "#  4 output option overview\n",
                 "#\n",
                 "# 11 output between phases output\n",
                 "# 12 output iteration (obj function) output\n", 
                 "# 13 output stomach parameters\n",
                 "# 19 Both 11, 12 and 13\n",
                 "#\n",
                 "# Forecast options\n",
                 "# 51 output hcr_option.dat file as read in\n",
                 "# 52 output prediction output summary\n",
                 "# 53 output prediction output detailed\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },
           "OP.output"          ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("# Produce output for SMS-OP program. 0=no, 1=yes\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "VPA.mode"          ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("# Single/Multispecies mode (option VPA.mode)\n",
                 "# 0=single species mode\n",
                 "# 1=multi species mode, but Z=F+M (used for initial food suitability parm. est.)\n",
                 "# 2=multi species mode, Z=F+M1+M2\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "no.areas"          ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("# Number of areas for multispecies run (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "first.year"        ={if (nice) {
             cat("#\n#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                 "# single species parameters\n#\n",
                 "#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                 file=file,append=T,sep='')
             cat("## first year of input data (option first.year)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "first.year.model"   ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## first year used in the model (option first.year.model)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           
           "last.year"         ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## last year of input data (option last.year)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "last.year.model"   ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## last year used in the model (option last.year.model)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "last.season"       ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("##  number of seasons (option last.season). Use 1 for annual data\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "last.season.last.year"={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## last season last year (option last.season.last.year). Use 1 for annual data\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "no.species"        ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## number of species (option no.species)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "species.names"      ={ cat(sepLine,file=file,append=T)
             cat("# Species names, for information only. See file species_names.in \n# ",sp.names,"\n",file=file,append=T)
           },
           "first.age"         ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## first age all species (option first.age)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "rec.season"        ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## recruitment season (option rec.season). Use 1 for annual data\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "max.age.all"       ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## maximum age for any species(max.age.all)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "species.info"      ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## various information by species\n",
                 "# 1. last age \n",
                 "# 2. first age where catch data are used (else F=0 assumed)\n",
                 "# 3. last age with age dependent fishing selection\n",
                 "# 4. Esimate F year effect from effort data. 0=no, 1=yes\n",
                 "# 5. Last age included in the catch at age likelihood (normally last age)\n",
                 "# 6. plus group, 0=no plus group, 1=plus group\n",
                 "# 7. predator species, 0=no, 1=VPA predator, 2=Other predator\n",
                 "# 8. prey species, 0=no, 1=yes\n",
                 "# 9. Stock Recruit relation\n",
                 "#      1=Ricker, 2=Beverton & Holt, 3=Geom mean,\n",
                 "#      4= Hockey stick, 5=hockey stick with smoother,\n",
                 "#      51=Ricker with estimated temp effect,\n",
                 "#      52=Ricker with known temp effect,\n",
                 "#      >100= hockey stick with known breakpoint (given as input)\n",
                 "# 10. Additional data for Stock Recruit relation\n",
                 "# 11. Additional data for Stock Recruit relation\n",
                 "##\n",
                 file=file,append=T,sep="")
             wr.matrix.nice(slot(control,x),paste(1:length(sp.names),sp.names))
           } else wr.matrix(slot(control,x),x)
           },
           "use.known.rec"       ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## use input recruitment estimate (option use.known.rec)\n",
                 "#   0=estimate all recruitments\n",
                 "#   1=yes use input recruitment from file known_recruitment.in\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "beta.cor"          ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## adjustment factor to bring the beta parameter close to one (option beta.cor)\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "SSB.R.year.first"  ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## year range for data included to fit the R-SSB relation (option SSB.R.year.range)\n",
                 "# first (option SSB.R.year.first) and last (option SSB.R.year.last) year to consider.\n",
                 "# the value -1 indicates the use of the first (and last) available year in time series\n",
                 "# first year by species\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "SSB.R.year.last"  ={if (nice) {
             cat("# last year by species\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "obj.func.weight"   ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Objective function weighting by species (option objective.function.weight)\n",
                 "# first=catch observations,\n",
                 "# second=CPUE observations,\n",
                 "# third=SSB/R relations\n",
                 "# fourth=stomach observations, weight proportions \n",
                 "# fifth=stomach observations, number at length \n",
                 "##\n",
                 file=file,append=T,sep="")
             wr.matrix.nice(slot(control,x),paste(1:length(sp.names),sp.names))
           } else wr.matrix(slot(control,x),x)
           },
           "phase.rec"         ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## parameter estimation phases for single species parameters\n",
                 "# phase.rec (stock numbers, first age) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "phase.rec.older"   ={if (nice) {
             cat("# phase.rec.older (stock numbers, first year and all ages) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "phase.F.y"         ={if (nice) {
             cat("# phase.F.y (year effect in F model) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "phase.F.y.spline"  ={if (nice) {
             cat("# phase.F.y.spline (year effect in F model, implemented as spline function)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "phase.F.q"         ={if (nice) {
             cat("# phase.F.q (season effect in F model) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "phase.F.a"         ={if (nice) {
             cat("# phase.F.a (age effect in F model) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "phase.catchability"={if (nice) {
             cat("# phase.catchability (survey catchability) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "phase.SSB.R.alfa"  ={if (nice) {
             cat("# phase.SSB.R.alfa (alfa parameter in SSB-recruitment relation) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "phase.SSB.R.beta"  ={if (nice) {
             cat("# phase.SSB.R.beta (beta parameter in SSB-recruitment relation) (default=1)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "min.catch.CV"      ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## minimum CV of catch observation used in ML-estimation (option min.catch.CV)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "min.SR.CV"         ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## minimum CV of catch SSB-recruitment relation used in ML-estimation (option min.SR.CV)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "discard"           ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Use proportion landed information in calculation of yield (option calc.discard)\n",
                 "#    0=all catches are included in yield\n",
                 "#    1=yield is calculated from proportion landed (file proportion_landed.in)\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "combined.catches"  ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## use seasonal or annual catches in the objective function (option combined.catches)\n",
                 "# do not change this options from default=0, without looking in the manual\n",
                 "#    0=annual catches with annual time steps or seasonal catches with seasonal time steps\n",
                 "#    1=annual catches with seasonal time steps, read seasonal relative F from file F_q_ini.in (default=0)\n",
                 file=file,append=T,sep="")   
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },                                                                                       
           "seasonal.catch.s2" ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## use seasonal or common combined variances for catch observation\n",
                 "# seasonal=0, common=1 (use 1 for annual data)\n", 
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },                      
           "catch.s2.group" ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## \n",
                 file=file,append=T)
             wr.list.nice(slot(control,x),"catch observations: number of separate catch variance groups by species",
                          "first age group in each catch variance group",VPA.species)
           } else  wr.list(slot(control,x),"n.catch.s2.group",x)
           },
           "catch.season.age"  ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## \n",
                 file=file,append=T)
             wr.list.nice(slot(control,x),"catch observations: number of separate catch seasonal component groups by species",
                          "first ages in each seasonal component group by species",VPA.species)
           } else  wr.list(slot(control,x),"n.catch.s2.group",x)
           },
           "avg.F.ages"        ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## first and last age in calculation of average F by species (option avg.F.ages)\n",
                 file=file,append=T)
             wr.matrix.nice(slot(control,x),VPA.species)
           } else wr.matrix(slot(control,x),x)
           },
           "min.catch"         = {if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## minimum 'observed' catch, (option min.catch). You cannot log zero catch at age!\n",
                 "#\n",
                 "# 0 ignore observation in likelihood\n#\n",
                 "# negative value gives percentage (e.g. -10 ~ 10%) of average catch in age-group for input catch=0\n",
                 "# negative value less than -100 substitute all catches by the option/100 /100 *average catch in the age group for catches less than (average catch*-option/10000\n",
                 "#\n",
                 "# if option>0 then will zero catches be replaced by catch=option\n",
                 "#\n",
                 "# else if option<0 and option >-100 and catch=0 then catches will be replaced by catch=average(catch at age)*(-option)/100\n",
                 "# else if option<-100  and catch < average(catch at age)*(-option)/10000 then catches will be replaced by catch=average(catch at age)*(-option)/10000\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "catch.sep.year"    ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## \n",
                 file=file,append=T)
             wr.list.nice(slot(control,x),"catch observations: number of year groups with the same age and seasonal selection","first year in each group (please note #1 will always be changed to first model year)",VPA.species)
           } else  wr.list(slot(control,x),"catch.sep.year",x)
           },
           "catch.spline.year"    ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## \n",
                 file=file,append=T)
             wr.list.nice(slot(control,x),"number of nodes for year effect Fishing mortality spline\n# 1=no spline (use one Fy for each year), >1 number of nodes","first year in each group",VPA.species)
           } else  wr.list(slot(control,x),"catch.spline.year",x)
           },
           "zero.catch.year.season"={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## year season combinations with zero catch (F=0) (option zero.catch.year.season)\n",
                 "# 0=no, all year-seasons have catchs,\n",
                 "# 1=yes there are year-season combinations with no catch.\n",
                 "#   Read from file zero_catch_seasons_ages.in\n",
                 "# default=0\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "zero.catch.season.age"={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## season age combinations with zero catch (F=0) (option zero.catch.season.ages)\n",
                 "# 0=no, all seasons have catchs,\n",
                 "# 1=yes there are seasons with no catch. Read from file zero_catch_season_ages.in\n",
                 "# default=0\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "fix.F.factor"      ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Factor for fixing last season effect in F-model (default=1) (fix.F.factor))\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "est.calc.sigma"  ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Uncertanties for catch, CPUE and SSB-R observations (option calc.est.sigma)\n",
                 "#  values: 0=estimate sigma as a parameter (the right way of doing it)\n",
                 "#          1=Calculate sigma and truncate if lower limit is reached \n",
                 "#          2=Calculate sigma and use a penalty function to avoid lower limit \n",
                 "#  catch-observation, CPUE-obs, Stock/recruit\n",
                 file=file,append=T,sep="")
             cat(formatC(slot(control,x),width=12),"\n",file=file,append=T,sep=" ")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "read.HCR"          = {if (nice) {
             cat(sepLine,file=file,append=T)
             cat("# Read HCR_option file (option=read.HCR) default=0 \n",
                 "#  0=no  1=yes\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
             if (!write.multi) break
           },                             
           "incl.stom.all"     = {if (nice) {
             cat(sepLine,file=file,append=T)
             cat("#\n#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                 "# multispecies parameters\n#\n",
                 "#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                 file=file,append=TRUE,sep='')
             cat("# Exclude year,season and predator combinations where stomach data are not incl.(option incl.stom.all)\n",
                 "#   0=no, all stomach data are used in likelihood\n",
                 "#   1=yes there are combinations for which data are not included in the likelihood.\n",
                 "#      Read from file: incl_stom.in\n",
                 "#   default(0)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },                                
           "use.Nbar"        = { if (nice)  {                              
             cat(sepLine,file=file,append=TRUE) 
             cat("##  N in the beginning of the period or N bar for calculation of M2 (option use.Nbar)\n",
                 "#  0=use N in the beginning of the time step (default)\n",
                 "#  1=use N bar\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },      
           "M2.iterations"   = {if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("## Maximum M2 iterations (option M2.iterations) in case of use.Nbar=1\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },      
           "max.M2.sum2"      = {if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("## convergence criteria (option max.M2.sum2) in case of use.Nbar=1\n",
                 "#  use max.M2.sum2=0.0 and M2.iterations=7 (or another high number) to make Hessian\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                            
           "stom.likelihood"   = {if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("## likelihood model for stomach content observations (option stom.likelihood)\n",
                 "#  1 =likelihood from prey weight proportions only (see option below)\n",
                 "#  2 =likelihood from prey weight proportions and from prey numbers to estimate size selection\n",
                 "#  3 =Gamma distribution for prey absolute weight and size selection from prey numbers\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },  
           "stomach.variance"   = {if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("# Variance used in likelihood model for stomach contents as prey weight proportion (option stomach.variance)\n",
                 "#  0 =not relevant, \n",
                 "#  1 =log normal distribution, \n",
                 "#  2 =normal distribution,\n",
                 "#  3 =Dirichlet distribution\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                             
           
           "simple.ALK"         = {if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("## Usage of age-length-keys for calc of M2 (option simple.ALK))\n",
                 "#  0=Use only one sizegroup per age (file lsea.in or west.in)\n",
                 "#  1=Use size distribution per age (file ALK_all.in)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },   
           "consum"            = {if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("## Usage of food-rations from input values or from size and regression parameters (option consum)\n",
                 "#  0=Use input values by age (file consum.in)\n",
                 "#  1=use weight at age (file west.in) and regression parameters (file consum_ab.in)\n",
                 "#  2=use length at age (file lsea.in), l-w relation and regression parameters (file consum_ab.in)\n",
                 "#  3=use mean length at size class (file ALK_all.in), l-w relation and regression parameters (file consum_ab.in)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                        
           "size.select.model"   ={if (nice)  {
             cat(sepLine,file=file,append=TRUE)
             cat("## Size selection model based on (option size.select.model)\n",
                 "#  1=length:\n",
                 "#      M2 calculation:\n",
                 "#         Size preference:\n",
                 "#           Predator length at age from file: lsea.in\n",
                 "#           Prey     length at age from file: lsea.in\n",
                 "#         Prey mean weight is weight in the sea from file: west.in\n",
                 "#      Likelihood:\n",
                 "#         Size preference:\n",
                 "#           Predator mean length per length group (file: stom_pred_length_at_sizecl.in) \n",
                 "#           Prey mean length per ength group (file stomlen_at_length.in \n",
                 "#         Prey mean weight from mean weight per prey length group (file: stomweight_at_length.in \n",
                 "#  2=weight:\n",
                 "#      M2 calculation:\n",
                 "#         Size preference:\n",
                 "#           Predator weight at age from file: west.in\n",
                 "#           Prey     weight at age from file: west.in\n",
                 "#         Prey mean weight is weight in the sea from file: west.in\n",
                 "#      Likelihood:\n",
                 "#         Size preference\n",
                 "#           Predator mean weight is based on mean length per predator length group (file: stom_pred_length_at_sizecl.in)\n",
                 "#              and l-w relation (file: length_weight_relations.in), \n",
                 "#           Prey mean weight per prey length group (file: stomweight_at_length.in) \n",
                 "#         Prey mean weight from mean weight per prey length group (file: stomweight_at_length.in \n",file=file,append=T,sep="")
             cat("#  3=weight:\n",
                 "#       M2 calculation: Same as option 2\n",
                 "#       Likelihood:\n",
                 "#         Size preference:\n",
                 "#           Predator mean weight is based on mean length per predator length group (file: stom_pred_length_at_sizecl.in)\n",
                 "#              and l-w relation (file: length_weight_relations.in), \n",
                 "#           Prey mean weight per prey length group (file: stomlen_at_length.in) and l-w relation (file:length_weight_relations.in)\n",
                 "#         Prey mean weight from prey mean length per prey length group (file: stomlen_at_length.in) and l-w relation (file: length_weight_relations.in) \n",
                 "#  4=weight:\n",
                 "#       M2 calculation:\n",
                 "#         Size preference:\n",
                 "#           Predator mean weight from file lsea.in (length in the sea) and l-w relation (file: length_weight_relations.in) \n",
                 "#           Prey mean weight from file lsea.in (length in the sea) and l-w relation (file: length_weight_relations.in) \n",
                 "#       Likelihood:  Same as option 3\n",
                 "#  5=weight in combination with simple.ALK=1:\n",
                 "#       M2 calculation:\n",
                 "#         Size preference:\n",
                 "#           Predator weight based on length from file ALK_all.in (length distribution at age) and l-w relation (file: length_weight_relations.in) \n",
                 "#           Prey     weight based on length from file ALK_all.in (length distribution at age) and l-w relation (file: length_weight_relations.in) \n",
                 "#         Prey mean weight based on length from file ALK_all.in (length distribution at age) and l-w relation (file: length_weight_relations.in) \n",
                 "#       Likelihood: Same as for option 2\n",
                 "#  6=weight in combination with simple.ALK=1:\n",
                 "#       M2 calculation: Same as option 5\n",
                 "#       Likelihood: Same as option 3\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           }
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },                                            
           "L50.mesh"           ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("# Adjust Length at Age distribution by a mesh selection function (option L50.mesh)\n",
                 "#  Please note that options simple.ALK shoud be 1 and option size.select.model should be 5\n",
                 "# L50 (mm) is optional given as input. Selection Range is estimated by the model\n",
                 "# L50= -1 do not adjust\n",
                 "# L50=0, estimate L50 and selection range\n",
                 "# L50>0, input L50 (mm) and estimate selection range\n",
                 "# by VPA species\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),VPA.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },                  
           "size.selection"        ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## spread of size selection (option size.selection)\n",
                 "#   0=no size selection, predator/preys size range defined from observations\n",
                 "#   1=normal distribution size selection\n",
                 "#   3=Gamma distribution size distribution\n",
                 "#   4=no size selection, but range defined by input min and max regression parameters (file pred_prey_size_range_param.in)\n",
                 "#   5=Beta distributed size distribution, within observed size range\n",
                 "#   6=log-Beta size distributed, within observed size range\n",
                 "#\n",
                 "# by predator\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           }, 
           "sum.stom.like"     ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## sum stomach contents over prey size for use in likelihood for prey weight proportions (option sum.stom.like)\n",
                 "#   0=no, use observations as they are; 1=yes, sum observed and predicted stomach contents before used in likelihood for prey weight proportions\n",
                 "#\n",
                 "# by predator\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           }, 
           "stom.obs.var"     ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## # Use estimated scaling factor to link number of observation to variance for stomach observation likelihood (option stom_obs_var)\n",
                 "#    0=no, do not estiamte factor (assumed=1);  1=yes, estimate the factor;  2=equal weight (1) for all samples\n",
                 "#\n",
                 "# by predator\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           }, 
           "stom.max.sumP"     ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## # Upper limit for Dirichlet sumP. A low value (e.g. 10) limits the risk of overfitting. A high value (e.g. 100) allows a full fit. (option stom_max_sumP)\n",
                 "# by predator\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           }, 
           "var.scale.stom"    ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Scaling factor (to bring parameters close to one) for relation between no of stomachs sampling and variance\n",
                 "#  value=0: use default values i.e. 1.00 for no size selection and otherwise 0.1 (option var.scale.stom)\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           
           "size.other.food.suit"={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## other food suitability size dependency  (option size.other.food.suit)\n",
                 "#  0=no size dependency\n",
                 "#  1=yes, other food suitability is different for different size classes\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "min.stom.cont"     ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Minimum observed relative stomach contents weight for inclusion in ML estimation (option min.stom.cont)\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "max.stom.sampl"     ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Upper limit for no of samples used for calculation of stomach observation variance (option max.stom.sampl)\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },
           "prey.pred.size.fac" ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## Max prey size/ pred size factor for inclusion in M2 calc (option max.prey.pred.size.fac)\n",
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },                                 
           "stom.type.include"  ={if (nice) {
             cat(sepLine,file=file,append=T)
             cat("## inclsion of individual stomach contents observations in ML for weight proportions (option stom.type.include)\n",
                 "# 1=Observed data\n",
                 "# 2= + (not observed) data within the observed size range (=fill in)\n",
                 "# 3= + (not observed) data outside an observed size range. One obs below and one above (=tails)\n",
                 "# 4= + (not observed) data for the full size range of a prey species irrespective of predator size (=expansion)\n", 
                 file=file,append=T,sep="")
             wr.vector.nice(slot(control,x),pred.species)
           } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
           },            
           
           "use.overlap"       ={if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("## use overlap input values by year and season (use.overlap)\n",
                 "#   0: overlap assumed constant or estimated within the model \n",
                 "#   1: overlap index from file overlap.in (assessment only, use overlap from last year in forecast)\n",
                 "#   2: overlap index from file overlap.in (assessment and forecast)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                              
           "phase.vulnera"     ={if (nice)  {                               
             cat(sepLine,file=file,append=TRUE) 
             cat("## parameter estimation phases for predation parameters\n",
                 "#  the number gives the phase, -1 means no estimation\n#\n",
                 "#  vulnerability (default=2) (phase phase.vulnera)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                               
           "phase.other.suit.slope"  ={if (nice)  {                                
             cat("# other food suitability slope (default=-1) (option phase.other.suit.slope)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                          
           "phase.pref.size.ratio"  ={if (nice)  {                                
             cat("# prefered size ratio (default=2) (option phase.pref.size.ratio)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                          
           "phase.pref.size.ratio.correction"={if (nice)  {                                
             cat("# predator size ratio adjustment factor (default=-1) (option phase.pref.size.ratio.correction))\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                 
           "phase.prey.size.adjustment" ={if (nice)  {                                
             cat("# prey species size adjustment factor (default=-1) (option phase.prey.size.adjustment)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                      
           "phase.var.size.ratio" =  {if (nice)  {                                
             cat("# variance of prefered size ratio (default=2) (option phase.var.size.ratio)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                              
           "phase.season.overlap"   ={if (nice)  {                                
             cat("# season overlap (default=-1) (option phase.season.overlap)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                             
           "phase.stom.var"     ={if (nice)  {                                
             cat("# Stomach variance parameter (default=2) (option phase.Stom.var)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },  
           "phase.mesh.adjust"     ={if (nice)  {                                
             cat("# Mesh size selection of stomach age length key (default=-1) (option phase.mesh.adjust)\n",
                 slot(control,x),"\n",file=file,append=T,sep="")
             cat(sepLine,file=file,append=T)
           } 
             else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
           },                                                                                                               
           #other wise                       
           # cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
    )  #end switch
  }
  
  if (writeSpNames) {
    sp.names<-slot(control,"species.names")
    sp.names<-substr(paste(sp.names,"___________",sep=''),1,11)
    sp.names<-gsub(' ','_',sp.names)
    write(sp.names,file=file.path(path,"species_names.in"))
    cat("12345678901\nPlease note. exactly 11 charaters for species names !!!!\n",file=file.path(path,"species_names.in"),append=TRUE) 
  }
  setwd(old.path)
}

###############################

FLIndex.SMS_TAF <- function(name=character(0), desc=character(0), distribution=character(0),
                            type=character(0), startf=NA, endf=NA, plusgroup=NA, season=NA, power.age=NA, 
                            q.age=NA,var.age.group=NA,minCV=0.3, ...) {
  
  args <- list(...)
  if(length(args)==0)
    args <- list(index=FLQuant())
  
  dimnames <- dimnames(args[[names(lapply(args, is.FLQuant)==TRUE)[1]]])
  sdimnames <- dimnames
  sdimnames[1] <- "all"
  
  if(!is.FLQuant(args['index']))
    index <- FLQuant(dimnames=dimnames)
  
  dims <- dims(index)
  
  new <- new("FLIndex.SMS", name = name, desc = desc, distribution = distribution,
             type=type,
             index = index, index.var = FLQuant(dimnames=dimnames),
             index.q = FLQuant(dimnames=dimnames), sel.pattern = FLQuant(dimnames=dimnames),
             catch.n = FLQuant(dimnames=dimnames), catch.wt = FLQuant(dimnames=dimnames),
             effort = FLQuant(dimnames=sdimnames), 
             range = unlist(list(min=dims$min, max=dims$max,
                                 plusgroup=NA, minyear=dims$minyear, maxyear=dims$maxyear, startf=startf, endf=endf)))
  range.SMS = unlist(list(season=season, power.age=power.age,
                          q.age=q.age,var.age.group=var.age.group, minCV=minCV))
  
  # Add extra arguments
  for(i in names(args)[names(args)!='iniFLQuant'])
    slot(new, i) <- args[[i]]
  
  return(new)
}  


############################



FLIndices2SMS_TAF<-function(indices=NULL,control=NULL,fleet.inf="fleet_info.dat",
                            fleet.index="fleet_catch.in",fleet.name="fleet_names.in") {
  old.wd<-getwd()
  
  
  if (is.null(indices))
    stop("A 'FLIndices' must be given")
  
  if (!inherits(indices, "FLIndices"))
    stop("indices must be an 'FLIndices' object!")
  
  for (i in 1:length(indices)) {
    if (is.na(indices[[i]]@range["startf"]) || is.na(indices[[i]]@range["endf"]))
      stop(paste("Must supply startf & endf for range in FLIndex",i))
    
    if (!all(names(indices[[i]]@range) == c("min","max","plusgroup","minyear","maxyear","startf","endf")))
      stop("Range must have names 'min','max','plusgroup','minyear','maxyear','startf','endf'")
  }
  
  if (!inherits(control, "FLSMS.control"))
    stop("control must be an 'FLSMS.control' object!")
  
  if (!validObject(control)) stop("control is not valid!")
  
  nsp<-slot(control,"no.species")
  #count number of other predators    
  info<-slot(control,"species.info")[,5]
  no.oth<-sum(info==2) 
  nsp<-nsp-no.oth  
  
  first.year<-slot(control,"first.year")
  last.year<-slot(control,"last.year.model")
  last.season<-slot(control,"last.season")
  n.season<-last.season
  
  no.indices<-rep(0,nsp)
  info<-matrix(0,ncol=10,nrow=length(indices))
  fl.name<-rep('',length(indices))
  v.age<-list()
  
  sp<-1; n<-1
  old.sp<-substr(indices[[1]]@desc,1,3)
  
  cat("# file fleet_catch.in\n",file=fleet.index)
  for (idc in indices) {
    fl.name[n]<-idc@name
    sp.name<-substr(idc@desc,1,3)
    if (nsp>1 & sp.name!=old.sp) {sp<-sp+1; old.sp<-sp.name}
    cat("# ",sp.name,",",fl.name[n],"\n",file=fleet.index,append=TRUE)
    no.indices[sp]=no.indices[sp]+1
    range<-idc@range
    info[n,1]<-range["minyear"]
    info[n,2]<-range["maxyear"]
    info[n,3]<-range["startf"]
    info[n,4]<-range["endf"]
    info[n,5]<-range["min"]
    info[n,6]<-range["max"]
    info[n,7]<-idc@range.SMS$q.age
    info[n,8]<-idc@range.SMS$power.age
    info[n,9]<-idc@range.SMS$season
    info[n,10]<-length(idc@range.SMS$var.age.group)
    v.age<-c(v.age,list(idc@range.SMS$var.age.group))
    minCV<-idc@range.SMS$minCV
    write.table(cbind(as.vector(idc@effort),
                      t(matrix(idc@catch.n,ncol=info[n,2]-info[n,1]+1,byrow=FALSE))),
                file=fleet.index,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
    n<-n+1
  }      
  
  cat(paste("# file: fleet_info.dat\n",minCV," #min CV of CPUE observations\n"),file=fleet.inf)
  cat("# number of fleets by species\n",file=fleet.inf,append=TRUE)
  write(no.indices,file=fleet.inf,ncolumns=nsp,append=TRUE)
  cat("#############", 
      "\n# 1-2, First year last year,",
      "\n# 3-4. Alpha and beta - the start and end of the fishing period for the fleet given as fractions of the season (or year if annual data are used),",
      "\n# 5-6   first and last age,",
      "\n# 7.   last age with age dependent catchability,", 
      "\n# 8.   last age for stock size dependent catchability (power model), -1 indicated no ages uses power model,",
      "\n# 9.   season for survey,",
      "\n# 10.  number of variance groups for estimated cathability,",
      "\n# by species and fleet",
      "\n#############\n", file=fleet.inf,append=TRUE)
  i<-0
  for (s in (1:nsp)) {
    cat("# ",control@species.names[s+no.oth],"\n",file=fleet.inf,append=TRUE)
    for (id in (1:no.indices[s])) {
      i<-i+1
      cat("# ",fl.name[i],"\n",file=fleet.inf,append=TRUE)
      write(file=fleet.inf ,info[i,],ncolumns=10,append=TRUE)
    }
  }
  
  #          write.table(file=fleet.inf ,info,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
  
  cat("# variance groups\n",file=fleet.inf,append=TRUE)
  #for (a in v.age) write(a,file=fleet.inf,append=TRUE)
  i<-0
  for (s in (1:nsp)) {
    cat("# ",control@species.names[s+no.oth],"\n",file=fleet.inf,append=TRUE)
    for (id in (1:no.indices[s])) {
      i<-i+1
      cat("# ",fl.name[i],"\n",file=fleet.inf,append=TRUE)
      write(file=fleet.inf ,v.age[[i]],append=TRUE)
    }
  }
  
  fl.name<-substr(paste(fl.name,"__________________________",sep=''),1,26)
  fl.name<-gsub(' ','_',fl.name)
  write(fl.name,file=fleet.name)
  
  setwd(old.wd)
}


###############################


read.FLSMS.control_TAF<-function(file="model/sms.dat") {       
  opt<-scan(file=file, comment.char = "#",quiet=T) 
  
  n<-1
  
  control<-new("FLSMS.control")
  n.<-slotNames(control)
  for (x in n.) {
    #print(paste0("begin:", x))
    #if(x!=n.[11]){
    switch(x,
           "no.species"          = {slot(control,x)<-as.integer(opt[n]); nsp<-as.integer(opt[n]); n<-n+1;
           species.names<-readLines("model/species_names.in", n=1)
           species.names<-gsub('_',' ',species.names)
           species.names<-sub('[[:space:]]+$', '', species.names)
           },
           "species.names"       = { slot(control,x)<-species.names; },
           
           "species.info"        = {    ncols<-11
           tmp<-matrix(opt[n:(n-1+nsp*ncols)],ncol=ncols,nrow=nsp,byrow=TRUE,
                       dimnames<-list(species.names,c("last-age","first-age F>0","last-age-selec","effort",
                                                      "last-age-likelihood","+group","predator","prey","SSB/R","RecAdd1","RecAdd2")));
           slot(control,x)<-tmp;
           n<-n+nsp*ncols;
           n.prey<-0; n.pred<-0; n.oth.pred<-0
           for (s in (1:nsp)) { 
             if(tmp[s,7]>=1) n.pred<-n.pred+1;
             if(tmp[s,7]==2) n.oth.pred<-n.oth.pred+1;
           }
           first.VPA<-n.oth.pred+1
           n.VPA.sp<-nsp-first.VPA+1
           },       
           
           "beta.cor"            = {slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
           "SSB.R.year.first"    ={slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
           "SSB.R.year.last"     ={slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
           "obj.func.weight"     = {slot(control,x)<-matrix(opt[n:(n-1+nsp*5)],ncol=5,nrow=nsp,byrow=TRUE,
                                                            dimnames=list(species.names,c("catch","survey","SSB/R","stomach1","stomach2"))); n<-n+nsp*5},  
           "min.catch.CV"        = {slot(control,x)<-as.numeric(opt[n]); n<-n+1},
           "min.SR.CV"           = {slot(control,x)<-as.numeric(opt[n]); n<-n+1},
           "discard"             ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp},
           "combined.catches"    ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp},
           "seasonal.catch.s2"   = {slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp},
           "catch.s2.group"      = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
           group<-vector("list", length=n.VPA.sp); 
           for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; } 
           slot(control,x)<-group},
           "catch.season.age"    = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
           group<-vector("list", length=n.VPA.sp); 
           for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; } 
           slot(control,x)<-group },                                 
           "avg.F.ages"          = {slot(control,x)<-matrix(opt[n:(n-1+n.VPA.sp*2)],ncol=2,nrow=n.VPA.sp,byrow=TRUE,
                                                            dimnames=list(species.names[first.VPA:nsp],c("first-age","last-age"))); n<-n+n.VPA.sp*2},
           "min.catch"           = {slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
           "catch.sep.year"      = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
           group<-vector("list", length=n.VPA.sp); 
           for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; } 
           slot(control,x)<-group },
           "catch.spline.year"    = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
           group<-vector("list", length=n.VPA.sp);
           for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; }
           slot(control,x)<-group },
           "fix.F.factor"        = {slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
           
           "est.calc.sigma"      = {slot(control,x)<-as.vector(opt[n:(n-1+3)]); n<-n+3},
           "L50.mesh"            ={slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
           "size.selection"      ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
           "sum.stom.like"       ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
           "stom.obs.var"        ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
           "stom.max.sumP"        ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
           "var.scale.stom"      ={slot(control,x)<-as.vector((opt[n:(n-1+n.pred)])); n<-n+n.pred},
           "size.other.food.suit"={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
           "min.stom.cont"       ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},
           "max.stom.sampl"       ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},
           "prey.pred.size.fac"  ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},
           "stom.type.include"   ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},                          
           # otherwise                        
           {slot(control,x)<-opt[n];n<-n+1}                                 
    )
  }
  #}
  control
}


#######################################################################################

##########################
## RESIDUAL FUNCTIONS

residplot_TAF<-function(residuals, xpos, ypos, maxsize = 0.25, poscol = 2, linecol = 1, lwd = 1, 
                        n = 50, maxn, negcol, txt = FALSE, csi = 0.1, xlab = "", ylab = "", main="", axes = T,
                        arg = T, argcol = 20, arglty = 2, cn = c("x", "y", "z"), append = F,refdot=F,start.year=0,end.year=0,Q){
  
  
  q=Q
  
  if(is.data.frame(residuals)) {
    x <- residuals[, cn[1]]
    y <- residuals[, cn[2]]
    residuals <- residuals[, cn[3]]
  }  else {
    residuals <- t(residuals)
    max.r <- max(abs(residuals), na.rm = T)
    
    if(missing(maxn))
      maxn <- max(abs(residuals), na.rm = T)
    if(missing(xpos))
      xpos <- 1:nrow(residuals)
    if(missing(ypos))
      ypos <- 1:ncol(residuals)
    x <- matrix(xpos, length(xpos), length(ypos))
    y <- matrix(ypos, length(xpos), length(ypos), byrow = T)
  }
  
  if(refdot) add.year<-6 else add.year<-2
  xx<-x; 
  xx[1,1]<-min(x)-2
  if (start.year >0) xx[1,1]<-start.year
  xx[1,2]<-max(x)+add.year
  if (end.year>0)xx[1,2]<-end.year
  
  # 
  plot(xx, y, type = "n", xlab = xlab, ylab = ylab, axes = FALSE, main=main, las=1,ylim=c(min(y)-0.5,max(y)+0.5))
  #plot(xx, y, type = "n",  axes = FALSE, main=main, las=1,ylim=c(min(y)-0.5,max(y)+0.5))
  
  axis(1)
  axis(2,min(ypos):max(ypos), tick=F,las=2)
  
  box()
  x.bck <- x
  y.bck <- y
  if(arg) {
    r <- x.bck - y.bck
    tmp <- unique(r)
    for(i in 1:length(tmp)) {
      j <- r == tmp[i]
      lines(x.bck[j], y.bck[j], col = argcol, lty = arglty)
    }
  }
  plt <- par()$pin
  xscale <- (par()$usr[2] - par()$usr[1])/plt[1] * maxsize
  yscale <- (par()$usr[4] - par()$usr[3])/plt[2] * maxsize
  rx <- c(unlist(sqrt(abs(residuals)/maxn) * xscale))
  ry <- c(unlist(sqrt(abs(residuals)/maxn) * yscale))
  theta <- seq(0, 2 * pi, length = n)
  n1 <- length(rx)
  theta <- matrix(theta, n1, n, byrow = T)
  x <- matrix(x, n1, n)
  y <- matrix(y, n1, n)
  rx <- matrix(rx, n1, n)
  ry <- matrix(ry, n1, n)
  x <- x + rx * cos(theta)
  y <- y + ry * sin(theta)
  x <- cbind(x, rep(NA, nrow(x)))
  y <- cbind(y, rep(NA, nrow(y)))
  x<-x
  i <- residuals > 0
  
  if(any(i)) {
    polygon(c(t(x[i,  ])), c(t(y[i,  ])), col = poscol)
    lines(c(t(x[i,  ])), c(t(y[i,  ])), col = linecol, lwd = lwd)
  }
  i <- residuals < 0
  if(any(i)) {
    if(!missing(negcol))
      polygon(c(t(x[i,  ])), c(t(y[i,  ])), col = negcol)
    lines(c(t(x[i,  ])), c(t(y[i,  ])), col = linecol, lwd = lwd)
  }
  if(txt)text(x.bck, y.bck, as.character(round(residuals)), csi = csi)
  
  if(refdot){
    # if (end.year==0) x <- max(xpos)+4 +xscale*cos(theta)
    # if (end.year>0)  x <- end.year-1 +xscale*cos(theta)
    if (end.year==0) x <- max(xpos)+4 +sqrt(max.r/maxn) * xscale*cos(theta)
    if (end.year>0)  x <- end.year-1  +sqrt(max.r/maxn) * xscale*cos(theta)
    
    #y <- max(max(ypos)%/%3+min(ypos),1) + yscale*sin(theta)     
    y <- max(max(ypos)%/%3+min(ypos),1) +sqrt(max.r/maxn)* yscale*sin(theta)     
    polygon(x, y, col = 7)
    if (end.year==0) text(max(xpos)+4, max(max(ypos)%/%3+min(ypos),1), as.character(round(max.r,digit=2)))
    if (end.year>0) text(end.year-1, max(max(ypos)%/%3+min(ypos),1), as.character(round(max.r,digit=2)))
  }
  return(invisible())
  
}

############
## Catch Resids Function

#Outer Function  for TAF



plot.catch.residuals_TAF<-function(start.year=2000,end.year=2020,reverse.colors=F,add.title=TRUE,over.all.max=1.5) {
  nox=1;noy=1
  
  taf.png("catch_residuals")
  
 use.ref.dot<-TRUE
  
  standardize=F
  dev='screen'
  Portrait=T
  use.ref.dot=TRUE
  add.title=F
  my.species=NA
  
  
  file<-file.path("model/catch_survey_residuals.out")
  res<-read.table(file,comment.char = "#",header=T)
  res<-subset(res,data=='catch')
  if (standardize) res$residual<- res$stand.residual
  res[res$residual==-99.9,'residual']<-NA
  quarters<-unique(res$Quarter)
  max.buble<-max(abs(res$residual),na.rm=TRUE)
  
  
  nsp=1
  nox.noy<-5
  plot.no<-0
  
  years<-rep(0,2)
  ages<-rep(0,2)
  aa<-res
  sp.name<-"Area-1r"
  
  quarters<-unique(aa$Quarter)
  
  par(mfrow=c(2,1))
  
  for (q in quarters) {
    nyr<-years[2]-years[1]+1
    nag<-ages[2]-ages[1]+1
    plot.no<-plot.no+1
    
    
    bb<-subset(aa,Quarter==q)
    tmp<-tapply(bb$residual,list(age=bb$Age,year=bb$Year),sum,na.rm=T)
    tmp[tmp==-99.99]<-0
    
    xpos <- as.numeric(dimnames(tmp)[[2]]) # years
    ypos <- as.numeric(dimnames(tmp)[[1]]) #ages
    title<- paste(sp.name," S:",q,sep="")
    
    residplot_TAF(tmp,xpos,ypos,main=title,refdot=use.ref.dot,start.year=start.year,end.year=end.year,maxn=over.all.max,Q=q)
    
  }
  dev.off()
}



###########
##Survey Resids Function
############

plot.survey.residuals_TAF<-function(start.year=1980,end.year=2020,use.ref.dot=TRUE,add.title=TRUE,over.all.max=1.5,standardize=F) {
  nox=1;noy=1
  my.species=NA
  fleet.names<-read.fleet_TAF()
  file<-file.path("./model","fleet_info.dat")
  finfo<-scan(file,comment.char = "#",quiet = T) 
  
  file<-file.path("./model",'catch_survey_residuals.out')
  res<-read.table(file,comment.char = "#",header=T)
  res<-subset(res,data=='survey')
  if (standardize) res$residual<- res$stand.residual
  res[res$residual==-99.9 ,'residual']<-NA
  
  max.buble<-max(abs(res$residual),na.rm=TRUE)
  
  nsp=1
  sp.name<-"Area-1r"
  nox.noy<-nox*noy
  plot.no<-0
  
  years<-rep(0,2)
  ages<-rep(0,2)
  
  my.species<-1
  aa<-subset(res,Species.n==1)
  nf<-ncol(fleet.names)
  
  for (f in 1:nf) {
    nyr<-years[2]-years[1]+1
    nyr=1
    nag<-ages[2]-ages[1]+1
    nag=1
    plot.no<-plot.no+1
    
    if (plot.no%%nox.noy==0 || f==1){
      png_name<-paste0("Survey_resids_Fleet_",f)
      taf.png(png_name)
      par(mar=c(3,4,3,2))
      plot.no<-0
    }
    
    bb<-subset(aa,fleet==f)
    tmp<-tapply(bb$residual,list(age=bb$Age,year=bb$Year),sum,na.rm=T)
    tmp[tmp==-99.99]<-0
    
    xpos <- as.numeric(dimnames(tmp)[[2]]) # years
    ypos <- as.numeric(dimnames(tmp)[[1]]) #ages
    
    # title<- paste(sp.name," fleet:",f,sep="")#"Area-1r fleet:1"
    title<- fleet.names[1,f]#"RTM 2007-2017"
    
    if (length(ypos)==1) {
      
      tmp2<-tmp
      tmp2[]<-0
      if (ypos[1]>=1) {
        tmp<-rbind(tmp2,tmp,tmp2)
        ypos <- (ypos-1):(ypos+1)
      }
      else if (ypos[1]==0) {
        tmp<-rbind(tmp,tmp2)
        ypos <- ypos:(ypos+1)
      }      
    }
    
    q=1
    
    if (over.all.max>0) {
      residplot_TAF(tmp,xpos,ypos,main=title,refdot=use.ref.dot,start.year=start.year,end.year=end.year,maxn=0.2,Q = q)
    }else{
      
      residplot_TAF(tmp,xpos,ypos,main=title,refdot=use.ref.dot,start.year=start.year,end.year=end.year,Q=q)
    }
    dev.off()
    
  }
  
}



####
## This is needed to create new FLSMS class objects

setClass("FLSMS.control",
         representation(
           test.output         ="numeric",
           OP.output           ="numeric",
           VPA.mode            ="numeric",
           no.areas            ="numeric",
           first.year          ="numeric",
           first.year.model    ="numeric",
           last.year           ="numeric",
           last.year.model     ="numeric",
           last.season         ="numeric",
           last.season.last.year="numeric",
           no.species          ="numeric",
           species.names       ="vector",
           first.age           ="numeric",
           rec.season          ="numeric",
           max.age.all         ="numeric",
           species.info        ="matrix",
           use.known.rec       ="numeric",
           beta.cor            ="vector",
           SSB.R.year.first    ="vector",
           SSB.R.year.last     ="vector",
           obj.func.weight     ="matrix",
           phase.rec           ="numeric",
           phase.rec.older     ="numeric",
           phase.F.y           ="numeric",
           phase.F.y.spline    ="numeric",
           phase.F.q           ="numeric",
           phase.F.a           ="numeric",
           phase.catchability  ="numeric",
           phase.SSB.R.alfa    ="numeric",
           phase.SSB.R.beta    ="numeric",
           min.catch.CV        ="numeric",
           min.SR.CV           ="numeric",
           discard             ="vector",
           combined.catches    ="vector",
           seasonal.catch.s2   ="vector",
           catch.s2.group      ="vector",
           catch.season.age    ="vector",
           avg.F.ages          ="matrix",
           min.catch           ="vector",
           catch.sep.year      ="vector",
           catch.spline.year   ="vector",
           zero.catch.year.season="numeric",
           zero.catch.season.age="numeric",
           fix.F.factor        ="vector",
           est.calc.sigma      ="vector",
           read.HCR            ="numeric",
           incl.stom.all       ="numeric",
           use.Nbar            ="numeric",
           M2.iterations       ="numeric",
           max.M2.sum2         ="numeric",
           stom.likelihood     ="numeric",
           stomach.variance    ="numeric",
           simple.ALK          ="numeric",
           consum              ="numeric",
           size.select.model   ="numeric",
           L50.mesh            ="vector",
           size.selection      ="vector",
           sum.stom.like       ="vector",
           stom.obs.var        ="vector",
           stom.max.sumP        ="vector",
           var.scale.stom      ="vector",
           size.other.food.suit="vector",
           min.stom.cont       ="vector",
           max.stom.sampl      ="vector",
           prey.pred.size.fac  ="vector",
           stom.type.include   ="vector",
           use.overlap         ="numeric",
           phase.vulnera       ="numeric",
           phase.other.suit.slope ="numeric",
           phase.pref.size.ratio  ="numeric",
           phase.pref.size.ratio.correction ="numeric",
           phase.prey.size.adjustment ="numeric",
           phase.var.size.ratio ="numeric",
           phase.season.overlap ="numeric",
           phase.stom.var       ="numeric",
           phase.mesh.adjust    ="numeric"
         )
         ,
         prototype=prototype(
           test.output     =0,
           OP.output       =0,
           VPA.mode        =0,
           no.areas        =1,                                            
           first.year      =1900, 
           first.year.model=1901,                                            
           last.year       =1901,                                            
           last.year.model =1901,                                            
           last.season     =1,                                            
           last.season.last.year=1,                                       
           no.species      =1,  
           species.names   =as.vector("sp1",mode="character"),                                         
           first.age       =0,                                            
           rec.season      =1,                                            
           max.age.all     =0,                                            
           species.info    =matrix(0,ncol=11,nrow=1,dimnames=list(c("sp1"),c("last-age","first-age F>0","last-age-selec","effort",
                                                                             "last-age-likelihood","+group","predator","prey","SSB/R","RecAdd1","RecAdd2"))),
           use.known.rec   =0,
           beta.cor        =as.vector(1.0E6,mode="numeric"),
           SSB.R.year.first=as.vector(0,mode="numeric"),
           SSB.R.year.last=as.vector(0,mode="numeric"),
           obj.func.weight =matrix(1,ncol=5,nrow=1,dimnames=list(c("sp1"),c("catch","survey","SSB/R",
                                                                            "stomach1","stomach2"))),                                        
           phase.rec       =1,                                                  
           phase.rec.older =1,                                                   
           phase.F.y       =1,
           phase.F.y.spline=-1,
           phase.F.q       =1,                                                  
           phase.F.a       =1,                                                  
           phase.catchability=1,                                                
           phase.SSB.R.alfa=1,                                                  
           phase.SSB.R.beta=1,                                                  
           min.catch.CV    =0.2,                                                
           min.SR.CV       =0.2,
           discard         =as.vector(0,mode="list"),
           combined.catches=as.vector(0,mode="list"),                                                 
           seasonal.catch.s2=as.vector(0,mode="list"),                                                 
           catch.s2.group  =as.vector(0,mode="list"),                                       
           catch.season.age=as.vector(0,mode="list"),                                       
           avg.F.ages      =matrix(0,ncol=2,nrow=1,dimnames=list(c("sp1"),c("first-age","last-age"))),                                        
           min.catch       =as.vector(-5,mode="numeric"),                                    
           catch.sep.year  =as.vector(0,mode="list"),
           catch.spline.year =as.vector(0,mode="list"),
           zero.catch.year.season =0,
           zero.catch.season.age =0,
           fix.F.factor    = as.vector(1,mode="numeric"),
           est.calc.sigma  =as.vector(0,mode="numeric"),                                   
           read.HCR        =0, 
           incl.stom.all   =0, 
           use.Nbar        =0,
           M2.iterations   =3,
           max.M2.sum2     =0.0, 
           stom.likelihood     =1,                                              
           stomach.variance    =1,                                              
           simple.ALK          =0, 
           consum              =0,                                            
           size.select.model   =2, 
           L50.mesh            =as.vector(0,mode="numeric"),                                                                           
           size.selection      =as.vector(0,mode="numeric"),  
           sum.stom.like       =as.vector(0,mode="numeric"),
           stom.obs.var        =as.vector(0,mode="numeric"),
           stom.max.sumP        =as.vector(0,mode="numeric"),
           var.scale.stom      =as.vector(0,mode="numeric"),
           size.other.food.suit=as.vector(0,mode="numeric"),
           min.stom.cont       =as.vector(0,mode="numeric"),
           max.stom.sampl      =as.vector(0,mode="numeric"),
           prey.pred.size.fac  =as.vector(0,mode="numeric"), 
           stom.type.include   =as.vector(1,mode="numeric"),                               
           use.overlap         =0,                                              
           phase.vulnera       =2,                                             
           phase.other.suit.slope =2,                                          
           phase.pref.size.ratio  =2,                                          
           phase.pref.size.ratio.correction =-1,                                
           phase.prey.size.adjustment =-1,                                      
           phase.var.size.ratio =-1,                                            
           phase.season.overlap =-1,                                            
           phase.stom.var       =2,
           phase.mesh.adjust    =-1                                                                                                              
         )                                                           
         
)


setClass("FLIndex.SMS",
         contains="FLIndex",
         representation(
           range.SMS="vector"
         ) ,
         prototype=prototype(range.SMS=list(season=1, power.age=-1, q.age=0,
                                            var.age.group=as.vector(0,mode="list"),minCV=0.3))
) 
