############

plot.survey.residuals_TAF<-function(nox=1,noy=1,start.year=0,end.year=0,use.ref.dot=TRUE,add.title=TRUE,over.all.max=1.5,my.species=NA,standardize=F) {
  
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
  
  
  nox.noy<-nox*noy
  plot.no<-0
  
  years<-rep(0,2)
  ages<-rep(0,2)
  
  my.species<-1
  
  aa<-subset(res,Species.n==1)
  nf<-finfo[sp+1] 

  
  sp.name<-"Area-1r"
  

  for (f in 1:nf) {
    
    
    # print(paste("sp:",sp,"  fleet:",f))
    
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
    
    title<- paste(sp.name," fleet:",f,sep="")#"Area-1r fleet:1"
    title<- fleet.names[sp,f]#"RTM 2007-2017"
    
    
    
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
    #print(tmp)
    q=1
  
    
    if (over.all.max>0) {
      residplot_TAF(tmp,xpos,ypos,main=title,refdot=use.ref.dot,start.year=start.year,end.year=end.year,maxn=over.all.max,Q = q)
      }else{
        
      residplot_TAF(tmp,xpos,ypos,main=title,refdot=use.ref.dot,start.year=start.year,end.year=end.year,Q=q)
        }
    dev.off()
    
  }
  
}


