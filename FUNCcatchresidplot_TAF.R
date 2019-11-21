
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


#Outer Function  for TAF

over.all.max<-6  # use over.all.max different from 0 to set bublesize  
over.all.max<-2.5

use.ref.dot<-TRUE


plot.catch.residuals_TAF<-function(dev,nox=1,noy=1,Portrait=T,start.year=0,end.year=0,reverse.colors=F,standardize=F,use.ref.dot=TRUE,add.title=TRUE,over.all.max=1.5,my.species=NA) {
  
  png_name<-paste0("catch_residuals")
  taf.png(png_name)
  
  standardize=F
  dev='screen'
  # nox=1
  # noy=2
  Portrait=T
  use.ref.dot=TRUE
  add.title=F
  over.all.max=4
  start.year=1982
  end.year=2021
  
  
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
  res
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


#######################################################################