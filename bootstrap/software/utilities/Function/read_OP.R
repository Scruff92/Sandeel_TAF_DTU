#Function to read summary data
Read.OP.par<-function(dir=data.path,infile='OP_optim_par.out')
{
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}

#Function to read summary data
Read.OP.condensed<-function(dir=data.path,infile='OP_condensed.out')
{
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}



#Function to read summary data
Read.OP.community.indicator<-function(dir=data.path,infile='OP_indicator_system.out')
{
  file<-file.path(dir,infile)
  read.table(file,header=TRUE)
}

Read.OP.other<-function(dir=data.path,infile="OP_other_sp.out")
{
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Species.n],s)
}

#Read refence points from file reference_points.in 

Read.reference.points.OP<-function(dir=data.path){
    a<-scan(file.path(dir,"OP_reference_points.in"),comment.char = "#",quiet = TRUE)
    b<-matrix(a,ncol=4,byrow=TRUE)
    colnames(b)<-c("Flim","Fpa","Blim","Bpa")   
    rownames(b)<-sp.names[first.VPA:nsp]
    b
}
