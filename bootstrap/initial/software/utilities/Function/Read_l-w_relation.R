Read.length.weight.relation<-function(dir=data.path)
{
    file<-file.path(dir,'Length_weight_relations.in')
    a<-scan(file,comment.char = "#") 
    a<-matrix(a,nrow=nsp,ncol=2,byrow=T)
    data.frame(Species.n=seq(1,nsp),Species=sp.names,a=a[,1],b=a[,2])
}
