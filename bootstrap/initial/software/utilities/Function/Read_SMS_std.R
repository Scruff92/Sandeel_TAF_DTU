#Function to read sms.std
Read.SMS.std<-function(dir=data.path)
{
a<-read.table(file=file.path(dir,"sms.std"),skip=1) 
tmp<-data.frame(index=a$V1,name=a$V2, value=a$V3, CV.round=round(a$V4/a$V3*100), std=a$V4)

b<-read.table(file.path(dir,"par_exp.out"),comment.char = "#",header=T) 
tmp<-merge(tmp,b,by.x="index",by.y="parNo",all.x=T)
tmp<-subset(tmp,select=-par)
if (SMS.control@no.species==1)  tmp<-subset(tmp,select=c(-prey,-predator))
#if (SMS.control@no.areas==1)  tmp<-subset(tmp,select=c(-area))

return(tmp)
}

#Read.SMS.std()
