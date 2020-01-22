# filenames for input data
newCW<-'Total_catch_in_numbers_and_mean_weight_dec_2018.csv'

first.year.out<-1983
last.year.out<-2018

#areas=c(4,6,8,9,10,11,12)  # Areas to be included in the Table
areas=1:7  # Areas to be included in the Table

####################################

a<-read.table(file=file.path(root,'data',newCW),header=T,sep=',')
a<-read.csv(file=file.path(root,'data',newCW))


a<-subset(a,Area %in% areas & aar %in% (first.year.out:last.year.out))
b<-subset(a,select=c(aar,hy, Area, n_samples))
head(b)
tab1<-tapply(b$n_samples,list(Year=b$aar,Area=b$Area),sum,na.rm=T)
tab1[is.na(tab1)]<-0
tab1<-cbind(tab1,rowSums(tab1,na.rm=T))
colnames(tab1)<- c(paste('Area', areas),'All')
rnames<-rownames(tab1)
tab1<-rbind(tab1,colSums(tab1,na.rm=T))
rownames(tab1)<-c(rnames,'Sum')
  units<-rep(' ',length(areas)+1)

xtab(tab1, caption="Table 1.  Number of biological samples available to ICES.", cornername='Year',
   file=file.path(data.path,paste('_SAN_tab_Sampling_intensity.html',sep='')), dec=rep(0,length(areas)+1), width='"100%"',units=units)
                 
md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=0)))
writeLines(md,con=file.path(mdDir,'_SAN_tab_Sampling_intensity.md'))                
