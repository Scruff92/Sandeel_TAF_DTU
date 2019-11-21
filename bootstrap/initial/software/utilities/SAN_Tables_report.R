
delete.Recruitment.last.year<-F  # delete recruitment in the last assessment year


#cleanup()
#########################################
file<-file.path(data.path,'summary_table_raw.out')
a<-read.table(file,header=TRUE)
a$Rec<-a$Rec
if (delete.Recruitment.last.year) a[a$Year==SMS.control@last.year,'Rec']<-NA

tab1<-cbind(a$Rec,a$TSB,a$SSB,a$SOP,a$mean.F)
rec<-a$Rec
rec<-rec[1:(length(rec)-2)]
geo<-c(exp(mean(log(rec),na.rm = T)),NA,NA,NA,NA)
tab1<-rbind(tab1,colMeans(tab1,na.rm = T),geo)

colnames(tab1)<-c("Recruits","TSB","SSB","Yield","Mean F")
rownames(tab1)<-c(a$Year,'arith. mean','geo. mean')

# overwrite with exp values from sms.std
if (TRUE) {
  tmp<-Read.SMS.std()
  tmp$name[tmp$name=="next_log_SSB"]<-'hist_log_SSB'
  geo<-exp(tmp[tmp$name=="GMminOne",'value'])
  a<-subset(tmp,name %in% c("hist_log_SSB","avg_log_sumF","log_recsd") & species>0 ,drop=TRUE)
  a$Species<-sp.names[a$species]
  a<-tapply(exp(a$value),list(a$year,a$name),sum)
  
  tab1[1:nrow(a),"Recruits"]<-a[,'log_recsd']
  tab1[1:nrow(a),"SSB"]<-a[,'hist_log_SSB']
  tab1[1:nrow(a),"Mean F"]<-a[,'avg_log_sumF']
  
  tab1['geo. mean','Recruits']<- geo
  
}


units<-c("(thousands)","(tonnes)","(tonnes)","(tonnes)",paste('ages ',SMS.control@avg.F.ages[1],'-', SMS.control@avg.F.ages[2],sep=''))
xtab(tab1, caption=paste("Table 1.",SMS.control@species.names,":   Estimated recruitment, total stock biomass (TBS), spawning stock biomass (SSB),",
                    " catch weight (Yield) and average fishing mortality."), cornername='Year',
     file=file.path(data.path,'_SAN_tab_summary.html'), dec=c(0,0,0,0,3), width='"100%"',units=units)


if (markD){ 
  colnames(tab1)<-c("Recruits (thousands)","TSB (tonnes)","SSB (tonnes)","Yield (tonnes)",paste("Mean F~",SMS.control@avg.F.ages[1],'-', SMS.control@avg.F.ages[2],'~',sep=''))
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=c(rep(0,4),3))))
  writeLines(md,con=file.path(mdDir,'_SAN_tab_summary.md'))
}  

#####################
# sum file
file<-file.path(data.path,'summary_table_raw.out')
a<-read.table(file,header=TRUE)
a$Rec<-a$Rec
if (delete.Recruitment.last.year) a[a$Year==SMS.control@last.year,'Rec']<-NA

tab1<-cbind(a$Year,a$Rec,a$TSB,a$SSB,a$SOP,a$SOP/a$SSB,a$mean.F)

outfile<-file.path(data.path,'SAN_34.sum')
cat(",            RECRUITS,    TOTALBIO,    TOTSPBIO,    LANDINGS,   YIELD/SSB,  FBAR  1-2, \n",file=outfile)
cat(",Age 0 \n",file=outfile,append=T)

tab2<-cbind(formatC(a$Year,format='d'),
           formatC(round(a$Rec,0),format='f',digits=0),
           formatC(round(a$TSB,0),format='f',digits=0),
           formatC(round(a$SSB,0),format='f',digits=0),
           formatC(round(a$SOP,0),format='f',digits=0),
           formatC(round(a$SOP/a$SSB,3),format='f',digits=3),
           formatC(round(a$mean.F,3),format='f',digits=3))
tab2[tab2=='NA']<-' '
tab2[tab2=='  NA']<-' '
outfile<-file.path(data.path,'SAN_34.sum')
cat(",            RECRUITS,    TOTALBIO,    TOTSPBIO,    LANDINGS,   YIELD/SSB,  FBAR  1-2, \n",file=outfile)
cat(",Age 0 \n",file=outfile,append=T)
write.table(tab2,file=outfile,append=T,row.names = F,col.names=F,na=' ',sep=',',quote=F)

cat("Arith. \n Mean,",file=outfile,append=T)
b<-colMeans(tab1[,2:7],na.rm=T)
tab2<-cbind(formatC(round(b[1],0),format='f',digits=0),
           formatC(round(b[2],0),format='f',digits=0),
           formatC(round(b[3],0),format='f',digits=0),
           formatC(round(b[4],0),format='f',digits=0),
           formatC(round(b[5],3),format='f',digits=3),
           formatC(round(b[6],3),format='f',digits=3))

write.table(tab2,file=outfile,append=T,row.names = F,col.names=F,na=' ',sep=',',quote=F)
cat("0 Units,   (Million),    (Tonnes),    (Tonnes),    (Tonnes),\n",file=outfile,append=T)


################################
a<-Read.summary.data(extend=T)
a<-subset(a,(Quarter==1 & Age>0) | (Quarter=SMS.control@rec.season & Age==SMS.control@first.age))
tab1<-tapply(a$N,list(a$Year,a$Age),sum)/1000
if (delete.Recruitment.last.year) {
  tab1[as.character(SMS.control@last.year),'0']<-NA
  tab1[as.character(SMS.control@last.year+1),'1']<-NA
}


colnames(tab1)<-paste("Age",seq(SMS.control@first.age,SMS.control@max.age.all))
tab1[dim(tab1)[1],1]<-NA


xtab(tab1, caption=paste("Table XX.",SMS.control@species.names,":   Stock numbers (millions). Age 0 at start of 2nd half-year, age 1+ at start of 1st half-year"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_N.html'), dec=rep(0,SMS.control@max.age.all-SMS.control@first.age+1), width='"100%"')

if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=0)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_N.md'))
} 

################################
age.lab<-c("Age 0, 2nd half","Age 1, 1st half","Age 1, 2nd half",
                                    "Age 2, 1st half","Age 2, 2nd half",
                                    "Age 3, 1st half","Age 3, 2nd half",
                                    "Age 4+, 1st half","Age 4+, 2nd half")

a<-Read.summary.data()
tab1<-tapply(a$C.obs,list(a$Year,a$Quarter,a$Age),sum)/1000
tab1<-cbind(tab1[,2,1],tab1[,,2], tab1[,,3],tab1[,,4],tab1[,,5])

colnames(tab1)<-age.lab
rnames<-rownames(tab1)
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(rnames,'arith. mean')
xtab(tab1, caption=paste("Table 1.",SMS.control@species.names,"Sandeel. Catch at age numbers (millions) by half year"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_C.html'), dec=rep(0,dim(tab1)[2]), width='"100%"')


if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=0)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_C.md'))
} 

#fil<-'catch_at_age_proportion'
#newplot(dev=My.device,filename=fil,nox=1,noy=1,Portrait=F);
#par(mar=c(5,4,4,5)+.1) 
# here we need to make a plot of catach at age  proportion 
a<-Read.summary.data()
#a<-subset(a,Year>=1995)
#tab1<-tapply(a$C.obs,list(a$Year,a$Age),sum)
#tab2<-tapply(a$C.obs,list(a$Year),sum)
#tab1<-tab1/rep(tab2,times=dim(tab1)[2])

#colnames(tab1)<-paste('Age',0:4)
library(plyr)
library(ggplot2)
tab4=ddply(a, ~Year, mutate, YearTot=sum(C.obs))
tab5=ddply(tab4, ~Year+Age, summarize, p=sum(C.obs)/YearTot[1])
tab5$Age=factor(tab5$Age)
ggplot(tab5, aes(x=Year, y=p))+geom_col(aes(fill=Age))+theme_bw()+ylab("Proportion at age")
#barplot(t(tab1),legend=colnames(tab1),args.legend = list(x = "topleft"), ylab='Proportion at age')

if (My.device=='screen'){
#  savePlot(filename = file.path(mdDir,fil),type = "png")
  ggsave(file.path(mdDir, "catch_at_age_proportion.png"), width=5, height=4)
} else cleanup()



# # 
# ################################
# age.lab<-c("Age 0, 2nd half","Age 1, 1st half","Age 1, 2nd half",
                                    # "Age 2, 1st half","Age 2, 2nd half",
                                    # "Age 3, 1st half","Age 3, 2nd half",
                                    # "Age 4+, 1st half","Age 4+, 2nd half")

# a<-Read.summary.data()
# tab1<-tapply(a$C.obs,list(a$Year,a$Quarter,a$Age),sum)/1000
# tab1<-cbind(tab1[,2,1],tab1[,,2], tab1[,,3],tab1[,,4],tab1[,,5])

# colnames(tab1)<-age.lab
# rnames<-rownames(tab1)
# tab1<-rbind(tab1,colMeans(tab1))
# rownames(tab1)<-c(rnames,'arith. mean')
# xtab(tab1, caption=paste("Table 1.",SMS.control@species.names,"Sandeel. Catch at age numbers (millions) by half year"),
     # cornername='Year/Age',
     # file=file.path(data.path,'_tab_SAN_C.html'), dec=rep(0,dim(tab1)[2]), width='"100%"')


# if (markD){ 
  # md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=0)))
  # writeLines(md,con=file.path(mdDir,'_tab_SAN_C.md'))
# } 

# fil<-'catch_at_age_proportion'
# newplot(dev=My.device,filename=fil,nox=1,noy=1,Portrait=F);
# par(mar=c(5,4,4,5)+.1) 
# # here we need to make a plot of catach at age  proportion 
# a<-Read.summary.data()
# a<-subset(a,Year>=1995)
# tab1<-tapply(a$C.obs,list(a$Year,a$Age),sum)
# tab2<-tapply(a$C.obs,list(a$Year),sum)
# tab1<-tab1/rep(tab2,times=dim(tab1)[2])
# colnames(tab1)<-paste('Age',0:4)
# barplot(t(tab1),legend=colnames(tab1),args.legend = list(x = "topleft"), ylab='Proportion at age')
# if (My.device=='screen'){
  # savePlot(filename = file.path(mdDir,fil),type = "png")
# } else cleanup()



################################
a<-Read.summary.data(extend=T)
a<-subset(a,Quarter==1)
tab1<-tapply(a$propmat,list(a$Year,a$Age),sum)

colnames(tab1)<-paste('Age',1:4)

xtab(tab1, caption=paste("Table 12.",SMS.control@species.names,"Sandeel. Proportion mature at age "),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_ProportionMature.html'), dec=rep(2,dim(tab1)[2]), width='"100%"')

#Changed after 2016 benchmark so that maturity does not vary by year
#tab1<-tab1[as.character(2003:SMS.control@last.year+1),]
#dimnames(tab1)[[1]][1]<-'1983-2004'
dimnames(tab1)[[1]][1]<-'1983-2016'
tab1<-tab1[1,, drop=FALSE]

if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=2)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_ProportionMature.md'))
} 
################################
#Natural mortality varies across years
a<-Read.summary.data()
tab1<-tapply(a$M,list(a$Year,a$Quarter,a$Age),sum)
tab1<-cbind(tab1[,2,1],tab1[,,2], tab1[,,3],tab1[,,4],tab1[,,5])

colnames(tab1)<-age.lab
rnames<-rownames(tab1)
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(rnames,'arith. mean')

xtab(tab1, caption=paste("Table 10.",SMS.control@species.names,"Sandeel. Natural mortality rate"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_M.html'), dec=rep(1,dim(tab1)[2]), width='"100%"')

if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=3)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_M.md'))
} 
################################
a<-Read.summary.data()
tab1<-tapply(a$weca*1000,list(a$Year,a$Quarter,a$Age),sum)
tab1<-cbind(tab1[,2,1],tab1[,,2], tab1[,,3],tab1[,,4],tab1[,,5])

colnames(tab1)<-age.lab
rnames<-rownames(tab1)
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(rnames,'arith. mean')

xtab(tab1, caption=paste("Table 10.",SMS.control@species.names,"Sandeel. Individual mean weight(g) at age in the catch and in the sea"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_W.html'), dec=rep(1,dim(tab1)[2]), width='"100%"')

if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=1)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_W.md'))
} 

################################

a<-Read.summary.data()
tab1<-tapply(a$weca*1000,list(a$Year,a$Age,a$Quarter),sum)
colnames(tab1)<-paste('Age_',0:4,sep='')
a<-rbind(
   data.frame(tab1[,,1],Year=rownames(tab1[,,1]),HalfYear='First half year'),
   data.frame(tab1[,,2],Year=rownames(tab1[,,2]),HalfYear='Second half year')
  )

if (My.device=='png') cex=2 else cex=0.7
newplot(dev=My.device,filename='mean_weight_at_age',nox=1,noy=1,Portrait=T,w11=10);
par(mar=c(5,4,4,5)+.1) 
print(xyplot(Age_0+Age_1+Age_2+Age_3+Age_4 ~as.numeric(as.character(Year)) | HalfYear,data=a,type='b',col=1:5,lwd=3,lty=1,pch=1:5,cex=cex,
scales = list(x = list( cex=cex*1.2, rot = 90),y = list( cex=cex*1.5)),
relation='free', par.strip.text=list(cex=cex*1.5),as.table=T,
auto.key = list(space = "bottom",columns = 5,col=1:5,points=F),
layout=c(1,2), ylab=list('Mean weight (gram)',cex=cex*1.5),xlab='' ))

if (My.device=='screen'){
  savePlot(filename = file.path(mdDir,'mean_weight_at_age'),type = "png")
} else cleanup()


########################
a<-Read.summary.data()
tab1<-tapply(a$F,list(a$Year,a$Quarter,a$Age),sum)
tab1<-cbind(tab1[,2,1],tab1[,,2], tab1[,,3],tab1[,,4],tab1[,,5])

colnames(tab1)<-age.lab
rnames<-rownames(tab1)
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(rnames,'arith. mean')

xtab(tab1, caption=paste("Table 10.",SMS.control@species.names,"Sandeel. Fishing mortality at age"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_F.html'), dec=rep(3,dim(tab1)[2]), width='"100%"')

if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=3)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_F.md'))
} 

################################
a<-Read.summary.data()

a$deadM<-a$N.bar*a$M
a$deadC<-a$N.bar*a$F
a$deadAll<-a$N.bar*a$Z

a<-subset(a,select=c(Year,Age,deadM,deadC,deadAll,Z))
a<-aggregate(list(deadM=a$deadM,deadC=a$deadC,deadAll=a$deadAll,Z=a$Z),list(Year=a$Year,Age=a$Age),sum)
a$FF<-a$Z*a$deadC/a$deadAll
a$M<- a$Z*a$deadM/a$deadAll

tab1<-tapply(a$FF,list(a$Year,a$Age),sum)
tmp<-tab1[,as.character(SMS.control@avg.F.ages[1,1]:SMS.control@avg.F.ages[1,2])]
tab1<-cbind(tab1,rowMeans(tmp))

colnames(tab1)<-c(paste("Age",seq(SMS.control@first.age,SMS.control@max.age.all)),
               paste("Avg. ",SMS.control@avg.F.ages[1,1],'-',SMS.control@avg.F.ages[1,2],sep=''))
rnames<-rownames(tab1)
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(rnames,'arith. mean')

xtab(tab1, caption=paste("Table 10.",SMS.control@species.names,":   Annual Fishing mortality (F) at age"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_F_annual.html'), dec=rep(3,SMS.control@max.age.all-SMS.control@first.age+2), width='"100%"')

if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=3)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_F_annual.md'))
} 


tab1<-tapply(a$M,list(a$Year,a$Age),sum)
colnames(tab1)<-c(paste("Age",seq(SMS.control@first.age,SMS.control@max.age.all)))
rnames<-rownames(tab1)
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(rnames,'arith. mean')

xtab(tab1, caption=paste("Table 10.",SMS.control@species.names,":   Annual natural mortality (M) at age"),
     cornername='Year/Age',
     file=file.path(data.path,'_tab_SAN_M_annual.html'), dec=rep(3,SMS.control@max.age.all-SMS.control@first.age+2), width='"100%"')

if (markD){ 
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=2)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_M_annual.md'))
} 

