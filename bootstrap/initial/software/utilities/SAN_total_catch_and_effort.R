## remeber to set the variable below
getTonsFrom<-c('effortFile','CatchAtAgeFile')[2]

effortFile<-'effort_to_sandeel_assessment_1982_2018_new_norwegian.csv'
CatchAtAgeFile<-'Total_catch_in_numbers_and_mean_weight_dec_2018.csv'
# area_names=c("8","9","10","4","11","6","12") #names used in effortFile and CatchAtAgeFile (ordered as they should appear in table)
# replace_names <- function(Area){
# 	Area <- gsub("8", "1r", Area)
# 	Area <- gsub("9", "2r", Area)
# 	Area <- gsub("10", "3r", Area)
# 	Area <- gsub("11", "5r", Area)
# 	Area <- gsub("12", "7r", Area)
# }
area_names=c("1","2","3","4","5","6","7") #names used in effortFile and CatchAtAgeFile (ordered as they should appear in table)
replace_names <- function(Area){
  Area <- gsub("1", "1r", Area)
  Area <- gsub("2", "2r", Area)
  Area <- gsub("3", "3r", Area)
  Area <- gsub("5", "5r", Area)
  Area <- gsub("7", "7r", Area)
}
a<-read.csv(file.path(root,'data',effortFile))  

a<-subset(a,year>=1983)
#Area hy year  effort1  effort2  effort3  aar      ton

tab_effort_tons<-function(hy=1,b=1,labs='Hej',dtype='effort',areas=area_names, replace= replace_names) {

  if (hy==9) {
    b<-apply(b,c(2,1),sum)
    tab1<-b[,areas[1]]
    for (i in areas[-1]) tab1<-cbind(tab1,b[,i])

  } else {
     tab1<-b[areas[1],,hy]
     for (i in areas[-1]) tab1<-cbind(tab1,b[i,,hy])
  }
  
  tab1<-cbind(tab1,rowSums(tab1))
  colnames(tab1)<- c(paste('Area', replace_names(areas)),'All')

  rnames<-rownames(tab1)
  tab1<-rbind(tab1,colMeans(tab1))
  rownames(tab1)<-c(rnames,'arith. mean')

  units<-rep(' ',length(areas)+1)
  fil<-paste('_SAN_tab_',dtype,'_',hy,sep='')
  xtab(tab1, caption=paste("Table 1.",labs  ), cornername='Year',
       file=file.path(data.path,paste(fil,'.html',sep='')), dec=rep(0,length(areas)+1), width='"100%"',units=units)
  
  
  md<-(gsub(' NA','   ' ,knitr::kable(tab1,digits=0)))
  writeLines(md,con=file.path(mdDir,paste(fil,'.md',sep='')))
}

# effort
bb<-tapply(a$effort2,list(a$Area,a$year,a$hy),sum,na.rm=T)

tab_effort_tons(hy=1,b=bb,labs="Effort (days fishing for a standard 200 GT vessel) first half year as estimated by ICES",dtype='effort',areas=area_names)
tab_effort_tons(hy=2,b=bb,labs="Effort (days fishing for a standard 200 GT vessel) second half year as estimated by ICES",dtype='effort',areas= area_names)
tab_effort_tons(hy=9,b=bb,labs="Effort (days fishing for a standard 200 GT vessel) as estimated by ICES",dtype='effort',areas= area_names)   # sum


if (getTonsFrom=='CatchAtAgeFile') {  # read tons from catact at age file 
  b<-read.csv(file.path(root,'data',CatchAtAgeFile))  
  b$year<-b$aar
  b<-subset(b,year>=1983,select=c(Area, hy, year,ton))
  
  a$ton<-NULL # delete ton from effortFile
  a<-merge(a,b)  # get ton from catach at age file
}

# tons
bb<-tapply(a$ton,list(a$Area,a$year,a$hy),sum,na.rm=T)

tab_effort_tons(hy=1,b=bb,labs="Total catch (tonnes) by area, first half year as estimated by ICES",dtype='landings')
tab_effort_tons(hy=2,b=bb,labs="Total catch (tonnes) by area, second half year as estimated by ICES",dtype='landings')
tab_effort_tons(hy=9,b=bb,labs="Total catch (tonnes) by area as estimated by ICES",dtype='landings')   # sum


# plot CPUE
eff<-tapply(a$effort2,list(a$Area,a$year),sum,na.rm=T)
CPUE<-tapply(a$ton,list(a$Area,a$year),sum,na.rm=T)/eff
CPUE[eff<10]<-NA
CPUE[4,"2011"]<-NA
years<-as.numeric(unlist(dimnames(CPUE)[2]))
CPUE<-CPUE[1:4,]

newplot(dev=My.device,filename='total_catch_and_effort',nox=1,noy=1,Portrait=F);
par(mar=c(5,4,4,5)+.1)

plot(years,CPUE[1,],type='b',xlab=' ',ylab='Standardised CPUE',col=1,lty=1,lwd=3,pch='1',ylim=c(0,max(CPUE,na.rm=T)),main=' ')
lines(years,CPUE[2,],col=2,lty=2,pch='2',lwd=3,type='b')
lines(years,CPUE[3,],col=3,lty=3,pch='3',lwd=3,type='b')
lines(years,CPUE[4,],col=4,lty=4,pch='4',lwd=3,type='b')

if (My.device=='screen'){
   savePlot(filename = file.path(mdDir,'total_catch_and_effort'),type = "png")
} else cleanup()
  
  
a$SA<-paste('SA',a$Area)
a[a$Area>3,'SA']<-'SA 4-7'
tab1<-tapply(a$ton,list(a$SA,a$year),sum)/1000
newplot(dev=My.device,filename='total_catches',nox=1,noy=1,Portrait=F);
par(mar=c(5,4,4,5)+.1)

barplot(tab1,legend=rownames(tab1),args.legend = list(x = "topright"), ylab='Catches (1000 tonnes)')


if (My.device=='screen'){
  savePlot(filename = file.path(mdDir,'total_catch'),type = "png")
} else cleanup()


