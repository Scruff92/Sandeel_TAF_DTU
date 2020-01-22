library(ggplot2)
area.no=gsub("SAN-area-", "", my.stock.dir)
dat0=subset(read.csv(file.path(root, "Data","survey_index.csv")), area==area.no )

#Dredge survey index
dat00=droplevels(subset(dat0, type=="dredge"& hy==2))[,c("area", "year","age0", "age1")]
dat000=reshape::melt(dat00, id.vars=c('area', 'year'))
dat1=transform(dat000,
	age=factor(variable, labels=c("0", "1"))
	)
dat=plyr::ddply(dat1, ~area+age, "mutate", scaled=scale(value))

miny =min(dat["year"])
maxy =max(dat["year"])
if(maxy-miny<10) by=2 else by=4

ggplot(dat, aes(x=year, y=scaled, color=age))+geom_line()+theme_bw()+ylab("survey index (scaled)")+
scale_x_continuous(breaks=seq(miny, maxy, by=by))

if (My.device=='screen') ggsave(file.path(mdDir,"dredge_survey_index.png"), height=2, width=4)


if (markD){ 
  names(dat00)=c("Area", "Year", "Age 0", "Age 1")
  md<-(gsub(' NA','   ' ,knitr::kable(dat00[,-1],digits=3, row.names =FALSE)))
  writeLines(md,con=file.path(mdDir,'_tab_SAN_survey_index.md'))
} 
########################################
#Acoustic survey index
if(length(unique(dat0$type))==2) #is it there?
{
dat00=droplevels(subset(dat0, type=="acoustic" & area==area.no & hy==1))[,c("area", "year","age1", "age2","age3", "age4")]
dat000=reshape::melt(dat00, id.vars=c('area', 'year'))
dat1=transform(dat000,
	age=factor(variable, labels=c("1", "2", "3", "4"))
	)
dat=plyr::ddply(dat1, ~area+age, "mutate", scaled=scale(value))
ggplot(dat, aes(x=year, y=scaled, color=age))+
geom_line()+theme_bw()+ylab("survey index (scaled)")+
scale_x_continuous(breaks=seq(2010, 20016, by=2))
if (My.device=='screen') ggsave(file.path(mdDir,"acoustic_survey_index.png"), height=2, width=4)
}
