 cleanup()
library(ggplot2); theme_set(theme_bw())
d = read.csv(file.path(root,'Data','survey_index.csv')) 
area.no = gsub("SAN-area-", "", my.stock.dir)
d00 = droplevels(subset(d,area==area.no))[,-1]
d0=na.omit(reshape2::melt(d00, id.vars=c("year", "hy", "type")))
names(d0)[4:5]=c("age", "CPUE")
d0$age=as.numeric(gsub("age", "", d0$age))
d1=na.omit(plyr::ddply(d0, ~type, "mutate", CPUEnext=CPUE[pmatch(paste(year+1, age+1), paste(year, age))]))
d1$age=factor(d1$age)

#Calculate R2 values
s=plyr::ddply(d1, ~age+type, "summarize", 
	r2=summary(lm(log(CPUEnext)~log(CPUE)))$r.squared,
	minx=min(log(CPUE), na.rm=TRUE),
	maxy=max(log(CPUEnext), na.rm=TRUE)
)
ddat=subset(d1, type=="dredge")
adat=subset(d1, type=="acoustic")
#now plot results
#Dredge survey first
p1=ggplot(ddat, aes(x=log(CPUE), y=log(CPUEnext)))+
	geom_point()+
	geom_smooth(method="lm", se=FALSE, colour="black")+
	xlab("log(CPUE age 0)")+ylab("log(CPUE age 1)")+
geom_point(data=subset(ddat, year==max(ddat$year)), colour="red")

p1+geom_text(data=subset(s, type=="dredge"), position="dodge", check_overlap="true", 
	aes(x=minx+1.5, y=maxy*.99, label=paste0("Rsquared = ", round(r2,2)))) 

if(My.device=="screen")	ggsave(filename= file.path(mdDir,"internal_consistency_dredge.png"), width=4, height=3)

#now acoustic
if(nrow(adat)>0){

	adat$age=factor(adat$age, labels=c("Age 1", "Age 2", "Age 3"))#hack lables
	s$age=factor(s$age, labels=c("Age0","Age 1", "Age 2", "Age 3"))#hack lables
	
	p2=ggplot(adat, aes(x=log(CPUE), y=log(CPUEnext)))+
		facet_wrap(~age, scale="free")+
		geom_point()+
		geom_smooth(method="lm", se=FALSE, colour="black")+
		xlab("log(CPUE)")+ylab("log(CPUE next)")+
		geom_point(data=subset(adat, year==max(adat$year)), colour="red")

	p2+geom_text(data=subset(s, type=="acoustic"), 
		aes(x=minx+1.5, y=maxy*.99, label=paste0("Rsquared = ", round(r2,2))))
	if(My.device=="screen")	ggsave(filename= file.path(mdDir,"internal_consistency_acoustic.png"), width=6, height=3)
}

#Old version below
#removed because itmade mistakes if years were missing 
# d <- read.csv(file.path(root,'Data','survey_index.csv')) 
# area.no<-gsub("SAN-area-", "", my.stock.dir)
# d<-droplevels(subset(d,area==area.no))

# #First plot dredge survey data	
# ## age0 vs age 1 only

# d1<-droplevels(subset(d,type=="dredge"))
# ly <- (dim(d1)[1])

# a0<-d1[1:ly-1,"age0"]
# a1<-d1[2:ly,"age1"]
# newplot(dev=My.device,filename='internal_consistency_dredge',nox=1,noy=1,Portrait=F);
# par(mar = c(4, 4, 1, 1))
# plot(log(a0),log(a1), pch = 21, cex = 2, bg = "black", 
# xlab = "ln(CPUE age 0)", ylab = "ln(CPUE age 1)",cex.lab = 1.1, cex.axis = 0.9)
# points(log(a0[ly-1]),log(a1[ly-1]),pch = 21, cex = 2, bg = "red")

# fit <- lm(log(a1)~log(a0))
# abline(fit$coefficients[1],fit$coefficients[2])

# r2 <- round(summary(fit)$r.squared,2)

# text(min(log(a0), na.rm=TRUE)*1.1,max(log(a1),na.rm=TRUE)*0.9,paste("R-squared =",r2),pos=4)

# if (My.device =='screen'){
	# savePlot(filename= file.path(mdDir,"internal_consistency_dredge"),type="png")
# } else cleanup()

# #Now plot acoustic survey data if it exists
# if(length(unique(d$type))==2)
# {
	# d1<-droplevels(subset(d,type=="acoustic"))
	# ly <- (dim(d1)[1])
	# newplot(dev=My.device,filename='internal_consistency_dredge',nox=1,noy=1,Portrait=F);
	# par(mfcol=c(1,3))
	# #age1 vs age2
	# a0<-d1[1:ly-1,"age1"]
	# a1<-d1[2:ly,"age2"]

	# par(mar = c(4, 4, 1, 1))
	# plot(log(a0),log(a1), pch = 21, cex = 2, bg = "black", 
	# xlab = "ln(CPUE age 1)", ylab = "ln(CPUE age 2)",cex.lab = 1.1, cex.axis = 0.9)
	# points(log(a0[ly-1]),log(a1[ly-1]),pch = 21, cex = 2, bg = "red")

	# fit <- lm(log(a1)~log(a0))
	# abline(fit$coefficients[1],fit$coefficients[2])

	# r2 <- round(summary(fit)$r.squared,2)

	# text(min(log(a0), na.rm=TRUE)*1.05,max(log(a1),na.rm=TRUE)*.99,paste("R-squared =",r2), pos=4)

	# #age2 vs age3, using same object names
	# a0<-d1[1:ly-1,"age2"]
	# a1<-d1[2:ly,"age3"]

	# par(mar = c(4, 4, 1, 1))
	# plot(log(a0),log(a1), pch = 21, cex = 2, bg = "black", 
	# xlab = "ln(CPUE age 2)", ylab = "ln(CPUE age 3)",cex.lab = 1.1, cex.axis = 0.9)
	# points(log(a0[ly-1]),log(a1[ly-1]),pch = 21, cex = 2, bg = "red")

	# fit <- lm(log(a1)~log(a0))
	# abline(fit$coefficients[1],fit$coefficients[2])

	# r2 <- round(summary(fit)$r.squared,2)

	# text(min(log(a0), na.rm=TRUE)*1.05,max(log(a1),na.rm=TRUE)*.99,paste("R-squared =",r2), pos=4)
	
	# #age3 vs age4, using same object names
	# a0<-d1[1:ly-1,"age3"]
	# a1<-d1[2:ly,"age4"]

	# par(mar = c(4, 4, 1, 1))
	# plot(log(a0),log(a1), pch = 21, cex = 2, bg = "black", 
	# xlab = "ln(CPUE age 3)", ylab = "ln(CPUE age 4)",cex.lab = 1.1, cex.axis = 0.9)
	# points(log(a0[ly-1]),log(a1[ly-1]),pch = 21, cex = 2, bg = "red")

	# fit <- lm(log(a1)~log(a0))
	# abline(fit$coefficients[1],fit$coefficients[2])

	# r2 <- round(summary(fit)$r.squared,2)

	# text(min(log(a0), na.rm=TRUE)*1.05,max(log(a1),na.rm=TRUE)*.99,paste("R-squared =",r2), pos=4)
# if (My.device =='screen'){
	# savePlot(filename= file.path(mdDir,"internal_consistency_acoustic"),type="png")
# } else cleanup()

# }

	


