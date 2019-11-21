
d11<-sub('area-2','area-1',data.path)  # path to sandeel area 1

d1 <- read.table(file.path(d11,'summary_table_raw.out') ,header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
d2 <- read.table(file.path(data.path,"summary_table_raw.out"),header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

ly <- (dim(d1)[1])-1
d1$Rec<-d1$Rec/1000000  # billions
d2$Rec<-d2$Rec/1000000  # billions

fil<-'ag0_area_1_area2_consitency'
#newplot(dev="screen",filename=fil,nox=1,noy=1,Portrait=F,w8=5,w11=5);
par(mar=c(5,4,4,5)+.1)

plot(d1$Rec,d2$Rec, pch = 21, cex = 1, bg = "black",
xlab = "Area 1 recruits (bill)", ylab = "Area 2 recruits (bill)",cex.lab = 1.1, cex.axis = 1.1)
points(d1$Rec[ly],d2$Rec[ly], pch = 21, cex = 1, bg = "red")
fit <- lm(d2$Rec~d1$Rec)
abline(fit$coefficients[1],fit$coefficients[2])
r2 <- round(summary(fit)$r.squared,2)
text(max(d1$Rec,na.rm=T)*0.1,max(d2$Rec,na.rm=T)*0.9,paste("R-squared =",r2),pos=4)

#savePlot(filename = file.path(mdDir,fil),type = "png")



