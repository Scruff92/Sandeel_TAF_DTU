## Prepare tables for report

## Before: catage.csv, effort.csv, maturity.csv, natmort.csv, survey.csv,
##         wcatch.csv (data), fatage.csv, natage.csv, summary.csv (output)
## After:  catage.csv, effort.csv, fatage.csv, maturity.csv, natage.csv,
##         natmort.csv, summary.csv, survey.csv, wcatch.csv  forecast_input.csv (report)
##        

library(icesTAF)
source("utilities_sms.R")

mkdir("report")

## 1  Data

## catage (xtab, trim col, col mean, round)
catage <- read.taf("data/catage.csv")
catage <- sandeel.table(catage, digits=0)
write.taf(catage, dir="report", quote=TRUE)  # commas in colnames

## effort (col mean, round)
effort <- read.taf("data/effort.csv")[c("Year","Total")]
names(effort) <- c("Year", "Area 1r")
effort <- rbind(effort, colMeans(effort))
effort[nrow(effort),1] <- "arith. mean"
effort <- rnd(effort, "Area 1r")
write.taf(effort, dir="report")

## maturity (col mean, trim col
maturity <- read.taf("data/maturity.csv")
yrange <- range(maturity$Year)
yrange[2] <- yrange[2] - 1
maturity <- as.data.frame(t(colMeans(maturity)))
maturity <- maturity[c("Year", "1", "2", "3", "4+")]
maturity$Year <- paste(yrange, collapse="-")
names(maturity) <- c("Year", paste("Age", 1:4))
maturity <- rnd(maturity, -1, 2)
write.taf(maturity, dir="report")

## natmort (trim row, xtab, trim col, col mean, round)
natmort <- read.taf("data/natmort.csv")
natmort <- head(natmort, -4)  # remove last rows
natmort <- sandeel.table(natmort, digits=3)
write.taf(natmort, dir="report", quote=TRUE)  # commas in colnames

## survey
survey <- read.taf("data/survey.csv")
names(survey) <- c("Year", "Age 0", "Age 1")
write.taf(survey, dir="report")

## wcatch (xtab, trim col, col mean, round)
wcatch <- read.taf("data/wcatch.csv")
wcatch <- sandeel.table(wcatch, digits=1)
write.taf(wcatch, dir="report", quote=TRUE)  # commas in colnames

## 2  Output

## fatage (xtab, trim col, col mean, round)
fatage <- read.taf("output/fatage.csv")
fatage <- sandeel.table(fatage, digits=3)
write.taf(fatage, dir="report", quote=TRUE)  # commas in colnames

## fatage_annual (xtab, trim col, col mean, round, aggregate to year, Fbar(1-2))
#Working progress 
fatage_annual <- read.taf("output/fatage.csv")
fatage_annual <- aggregate(cbind(`0`,`1`,`2`,`3`,`4+`)~Year,data = fatage_annual,sum)

colnames(fatage_annual)<-c("Year", "Age 0","Age 1","Age 2","Age 3","Age 4+")

fatage_annual["Avg. 1-2"] <- rowMeans(fatage_annual[c("Age 1","Age 2")])

fatage_annual <- rbind(fatage_annual, colMeans(fatage_annual))
fatage_annual[nrow(fatage_annual),1] <- "arith. mean"
fatage_annual <- rnd(fatage_annual, -1, 3)

write.taf(fatage_annual, dir="report", quote=TRUE)  # commas in colnames
###################
#fatage 2 (different method)
a<-Read.summary.data_TAF()

a$deadM<-a$N.bar*a$M
a$deadC<-a$N.bar*a$F
a$deadAll<-a$N.bar*a$Z

a<-subset(a,select=c(Year,Age,deadM,deadC,deadAll,Z))
a<-aggregate(list(deadM=a$deadM,deadC=a$deadC,deadAll=a$deadAll,Z=a$Z),list(Year=a$Year,Age=a$Age),sum)
a$FF <- a$Z * a$deadC /a$deadAll
a$M<- a$Z*a$deadM/a$deadAll

tab1<-tapply(a$FF,list(a$Year,a$Age),sum)
Fbar_Ages=1:2
tmp<-tab1[,as.character(Fbar_Ages)]
tab1<-cbind(tab1,rowMeans(tmp))

colnames(tab1)<-c(paste("Age",seq(0,4)),paste("Avg. ",Fbar_Ages[1],'-',Fbar_Ages[2],sep=''))
rnames<-rownames(tab1)
tab1<-rbind(tab1,colMeans(tab1))
rownames(tab1)<-c(rnames,'arith. mean')
tab1
write.taf(tab1,dir="report", quote=TRUE,file = "fatage_annual_2")

######################
## natage (realign step, add current, round)
natage <- read.taf("output/natage.csv")
natage$"0" <- c(natage$"0"[-1], NA)
natage <- natage[natage$Step==1,]
natage$Step <- NULL
names(natage)[-1] <- paste("Age", names(natage)[-1])
natage <- round(natage)
write.taf(natage, dir="report")

## summary (col mean, round)
summary <- read.taf("output/summary.csv")
geom <- exp(mean(log(summary$Rec), na.rm=TRUE))
summary <- rbind(summary, colMeans(summary, na.rm=TRUE))
summary[nrow(summary),1] <- "arith. mean"  # avoid string->factor with I():
summary[nrow(summary)+1,] <- data.frame(I("geo. mean"), geom, NA, NA, NA, NA)
summary <- rnd(summary, c("Rec","TSB","SSB","Catch"))
summary <- rnd(summary, "Fbar", 3)
names(summary) <- c("Year", "Recruits (thousands)", "TSB (tonnes)",
                    "SSB (tonnes)", "Yield (tonnes)", "Mean F1-2")
write.taf(summary, dir="report")

## transcript (trim)
transcript <- readLines("output/transcript.txt")
beg <- grep("Average F:", transcript)
end <- grep("Recruit-SSB", transcript) - 1
transcript <- transcript[-(beg:end)]
writeLines(transcript, "report/transcript.txt")


###################################################
## forecast input

o <- read.taf("output/forecast_input.csv")
oo<-cbind(o[,1],round(o[,-1],3))
colnames(oo)[1]<-"Variable"

write.taf(oo,dir="report",file="forecast_input.csv")

