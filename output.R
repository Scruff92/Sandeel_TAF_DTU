## Extract results of interest, write TAF output tables

## Before: natmort.csv (data), details.out, sms.rep, summary_table.out (model)
## After:  fatage.csv, fatage_annual.csv natage.csv, summary.csv, transcript.txt 
##         forecast_input.csv forecast_basis.csv, forecast_output.csv (output)

library(icesTAF)
source("utilities_sms.R")

mkdir("output")

## Fishing mortalities
fatage <- read.output("model/details.out", "Fishing mortality")
fatage <- plus(fatage)
## Fbar is the total F in a year (time steps 1+2), averaging ages 1 and 2
fbar <- aggregate(cbind(`1`,`2`)~Year, fatage, sum)
fbar <- data.frame(Year=fbar$Year, Fbar=rowMeans(fbar[c("1","2")]))


##Fatage_annual

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
fatage_annual <- cbind(data.frame(Year=row.names(tab1)),tab1)

## Numbers at age
natage <- read.output("model/details.out", "Stock numbers")
natage <- div(natage, as.character(0:4))
natage <- plus(natage)
## Calculate N for current year
natmort <- read.taf("data/natmort.csv")
n <- natage[nrow(natage), c("0","1","2","3","4+")]
f <- fatage[nrow(fatage), c("0","1","2","3","4+")]
m <- natmort[nrow(natmort), c("0","1","2","3","4+")]
cur <- n * exp(-f-m)
cur$"4+" <- cur$"3" + cur$"4+"
cur[c("1","2","3")] <- cur[c("0","1","2")]
cur$"0" <- 0
natage <- rbind(natage, cbind(Year=max(natage$Year)+1, Step=1, cur))

## Summary
widths <- c(6, 15, 15, 15, 15, 15)
summary <- read.fwf("model/summary_table.out", widths=widths, skip=4)
names(summary) <- c("Year", "Rec", "SSB", "TSB", "Catch", "Fbar")
summary <- summary[c("Year", "Rec", "TSB", "SSB", "Catch", "Fbar")]
summary$Year[nrow(summary)] <- summary$Year[nrow(summary)-1] + 1  # current year
summary$Fbar <- c(fbar$Fbar, NA)  # precise F values

## Transcript
transcript <- readLines("model/sms.rep")
transcript <- trimws(transcript)

## Export
setwd("output")
write.taf(fatage)
write.taf(fatage_annual)
write.taf(natage)
write.taf(summary)
writeLines(transcript, "transcript.txt")
setwd("..")

#Forecast input, basis and output
source("Forecast.R")

