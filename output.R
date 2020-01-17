## Extract results of interest, write TAF output tables

## Before: natmort.csv (data), details.out, sms.rep, summary_table.out (model)
## After:  fatage.csv, fatage_annual.csv natage.csv, summary.csv, transcript.txt 
##         forecast_input.csv forecast_basis.csv, forecast_output.csv (output)



#Arith log is either "Arithmetric" or "Log values". This is used for model output, uncertainities plots and table.
Arith_log <- c("Arithmetric","Log values")[2]  #select assumed distribution of variables
include.last.assessment.year.recruit<-T       # should be T when the dregde survey data are availeble



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

## Forecast input, basis and output
source("Forecast.R")

## SSB, Rec and F with uncertaintity

# Note these values do not come from summary out, instead from SMS.std. Therefore values are slightly different
###Arith log is either "Arithmetric" or "Log values". This is originally set at the beginning of output.
#print(Arith_log)

tmp<-Read.SMS.std_TAF()
tmp$Species<-"Area-1r"


confidence<- 0.90  # 90% confidence limits

two<-qt(1-(1-confidence)/2, df = 10000)  # plus minus factor to get confidence interval


if (Arith_log == "Arithmetric") {
  tmp$name[tmp$name=="next_SSB"]<-'hist_SSB'
  a<-tmp[tmp$name %in% c("hist_SSB","avg_sumF","rec_sd"),]
} else if (Arith_log == "Log values"){
  tmp$name[tmp$name=="next_log_SSB"]<-'hist_log_SSB'
  a<-tmp[tmp$name %in% c("hist_log_SSB","avg_log_sumF","log_recsd"),]
}

if (Arith_log == "Arithmetric") a<-data.frame(Species="Area-1r",Year=a$year, variable=a$name,value=a$value,std=a$std,CV=a$std/a$value*100)
if (Arith_log == "Log values")  a<-data.frame(Species="Area-1r",Year=a$year, variable=a$name,value=a$value,std=a$std,CV=a$std)
if (include.last.assessment.year.recruit==F) {
  a<-a[a$Year != read.sms.dat_TAF("last.year.model") | a$variable != "rec_sd",]
} 

if (Arith_log == "Arithmetric") {
  a$upper<-a$value+two*a$std
  a$lower<-a$value-two*a$std
}else if (Arith_log == "Log values") {
  a$upper<-exp(a$value+two*a$std)
  a$lower<-exp(a$value-two*a$std)
  a$value<-exp(a$value)
}

a$titl<- ifelse (a$variable %in% c('hist_SSB','hist_log_SSB'),"SSB", ifelse(a$variable %in% c('avg_sumF','avg_log_sumF'),"Average F",ifelse(a$variable %in% c('rec_sd','log_recsd'), "Recruitment",'error')))
years<-sort(unique(a$Year))
b<-matrix(NA,ncol=9,nrow=length(years))
rownames(b)<-as.character(years)
colnames(b)<-c('Recruitment','High','Low','SSB','High','Low','F ages 1-2','High','Low')
aa<-aggregate(cbind(value,upper,lower)~Year+titl,data=a,sum)
a2 <- as.matrix(aa[aa$titl=="Recruitment",c("value","upper","lower")],ncol=3)
b[1:dim(a2)[[1]],1:3]<- a2
a2 <- as.matrix(aa[aa$titl=="SSB",c("value","upper","lower")],ncol=3)
b[1:dim(a2)[[1]],4:6]<- a2
a2 <- as.matrix(aa[aa$titl=="Average F",c("value","upper","lower")],ncol=3)
b[1:dim(a2)[[1]],7:9]<- a2

b <- xtab2taf(b,"Year")
write.taf(natmort, dir="output", quote=TRUE)  # commas in colnames

if (Arith_log == "Arithmetric") {
  suppressWarnings(write.taf(b,file = "arithmetric_values.csv",dir="ouput"))
} else { suppressWarnings(write.taf(b,file = "logarithmic_values.csv",dir = "output"))}


