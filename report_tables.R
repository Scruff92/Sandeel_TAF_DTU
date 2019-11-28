## Prepare tables for report

## Before: catage.csv, effort.csv, maturity.csv, natmort.csv, survey.csv,
##         wcatch.csv (data), fatage.csv, natage.csv, summary.csv (output)
## After:  catage.csv, effort.csv, fatage.csv, maturity.csv, natage.csv,
##         natmort.csv, summary.csv, survey.csv, wcatch.csv (report)

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
#ALSO: A lot of logic i dont get in this table compared to haf step
fatage_annual <- read.taf("output/fatage.csv")
fatage_annual <- aggregate(cbind(`0`,`1`,`2`,`3`,`4+`)~Year,data = fatage_annual,sum)
fatage_annual<-fatage_annual[,-2]
fatage_annual["Age 4+"]<-NA #what to do with this?
colnames(fatage_annual)<-c("Year", "Age 0","Age 1","Age 2","Age 3","Age 4+")
fatage_annual["Avg. 1-2"]<-fatage_annual$`Age 0`
fatage_annual <- rbind(fatage_annual, colMeans(fatage_annual))
fatage_annual[nrow(fatage_annual),1] <- "arith. mean"
fatage_annual <- rnd(fatage_annual, -1, 3)


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

########################
# Input to forecast
#!Note also that to produce the Blim-option I just have to scroll down til a place where it says ###  ### !Her  ### and change setting for Bmsy

#defining if we use long term og 10 year average rec: 
AA <- 34 #area 1r #this means using a long-term geom

RANGE = 100 #top end of the Fmult range, which the optimize function should explore when calculating Fmsy

TAC.year<-2019

#use this for area 1r
scale.options<-c(0,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7)*1

roundTAC<-3   # number of decimals in TAC 

recruimentMultipliers <-seq(0,1.0,0.2)   

save.on.file<-T   #  output on file or on screen only

Recruit.in.Assess.year<- 0     #  >0 = Overwrite estimate from assessment with the given input value. Value must be given as recruitment (i.e 0-group in the assessment year) 
#   0 = Use value estimated from assessment (default)
#  -1 = make forcast from based on values of 1-group in the TAC year (for Real time monitoring purposes) 

#  user options according to stock annex, Please keep unchanged. 
PM.TAC.year<-TAC.year 
#PM.after.TAC.year<-seq(1983,2000,1)                       # Year or year Range for Proportion Mature for the TAC year
PM.after.TAC.year<-seq(1983,TAC.year-1,1)         # Year or year Range for Proportion Mature for the year after TAC year  (default long term average)
west.TAC.year<-seq(TAC.year-5,TAC.year-1,1) # Year or year Range for weight in the Sea  for the TAC year  (default the 3 most recent years ~ seq(TAC.year-3,TAC.year-1,1))
weca.TAC.year<-seq(TAC.year-5,TAC.year-1,1) # Year or year Range for weight in the Catch  for the TAC year (default the 3 most recent years ~seq(TAC.year-3,TAC.year-1,1))
NatMor.year<-TAC.year-1                      # Year or year Range for Natural mortality (M) (default the most recent year)

# read assessment summary file
s<-Read.summary.data_TAF()

#recruitment geometric mean
tmp<-subset(s,Year < (TAC.year-1) & Quarter==read.sms.dat_TAF("rec.season") & Age==read.sms.dat_TAF("first.age"),select=c(Year,N))

firstREC = length(tmp$N)- AA 

#first = 1
lastREC = length(tmp$N)-0
#last=35

recruit.TAC.year<-exp(mean(log(tmp$N[firstREC:lastREC])))

#rec.Y.min<-min(tmp$Year); if (rec.Y.min>=2000) rec.Y.min<-rec.Y.min-2000 else rec.Y.min<-rec.Y.min-1900  
#rec.Y.max<-max(tmp$Year);if (rec.Y.max>=2000) rec.Y.max<-rec.Y.max-2000 else rec.Y.max<-rec.Y.max-1900 
rec.Y.min <- tmp$Year[firstREC]
rec.Y.max <- tmp$Year[lastREC]
rec.string<-paste(formatC(rec.Y.min,format='d',flag='0',width=2),'-',formatC(rec.Y.max,format='d',flag='0',width=2),sep='')

Yield.assess<-sum(subset(s,Year %in% (TAC.year-1),select=c(Yield)))/1000

tmp<-subset(s,Year %in% PM.TAC.year & Quarter==1,select=c(Year,Age,propmat))
PM.TAC<-c(0,tapply(tmp$propmat,list(Age=tmp$Age),mean))

tmp<-subset(s,Year %in% PM.after.TAC.year & Quarter==1,select=c(Year,Age,propmat))
PM.after.TAC<-c(0,tapply(tmp$propmat,list(Age=tmp$Age),mean))

tmp<-subset(s,Year %in% west.TAC.year,select=c(Year,Age,Quarter,west))
west.TAC<-tapply(tmp$west,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% weca.TAC.year,select=c(Year,Age,Quarter,weca))
weca.TAC<-tapply(tmp$weca,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% NatMor.year,select=c(Year,Age,Quarter,M))
M<-tapply(tmp$M,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% (TAC.year-1),select=c(Year,Age,Quarter,F))
FF<-tapply(tmp$F,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% TAC.year & Quarter==1,select=c(Year,Age,N))
N.TAC<-tapply(tmp$N,list(Age=tmp$Age),sum)

N.TAC<-c(recruit.TAC.year,N.TAC)
if (Recruit.in.Assess.year>0) N.TAC[2,1]<-Recruit.in.Assess.year*(exp-(M[1,2]))
No.recruits.in.assessment.year<-subset(s,Year %in% (TAC.year-1) & Quarter==2 & Age==0,select=c(N))$N

outTab<-cbind(N.TAC/1000,FF,west.TAC[,1]*1000,weca.TAC*1000,PM.TAC,PM.after.TAC,M) 
dimnames(outTab)[1]<-list(paste("Age",seq(0,4,1)))
dimnames(outTab)[2]<-list(c(paste('Stock numbers(',TAC.year,')',sep=''),'Exploitation pattern 1st half', 'Exploitation pattern 2nd half','Weight in the stock 1st half','Weight in the catch 1st half','weight in the catch 2nd half',
                            paste('Proportion mature(',TAC.year,')',sep=''),paste('Proportion mature(',TAC.year+1,')',sep=''),'Natural mortality 1st half','Natural mortality 2nd half'))



write.csv(t(outTab),file=file.path("./output",'forecast_input.csv'))

##############################################################