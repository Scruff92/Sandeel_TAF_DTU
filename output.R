## Extract results of interest, write TAF output tables

## Before: natmort.csv (data), details.out, sms.rep, summary_table.out (model)
## After:  fatage.csv, natage.csv, summary.csv, transcript.txt 
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
write.taf(natage)
write.taf(summary)
writeLines(transcript, "transcript.txt")
setwd("..")

#Forecast input, basis and output
source("Forecast.R")

