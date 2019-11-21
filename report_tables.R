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
