## Preprocess data, write TAF data tables

## Before: canum.in, effort.in, fleet_catch.in, natmor.in, propmat.in,
##         weca.in (bootstrap/data)
## After:  catage.csv, effort.csv, maturity.csv, natmort.csv, survey.csv,
##         wcatch.csv (data)

library(icesTAF)

mkdir("data")

## Catch at age
catage <- read.table("bootstrap/data/canum.in", comment.char="", fill=TRUE)
catage <- na.omit(catage)[c(7,1:5)]
names(catage) <- c("Year", "0", "1", "2", "3", "4+")
catage$"0" <- as.numeric(as.character(catage$"0"))  # as.character for fac->num
catage[-1] <- catage[-1] / 1000
catage$Step <- 1:2
catage <- catage[c("Year","Step","0","1","2","3","4+")]

## Effort
txt <- suppressWarnings(readLines("bootstrap/data/effort.in"))
txt <- sub("#", "X", txt)
n <- match("", txt) - 1
txt.1 <- txt[1:n]
txt.2 <- tail(txt, -n)
txt.2 <- grep("[0-9]", txt.2, value=TRUE)
effort.1 <- read.table(text=txt.1)[c(3,1)]
effort.2 <- read.table(text=txt.2)[c(3,1)]
names(effort.1) <- c("Year", "Effort")
names(effort.2) <- c("Year", "Effort")
effort <- data.frame(Year=effort.1$Year,
                     Step1=effort.1$Effort,
                     Step2=effort.2$Effort,
                     Total=effort.1$Effort+effort.2$Effort)

## Survey
txt <- readLines("bootstrap/data/fleet_catch.in")
dredge <- grep("# Dredge survey", txt)
num <- grep("[0-9]", txt)
skip <- min(num[num > dredge]) - 1
survey <- read.table("bootstrap/data/fleet_catch.in",
                     skip=skip, comment.char="")
names(survey) <- c("-", "0", "1", "#", "Year")
survey <- survey[c("Year","0","1")]

## Natural mortality
natmort <- read.table("bootstrap/data/natmor.in", comment.char="", header=TRUE)
names(natmort) <- c("0", "1", "2", "3", "4+", "#", "Year", "Step")
natmort$Step <- as.integer(substring(natmort$Step, 1, 1))
natmort <- natmort[c("Year","Step","0","1","2","3","4+")]

## Maturity
maturity <- read.table("bootstrap/data/propmat.in", comment.char="",
                       header=TRUE)
names(maturity) <- c("0", "1", "2", "3", "4+", "#", "Year", "Step")
maturity <- maturity[c("Year","Step","0","1","2","3","4+")]

## Catch weights = stock weights
n <- match("", readLines("bootstrap/data/weca.in")) - 1
wcatch <- read.table("bootstrap/data/weca.in", nrows=n)
names(wcatch) <- c("0", "1", "2", "3", "4+")
wcatch <- wcatch * 1000
wcatch$Year <- rep(seq(1983, length=n/2), each=2)
wcatch$Step <- 1:2
wcatch <- wcatch[c("Year","Step","0","1","2","3","4+")]

## Export
setwd("data")
write.taf(effort)    # 1.5
write.taf(catage)    # 2.1
write.taf(wcatch)    # 2.2
write.taf(maturity)  # 2.3
write.taf(survey)    # 2.4
write.taf(natmort)   # 2.8
setwd("..")
