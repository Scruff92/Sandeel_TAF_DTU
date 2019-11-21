## Prepare plots for report

## Before: catage.csv, wcatch.csv (data)
## After:  catage.png, wcatch.png (report)

library(icesTAF)
library(lattice)
source("utilities_sms.R")

mkdir("report")

## 1  Data

taf.png("catage")
catage <- read.taf("data/catage.csv")
catage <- aggregate(cbind(`0`,`1`,`2`,`3`,`4+`)~Year, catage, sum)
catage <- prop.table(as.matrix(taf2xtab(catage)), 1)
barplot(t(rev(as.data.frame(catage))), ylim=0:1,
        xlab="Year", ylab="Proportion at age",
        col=c("violet","deepskyblue","seagreen","khaki4","lightcoral"))
box()
dev.off()

taf.png("wcatch")
wcatch <- read.taf("data/wcatch.csv")
wcatch <- step2long(wcatch)
wcatch$Label <- factor(wcatch$Step,
                       labels=c("First half year", "Second half year"))
zoom(xyplot(Value~Year|Label, groups=Age, data=wcatch, layout=c(1,2),
            as.table=TRUE, ylim=c(0,NA), type="l", col=1:5, lwd=2,
            ylab="Mean weight (g)", subset=!(Step==1 & Age==0)))
dev.off()
