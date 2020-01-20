## Run analysis, write model results

## Before: data, software (bootstrap)
## After:  details.out, sms.rep, summary.table.out (model)

library(icesTAF)
library(FLCore)
source("utilities_sms.R")

mkdir("model")

## Get config
cp("bootstrap/data/config/*", "model")

## Get data
cp("bootstrap/data/*.in", "model")

## Get software
exefile <- if(os.linux()) "sms" else "sms.exe"
cp(file.path("bootstrap/software/sms", exefile), "model")

## Run model
setwd("model")
system("./sms")
setwd("..")

## Run retrospective analysis
source("Retro_working.R")
