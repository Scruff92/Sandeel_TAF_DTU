## Run analysis, write model results

## Before: data, software (bootstrap)
## After:  details.out, sms.rep, summary.table.out (model)

source("utilities_sms.R")

# Make Directory
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
source("retro.R")
