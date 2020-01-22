#Remove all objects
#rm(list = ls())

# Harddisk drive for SMS, runs
dosDrive<-"M:"  
#dosDrive<-"/Volumes/Faelles"

# root directory for the SMS package, runs
root<-file.path(dosDrive,"Tobis","Tobis_assessment","SMS_2019")

# root directory for the SMS package, R-programs
root.prog<-file.path(dosDrive,"Tobis","Tobis_assessment","SMS_2019")


###########################
# Specify directory
###########################


#my.stock.dir<-'SAN-area-1r'
#my.stock.dir<-'SAN-area-2r'
#my.stock.dir<-'SAN-area-3r'
#my.stock.dir<-'SAN-area-4'

#my.stock.dir<-'SAN-area-1r_explor_old_M'

#my.stock.dir<-'SAN-area-3r_explor_new_M'


#my.stock.dir<-'SAN-area-4_MvD'

#my.stock.dir<-'SAN-area-2r_MvD'
#my.stock.dir<-'SAN-area-1r_MvD'
#my.stock.dir<-'SAN-area-2r_MvD2'
my.stock.dir<-'SAN-area-1r'
#my.stock.dir<-'SAN-area-2r'
#my.stock.dir<-'SAN-area-3r'
#my.stock.dir<-'SAN-area-4'
#################################################

#################################################





markD<-T      # create MarkDown dokument for automatic production of word document with table and figures 


###################### do not change the code below this line ###############################################################################


#Installation of FLR, if needed
#  install.packages("FLCore", repos="http://R-Forge.R-project.org")
# install.packages("FLEDA", repos="http://R-Forge.R-project.org")


SMSdir<-function(stock=my.stock.dir) {
  # Path to data directory
  data.path<<-file.path(root,my.stock.dir)
  
  # Path to R-programme directory
  prog.path<<-file.path(root.prog,"R_prog")
  my.FLR.path<<-file.path(root.prog,"R_prog","FLSMS")
  prog.path.SAN<<-file.path(prog.path,'Sandeel')  # do not change
  mdDir<<-file.path(data.path,'mddir')
}

SMSdir(stock=my.stock.dir) 

options(stringsAsFactors = FALSE)
# libraries
# Use all libraries or just a simple configuartion (few libraries and limited access to FLR) or full access to 
allLibraries<-FALSE

library(lattice,quietly=TRUE)
library(MASS,quietly=TRUE)
library(reshape,quietly=TRUE)

#library(gtools,quietly=TRUE)  # for def of macro

if (allLibraries) library(quantreg,quietly=TRUE)

setwd(data.path)

# Path to R-programme directory
prog.path.func<-file.path(root.prog,"R_prog","Function")

source(file.path(prog.path.func,"Init_R_functions.R"))
if (!markD) cat("active stock directory:",getwd(),"\n");


