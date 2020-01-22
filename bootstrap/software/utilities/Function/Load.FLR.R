
library(FLCore,quietly=TRUE)
#library(FLAssess)
 #  library(FLEDA)
if (allLibraries) { 
  #ibrary(FLXSA)
  #library(FLBRP)
  #library(FLHCR)
  #library(FLEDA)

 }

do.test<-FALSE


#library(FLSMS)

#overwrite FLSMS library
source(file.path(my.FLR.path,"FLSMS.control.R"))
source(file.path(my.FLR.path,"FLSMS.predict.control.R"))

source(file.path(my.FLR.path,"FLStockMulti.R"))
source(file.path(my.FLR.path,"FLIndex.SMS.R"))
source(file.path(my.FLR.path,"XSA2SMS.R"))
source(file.path(my.FLR.path,"SMS2FLSMSs.R"))

source(file.path(my.FLR.path,"FLSMS.R"))
source(file.path(my.FLR.path,"FLSMSs.R"))

source(file.path(my.FLR.path,"FLOP.control.R"))
source(file.path(my.FLR.path,"FLOP_MSFD.control.R"))
#source(file.path(my.FLR.path,"assess4.R"))


