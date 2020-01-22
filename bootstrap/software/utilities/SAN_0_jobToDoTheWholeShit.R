#
# First you have to edit the "init.r" file in the R-prog directory
# 1. change the "my.stock.dir" variable to the actual assessment
# 2. and set a few other variables for output etc
# Then submit the "init.r." file


# select device
My.device<-"screen"     # "screen", "wmf" "ps" or "png". Use "screen" for MarkDown
#My.device<-"wmf"
#My.device<-"png" 



#############################
# Run the SMS program
#

# delete a lot of output filet etc., files before running a new SMS 
deleteFiles<-c("mdDir\\*.*","retro\\*.*","mcmc*.*",
               "*.?0?","*.out","*.mcm","*.bin","admodel.*","*.csv","*.std","*.bar","*.mc2","*.cor","*.psv","*.ecm", "*.xls","*.html", "mcout*.all",
                    "*.wmf","*.png","*.lg","*.log","ud.dat","HCR_prob.dat","HCR_yield.dat","HCR_SSB.dat","*.par","*.rep","*.hst","*.eva","*.tmp")

# Cleanup and make a new SMS run
do.a.full.SMS.run(label="run_",                   # label for output
                  cleanup=T,                      # detelete  the files in deleteFiles
                  do.single=T,                    # run SMS in single species mode
                  do.hessian=T,                   # Make the Hessian matrix and estimate uncertanties
                  Screen.show=T,                  # show the output on screen, or save it in file
                  do.run=T,                       # Make the run immidiatly, or just make the batch file for the run
                  deleteFiles=deleteFiles,        # clean up in files before the run is made
                  new.version=F)                  # copy a (new) version of the sms program from the program directory (default=FALSE)
###########################

if (!file.exists(file.path(data.path,'mddir'))) dir.create(file.path(data.path,'mddir'))
  

if (SMS.control@species.names=='Area-1r'  & (my.stock.dir=='SAN-area-1r')) {   # you only have to run this once (e.g. as part of the Area 1 assessment), as it gives common output (of assessment input) for all thre sandeel stocks
  # The script SAN_total_catch_and_effort.r makes a lot of output "*.html" on the stock directory
  # where total effort and landings are tabulated for ALL areas
  source(file.path(prog.path.SAN,"SAN_total_catch_and_effort.r"))
  
  source (file.path(prog.path.SAN,"SAN_international_catch.r"))
  
  # Sampling activety output in  
  source(file.path(prog.path.SAN,"SAN_sampling_intensity.r"))
}  


# Plot of survey internal consitency
if (SMS.control@species.names!='Area-2' | SMS.control@species.names=='Area-2') 
source(file.path(prog.path.SAN,"SAN_Internal_consistency.r"))

# plot ag1 0 concistency Araea 2 and Area 1
if (SMS.control@species.names=='Area-2' & (my.stock.dir!='SAN-area-2-2016-area2_index')) 
source(file.path(prog.path.SAN,"SAN_Age0_consitecy_area1_and_2.r"))

# Plot of Catch Residuals
source(file.path(prog.path.SAN,"SAN_Plot_catch_residuals_bubble.R"))

# Plot of survey Residuals
source(file.path(prog.path.SAN,"SAN_Plot_survey_residuals_bubbles.R"))

# Plot of retrospective analysis, Remember to set the year range in the script
source(file.path(prog.path.SAN,"SAN_Retrospectiv_single_sp.R"))

# Plot of total effort og F  and produce output file _tab_SAN_effort.html
source(file.path(prog.path.SAN,"san_effort.R"))

# Produce tables with input and output, all html files
source(file.path(prog.path.SAN,"SAN_Tables_report.r"))


# Plot of stock recruitment relation 
source(file.path(prog.path.SAN,"SAN_Plot_SSB_rec.R"))

# Plot of stock recruitment relation 
source(file.path(prog.path.SAN,"SAN_Plot_summary_ICES_multi.R"))

# Uncertanties, two plots
# remember to set the variable include.last.assessment.year.recruit in the top of the script
source(file.path(prog.path.SAN,"SAN_CV.R"))

#plot survey indecies
source(file.path(prog.path.SAN,"SAN_survey_index_timeline.R"))#makes mddir/survey_index.png



# Forecast
# remember to set the variable in start of the script
source(file.path(prog.path.SAN,"SAN_forecast_not_rounded.r"))


###################  
