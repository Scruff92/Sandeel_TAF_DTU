
#Function to read and plot survey residuals
# parameter start.year: first year on X-axis, default=0 (defined from data)
# parameter end.year: end year on X-axis, default=0 (defined from data)
# 
# use over.all.max to set the maximum size of the reference buble. A value of 0 scales bubles individually  
# dev=screen or wmf with output on a wmf file in the default directory

#cleanup()
if (my.stock.dir=='SAN-area-3-2016-Norway' |my.stock.dir=='SAN-area-1-2016-RTM-request') plot.survey.residuals2(standardize=F,reverse.colors=F,dev='screen',pointsize=12,nox=1,noy=2,Portrait=F,start.year=2000,end.year=2020,over.all.max=1,my.species=NA)
if (my.stock.dir!='SAN-area-3-2016-Norway') plot.survey.residuals2(standardize=F,reverse.colors=F,dev='screen',pointsize=12,nox=1,noy=1,Portrait=F,start.year=2000,end.year=2020,over.all.max=1,my.species=NA)

#plot.survey.residuals2(reverse.colors=T,standardize=F,dev='screen',pointsize=12,nox=1,noy=2,Portrait=T,start.year=1990,end.year=2015,over.all.max=5,my.species=NA)

#plot.survey.residuals2(standardize=T,use.ref.dot=FALSE,dev='screen',nox=1,noy=3,Portrait=T,start.year=1990,end.year=2011,over.all.max=5,my.species=NA)
#plot.survey.residuals2(reverse.colors=T,standardize=T,use.ref.dot=FALSE,dev='screen',nox=1,noy=3,Portrait=T,start.year=1990,end.year=2011,over.all.max=5.5,my.species=NA)
if (My.device =='screen') savePlot(filename= file.path(mdDir,"survey_residuals.png"),type="png")
if (My.device %in% c('png','wmf','pdf')) cleanup()