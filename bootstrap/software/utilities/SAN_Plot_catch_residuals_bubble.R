#Function call to plot catch residuals

#clear graphical windows
# cleanup()

# use over.all.max to set the maximum size of the reference buble. A value of 0 scales bubles individually between plots (e.g by quarter)

plot.catch.residuals2(standardize=F,dev='screen',nox=1,noy=2,Portrait=T,use.ref.dot=TRUE,add.title=T,over.all.max=4,start.year=1982,end.year=2021)

#plot.catch.residuals2(standardize=T,dev='screen',nox=1,noy=1,Portrait=F,use.ref.dot=F,add.title=T,over.all.max=5)
#plot.catch.residuals2(reverse.colors=T,standardize=T,dev='screen',nox=1,noy=1,Portrait=F,use.ref.dot=F,add.title=T,over.all.max=4)
if (My.device =='screen') savePlot(filename= file.path(mdDir,"catch_residuals.png"),type="png")
#if (My.device %in% c('png','wmf','pdf')) cleanup()