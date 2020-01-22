#!Note also that to produce the Blim-option I just have to scroll down til a place where it says ###  ### !Her  ### and change setting for Bmsy

#defining if we use long term og 10 year average rec: 
AA <- 34 #area 1r
BB <- 9  #area 2r
CC <- 31 #area 3r
DD <- 9  #area 4

RANGE = 100 #top end of the Fmult range, which the optimize function should explore when calculating Fmsy

TAC.year<-2019

#use this for area 1r
if (my.stock.dir=='SAN-area-1r') scale.options<-c(0,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7)*1
if (my.stock.dir=='SAN-area-1r_MV') scale.options<-c(0,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7)*1
if (my.stock.dir=='SAN-area-1r_explor_old_M') scale.options<-c(0,0.0101,1,1.043,2,3,4,5,6)*1
if (my.stock.dir=='SAN-area-1r_meeting15jan') scale.options<-c(0,0.0101,1,1.043,2,3,4,5,6)*1


#use this for area 3r #!!! hvis der kommer nye stock dir til, s? skal det ogs? tilf?jes/?ndres l?ngere nede i koden
if (my.stock.dir=='SAN-area-3r_forecast_longterm_mean') scale.options<-c(0,0.1999,0.2,0.25,0.3,0.35,0.4522,0.503,1.33)*5

#use this for area 3r
if (my.stock.dir=='SAN-area-3r') scale.options<-c(0,0.08477,0.1,0.15,0.2,0.25,0.3,0.35,0.4)*10
if (my.stock.dir=='SAN-area-3r_explor_new_M') scale.options<-c(0,0.05,0.1,0.15,0.1677,0.25,0.3,0.35,0.4)*5

#use this for area 4
if (my.stock.dir=='SAN-area-4') scale.options<-c(0,0.09855,0.033333,0.265,0.3,0.35,0.4,0.45,0.5)*10 

#use this for area 2r
if (my.stock.dir=='SAN-area-2r') scale.options<-c(0,0.1,0.3,0.4,0.5,0.6,0.7,0.8,0.9)*1
if (my.stock.dir=='SAN-area-2r_meeting15jan') scale.options<-c(0,0.0997,0.3,0.3425,0.4,0.45,0.5,0.55,0.6)*10

#use this for area 2r
if (my.stock.dir=='SAN-area-2r_stoch_forecast') scale.options<-c(0,0.0997,0.3,0.3425,0.4,0.45,0.5,0.55,0.6)*10

#use this for area 3rexplorative
#scale.options<-c(0,0.25,0.3,0.35,0.4,0.468,0.5,0.55,0.6)*6

roundTAC<-3   # number of decimals in TAC 

recruimentMultipliers <-seq(0,1.0,0.2)   

save.on.file<-T   #  output on file or on screen only

Recruit.in.Assess.year<- 0     #  >0 = Overwrite estimate from assessment with the given input value. Value must be given as recruitment (i.e 0-group in the assessment year) 
                               #   0 = Use value estimated from assessment (default)
                               #  -1 = make forcast from based on values of 1-group in the TAC year (for Real time monitoring purposes) 

#  user options according to stock annex, Please keep unchanged. 
PM.TAC.year<-TAC.year 
#PM.after.TAC.year<-seq(1983,2000,1)                       # Year or year Range for Proportion Mature for the TAC year
PM.after.TAC.year<-seq(1983,TAC.year-1,1)         # Year or year Range for Proportion Mature for the year after TAC year  (default long term average)
west.TAC.year<-seq(TAC.year-5,TAC.year-1,1) # Year or year Range for weight in the Sea  for the TAC year  (default the 3 most recent years ~ seq(TAC.year-3,TAC.year-1,1))
weca.TAC.year<-seq(TAC.year-5,TAC.year-1,1) # Year or year Range for weight in the Catch  for the TAC year (default the 3 most recent years ~seq(TAC.year-3,TAC.year-1,1))
NatMor.year<-TAC.year-1                      # Year or year Range for Natural mortality (M) (default the most recent year)

## end user options, do not change in the code below


############

# read assessment summary file
s<-Read.summary.data(extend=TRUE)

#recruitment geometric mean

tmp<-subset(s,Year < (TAC.year-1) & Quarter==SMS.control@rec.season & Age==SMS.control@first.age,select=c(Year,N))


if (my.stock.dir=='SAN-area-1r') firstREC = length(tmp$N)- AA 
if (my.stock.dir=='SAN-area-1r_MV') firstREC = length(tmp$N)- AA 
if (my.stock.dir=='SAN-area-1r_explor_old_M') firstREC = length(tmp$N)- AA 
if (my.stock.dir=='SAN-area-1r_meeting15jan') firstREC = length(tmp$N)- AA 
if (my.stock.dir=='SAN-area-2r') firstREC = length(tmp$N)- BB
if (my.stock.dir=='SAN-area-2r_meeting15jan') firstREC = length(tmp$N)- BB 
if (my.stock.dir=='SAN-area-3r') firstREC = length(tmp$N)- CC
if (my.stock.dir=='SAN-area-3r_explor_new_M') firstREC = length(tmp$N)- CC
if (my.stock.dir=='SAN-area-4') firstREC = length(tmp$N)- DD 





lastREC = length(tmp$N)-0


recruit.TAC.year<-exp(mean(log(tmp$N[firstREC:lastREC])))

#rec.Y.min<-min(tmp$Year); if (rec.Y.min>=2000) rec.Y.min<-rec.Y.min-2000 else rec.Y.min<-rec.Y.min-1900  
#rec.Y.max<-max(tmp$Year);if (rec.Y.max>=2000) rec.Y.max<-rec.Y.max-2000 else rec.Y.max<-rec.Y.max-1900 
rec.Y.min <- tmp$Year[firstREC]
rec.Y.max <- tmp$Year[lastREC]
rec.string<-paste(formatC(rec.Y.min,format='d',flag='0',width=2),'-',formatC(rec.Y.max,format='d',flag='0',width=2),sep='')

Yield.assess<-sum(subset(s,Year %in% (TAC.year-1),select=c(Yield)))/1000

tmp<-subset(s,Year %in% PM.TAC.year & Quarter==1,select=c(Year,Age,propmat))
PM.TAC<-c(0,tapply(tmp$propmat,list(Age=tmp$Age),mean))

tmp<-subset(s,Year %in% PM.after.TAC.year & Quarter==1,select=c(Year,Age,propmat))
PM.after.TAC<-c(0,tapply(tmp$propmat,list(Age=tmp$Age),mean))

tmp<-subset(s,Year %in% west.TAC.year,select=c(Year,Age,Quarter,west))
west.TAC<-tapply(tmp$west,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% weca.TAC.year,select=c(Year,Age,Quarter,weca))
weca.TAC<-tapply(tmp$weca,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% NatMor.year,select=c(Year,Age,Quarter,M))
M<-tapply(tmp$M,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% (TAC.year-1),select=c(Year,Age,Quarter,F))
FF<-tapply(tmp$F,list(Age=tmp$Age,season=tmp$Quarter),mean)

tmp<-subset(s,Year %in% TAC.year & Quarter==1,select=c(Year,Age,N))
N.TAC<-tapply(tmp$N,list(Age=tmp$Age),sum)

if (my.stock.dir=='SAN-area-3-2015-retro') N.TAC[1]<-N.TAC[1]*0.695
N.TAC<-c(recruit.TAC.year,N.TAC)
if (Recruit.in.Assess.year>0) N.TAC[2,1]<-Recruit.in.Assess.year*(exp-(M[1,2]))
No.recruits.in.assessment.year<-subset(s,Year %in% (TAC.year-1) & Quarter==2 & Age==0,select=c(N))$N

outTab<-cbind(N.TAC/1000,FF,west.TAC[,1]*1000,weca.TAC*1000,PM.TAC,PM.after.TAC,M) 
dimnames(outTab)[1]<-list(paste("Age",seq(0,4,1)))
dimnames(outTab)[2]<-list(c(paste('Stock numbers(',TAC.year,')',sep=''),'Exploitation pattern 1st half', 'Exploitation pattern 2nd half','Weight in the stock 1st half','Weight in the catch 1st half','weight in the catch 2nd half',
               paste('Proportion mature(',TAC.year,')',sep=''),paste('Proportion mature(',TAC.year+1,')',sep=''),'Natural mortality 1st half','Natural mortality 2nd half'))




#######################
#######################
#######################


                             
write.csv(t(outTab),file=file.path(data.path,'forecast_input.csv'))





o<-t(outTab)
oo<-matrix('aa',ncol=ncol(o),nrow=nrow(o),dimnames=dimnames(o))
oo[1,]<-as.character(round(o[1,],3))
oo[2:3,]<-as.character(formatC(o[2:3,],digits=3,flag='0',format='f'))
oo[4:10,]<-as.character(formatC(o[4:10,],digits=3,flag='0',format='f'))
save(oo,file=file.path(data.path,'forecast_input_md.RData'))



if (T) {
  colnames(outTab)<-paste(colnames(outTab),c('(millions)',' ',' ','(gram)','(gram)','(gram)',' ',' ',' ',' '))
  xtab3(t(outTab), caption=paste("Table XXX.  ",SMS.control@species.names," Sandeel. input to forecast",sep=''),
       cornername='Age',
     file=file.path(data.path,'_forcast_input.html'), dec=c(0,3,3,2,2,2,2,2,2,2), width='"100%"',units=NULL)
}

cat('\nInput data to outlook table in files: _forcast_input.html  and forecast_input.csv\n')
f.age<-1   # f.age is always 1
l.age<-SMS.control@max.age.all+1  
plus.group<-1   #1=yes, 0=no

f.season<-1
l.season<-SMS.control@last.season
rec.season<-SMS.control@rec.season      


N.TAC<-cbind(N.TAC,N.TAC)
N.TAC[1,1]<-0
N.TAC[(f.age+1):l.age,2]<-0
dimnames(N.TAC)<-list(paste("Age",seq(f.age-1,l.age-1,1)),c('1','2'))





west.TAC[is.na(west.TAC)]<-0
weca.TAC[is.na(weca.TAC)]<-0
FF[is.na(FF)]<-0
M[is.na(M)]<-0

Bmsy<-Read.reference.points(read.init.function=F)[1,'Bpa']               ##### ######### ####### ######## HER! ##############
#Bmsy<-Read.reference.points(read.init.function=F)[1,'Blim']


if (save.on.file) sink(file.path(data.path,"forecast_output.csv"))

####################### Functions #############################

# move the N forward to the end of the year
move.on<-function(N,M,f){
    Z<-M+f
    C<-Z   #copy structure
    for (q in (f.season:l.season)) {
      for (a in (f.age:l.age)){
        if (Z[a,q]>0) C[a,q]<-f[a,q]*N[a,q]*(1-exp(-Z[a,q]))/Z[a,q]
        if (q<l.season) { 
           if (!(q<rec.season & a==f.age)) N[a,q+1]<-N[a,q]*exp(-Z[a,q]) 
        }
      }
    }
   list(N,C)
}

 
birthday<-function(N,Z,PM) {
  N[,1]<-0
  for (a in (l.age:(f.age+1))){
   if (a==l.age && plus.group==1) N[a,f.season]<-N[a-1,l.season]*exp(-Z[a-1,l.season])+N[a,l.season]*exp(-Z[a,l.season])
   if (a<l.age || plus.group==0) N[a,f.season]<-N[a-1,l.season]*exp(-Z[a-1,l.season])
  }
  N[,(f.season+1):l.season]<-0  
  SSB0<-sum(N*PM*WEST)
  list(N,SSB0)
}

#
do.prediction<-function(fmult,RTM,F.default){   
    f<-F.default*fmult
    N<-N.ini
    if (RTM>0) N[2,1]<-RTM*exp(-M[1,rec.season])  # ignore Fishing mortality
    res<-move.on(N,M,f)
    N<-res[[1]]
    C<-res[[2]]
    TAC<<-sum(C*WECA,rm.na=T)/1000
    # longterm data 
    res<-birthday(N,M+f,PM.after.TAC)
    res[[2]]
}

#
calc.fterm<-function(RTM,target.SSB,F.default) {
  mini <- function(x) {
    a<-do.prediction(fmult=x,RTM,F.default) 
    (a[[1]]-target.SSB)^2
  }
  res <- optimize(f=mini,interval=c(0,RANGE) )
  res
}

######## end functions #############################

n.age<-l.age-f.age+1
n.year<-2

# initialize data input
N.ini<-N.TAC
WECA<-weca.TAC
WEST<-west.TAC

SSB0<-sum(N.ini[,1]*WEST[,1]*PM.TAC/1000,na.rm=T)

if (TRUE) {  # added by MV: to get a consistent (with output from the other tables) SSB in thebeginning of the TAC year
  tmp<-subset(Read.SMS.std(),name=="next_log_SSB")
  SSB0.fromStdFile<-exp(tmp$value)
  #cat("SSB in (",TAC.year,") changed from calculated value based in input (N,west and propmat) to the forecast ",SSB0)
  SSB0<-SSB0.fromStdFile/1000
  #cat(" to ",SSB0, " from SSB calculated by SMS in the std file\n ")
  tmp<-tail(subset(Read.SMS.std(),name=="log_recsd"),1)
  Recruit.in.Assess.year<-exp(tmp$value)
} 

mean.f<-sum(FF[(SMS.control@avg.F.ages[1,1]+1):(SMS.control@avg.F.ages[1,2]+1),]) /(SMS.control@avg.F.ages[1,2]-SMS.control@avg.F.ages[1,1]+1)
cat(paste(SMS.control@species.names,"Sandeel\n"))
cat(paste("\nBasis: Fsq=F(",TAC.year-1,")=",round(mean.f,4),";  Yield(",TAC.year-1,")=",round(Yield.assess,3),sep=''))
if (Recruit.in.Assess.year>0) cat(paste("; Recruitment(",TAC.year-1,")=",formatC(Recruit.in.Assess.year/1000000,format='f',digits=6),sep=''))
if (Recruit.in.Assess.year==0) cat(paste("; Recruitment(",TAC.year-1,")=",formatC(No.recruits.in.assessment.year/1000000,format='f',digits=6),sep=''))
if (Recruit.in.Assess.year==-1)  cat(paste("; Recruitment(",TAC.year-1,")= input",sep=''))

cat(paste("; Recruitment(",TAC.year,")=geometric mean (GM ",rec.string,")=", formatC(recruit.TAC.year/1000000,format='f',digits=6)," billions;",sep=''))
if (Recruit.in.Assess.year>=0) cat(paste("SSB(",TAC.year,")=",round(SSB0,3),"\n\n",sep='')) else cat('\n\n')

    
if (Recruit.in.Assess.year>=0) {
  for (fmult in scale.options) {    
    SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000

    if (fmult==scale.options[1]) string<-"F=0," else string<-paste('Fsq*',round(fmult,2),',',sep='')
    if (fmult==scale.options[1]) cat(paste('F multiplier,Basis,F(',TAC.year,'),Catch(',TAC.year,'),SSB(',TAC.year+1,'),%SSB change*,%TAC change**\n',sep=''))
    cat(paste(round(fmult,2),',',string,round(mean.f*fmult,3),',',round(TAC,roundTAC),',',
              round(SSB1,3),',', round((SSB1-SSB0)/SSB0*100),'%,', round((TAC-Yield.assess)/Yield.assess*100),'%','\n'))
 }

 # MSY, find F 
  res<-calc.fterm(RTM=Recruit.in.Assess.year,target.SSB=Bmsy,F.default=FF)
  fmult<-res$minimum
  obj<-res$objective
  if (obj<10) {
      SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000  
      cat(paste(round(fmult,3),',',"MSY",',',round(mean.f*fmult,3),',',round(TAC,roundTAC),',',  round(SSB1,3),',', round((SSB1-SSB0)/SSB0*100),'%,', round((TAC-Yield.assess)/Yield.assess*100),'%','\n'))
  } else cat('No conversion for calculation of MSY catch')
}

if (Recruit.in.Assess.year<0) {
  rr<-recruimentMultipliers
  rec0<-rr* recruit.TAC.year +1
  TACS<-rec0    #copy structure
  F.TAC.mult<-rec0
  F.TAC<-rec0
  SSBS1<-rec0
  SSBS0<-rec0
  obj<-rec0

cat(paste('Assumption: Recruitment(',TAC.year-1,'), Basis,Catch(',TAC.year,'), F(',TAC.year,'),SSB(',TAC.year,'), SSB(',TAC.year+1,'),%SSB change1 \n',sep=''))

  i<-1
  for (r in rec0) {
    res<-calc.fterm(RTM=r,target.SSB=Bmsy,F.default=FF)
    fmult<-res$minimum
    TACS[i]<-TAC
    F.TAC.mult[i]<-fmult
    F.TAC[i]<-F.TAC.mult[i]*mean.f
    obj[i]<-res$objective
    NN<-N.ini
    NN[2,1]<-r*exp(-M[1,rec.season])
    SSBS0[i]<-sum(NN[,1]*WEST[,1]*PM.TAC/1000,na.rm=T)
    SSBS1[i]<-do.prediction(fmult,RTM=r,F.default=FF)/1000  

   cat(paste(round(rr[i]*100),'% of GM,Fsq*',formatC(round(fmult,2),digits=2,format='f'),',',round(TAC,roundTAC),',',formatC(round(mean.f*fmult,2),digits=2,format='f'),',',round(SSBS0[i],3),',', round(SSBS1[i],3),',', round((SSBS1[i]-SSBS0[i])/SSBS0[i]*100),'%,', '\n'))
  
    i<-i+1
  }
  
  
  rec0<-rec0/1000000
  rec1<-rec0*exp(-M[1,2])
  
  a<-data.frame(rec0,rec1,TACS,SSBS0,SSBS1,F.TAC,obj)
  a<-subset(a,obj<10)
  
  newplot(dev="screen",nox=2,noy=2,Portrait=TRUE);

  reg <- lm(TACS ~ rec0, data = a) 
  plot(rec0,TACS,type='p',xlab=paste('Recruitment age 0 in',TAC.year-1),ylab='TAC' )
  title(paste("TAC=",round(reg$coefficients[1],3),"+recruit*",round(reg$coefficients[2],3),sep=''))
  abline(reg)
  
  #print(reg$coefficients)
  
  plot(rec0,F.TAC,xlab=paste('Recruitment age 0 in',TAC.year-1),ylab=paste('F(',TAC.year,')',sep=''))

  #plot(rec0,obj)
  
  reg <- lm(TACS ~ rec1, data = a) 
  plot(rec1,TACS,type='p',xlab=paste('Age 1 in',TAC.year),ylab='TAC')
  title(paste("TAC=",round(reg$coefficients[1],3),"+recruit*",round(reg$coefficients[2],3),sep=''))
  abline(reg)
  #print(reg$coefficients)

  plot(rec0,SSBS0,xlab=paste('Recruitment age 0 in',TAC.year-1),ylab=paste('SSB(',TAC.year,')',sep=''))



}
if (save.on.file) { sink() ;cat('output on file forecast.csv\n') } 
