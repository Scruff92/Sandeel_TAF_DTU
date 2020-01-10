#input and output of Forecast

########################
# Input to forecast

#defining if we use long term og 10 year average rec: 
AA <- 34 #area 1r #this means using a long-term geom 

RANGE = 100 #top end of the Fmult range, which the optimize function should explore when calculating Fmsy

TAC.year<-read.sms.dat_TAF(label = "last.year")+1


#use this for area 1r
scale.options<-c(0,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7)*1

roundTAC<-3   # number of decimals in TAC 

recruimentMultipliers <-seq(0,1.0,0.2)   

Recruit.in.Assess.year<- 0     
#  >0 = Overwrite estimate from assessment with the given input value. Value must be given as recruitment (i.e 0-group in the assessment year) 
#   0 = Use value estimated from assessment (default)
#  -1 = make forcast from based on values of 1-group in the TAC year (for Real time monitoring purposes) 

#  user options according to stock annex, Please keep unchanged. 
PM.TAC.year<-TAC.year 
#PM.after.TAC.year<-seq(1983,2000,1)                       # Year or year Range for Proportion Mature for the TAC year
PM.after.TAC.year<-seq(1983,TAC.year-1,1)         # Year or year Range for Proportion Mature for the year after TAC year  (default long term average)
west.TAC.year<-seq(TAC.year-5,TAC.year-1,1) # Year or year Range for weight in the Sea  for the TAC year  (default the 3 most recent years ~ seq(TAC.year-3,TAC.year-1,1))
weca.TAC.year<-seq(TAC.year-5,TAC.year-1,1) # Year or year Range for weight in the Catch  for the TAC year (default the 3 most recent years ~seq(TAC.year-3,TAC.year-1,1))
NatMor.year<-TAC.year-1                      # Year or year Range for Natural mortality (M) (default the most recent year)

# read assessment summary file
s<-Read.summary.data_TAF()

#recruitment geometric mean
tmp<-subset(s,Year < (TAC.year-1) & Quarter==read.sms.dat_TAF("rec.season") & Age==read.sms.dat_TAF("first.age"),select=c(Year,N))

firstREC = length(tmp$N)- AA 

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

N.TAC<-c(recruit.TAC.year,N.TAC)
if (Recruit.in.Assess.year>0) N.TAC[2,1]<-Recruit.in.Assess.year*(exp-(M[1,2]))
No.recruits.in.assessment.year<-subset(s,Year %in% (TAC.year-1) & Quarter==2 & Age==0,select=c(N))$N

outTab<-cbind(N.TAC/1000,FF,west.TAC[,1]*1000,weca.TAC*1000,PM.TAC,PM.after.TAC,M) 
dimnames(outTab)[1]<-list(paste("Age",seq(0,4,1)))
dimnames(outTab)[2]<-list(c(paste('Stock numbers(',TAC.year,')',sep=''),'Exploitation pattern 1st half', 'Exploitation pattern 2nd half','Weight in the stock 1st half','Weight in the catch 1st half','weight in the catch 2nd half',
                            paste('Proportion mature(',TAC.year,')',sep=''),paste('Proportion mature(',TAC.year+1,')',sep=''),'Natural mortality 1st half','Natural mortality 2nd half'))



write.csv(t(outTab),file=file.path("./output",'forecast_input.csv'))
########################################################

##############################################################

outTab<-read.taf(file = file.path("./output","forecast_input.csv"))

f.age<-1   # f.age is always 1
l.age<-as.numeric(sub("Age ",replacement = "",x = tail(colnames(outTab),1))) + 1 
plus.group<-1   #1=yes, 0=no

f.season<-1
l.season<-read.sms.dat_TAF("last.season") 
rec.season<-read.sms.dat_TAF("rec.season")


N.TAC<-cbind(N.TAC,N.TAC) #(5 rows (4 with row names) and 2 cols)
N.TAC[1,1]<-0
N.TAC[(f.age+1):l.age,2]<-0
dimnames(N.TAC)<-list(paste("Age",seq(f.age-1,l.age-1,1)),c('1','2'))

west.TAC[is.na(west.TAC)]<-0
weca.TAC[is.na(weca.TAC)]<-0
FF[is.na(FF)]<-0
M[is.na(M)]<-0

Bmsy_Bpa<-Read.reference.points_TAF()[1,'Bpa']
Bmsy_Blim<-Read.reference.points_TAF()[1,'Blim']


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

SSB0<-sum(N.ini[,1]*WEST[,1]*PM.TAC/1000,na.rm=T) #97.59

if (FALSE) {  # added by MV: to get a consistent (with output from the other tables) SSB in thebeginning of the TAC year
  tmp<-subset(Read.SMS.std(),name=="next_log_SSB")
  SSB0.fromStdFile<-exp(tmp$value)
  #cat("SSB in (",TAC.year,") changed from calculated value based in input (N,west and propmat) to the forecast ",SSB0)
  SSB0<-SSB0.fromStdFile/1000
  #cat(" to ",SSB0, " from SSB calculated by SMS in the std file\n ")
  tmp<-tail(subset(Read.SMS.std(),name=="log_recsd"),1)
  Recruit.in.Assess.year<-exp(tmp$value)
} 
avg.F.ages<-read.sms.dat_TAF("avg.F.ages")
avg.F.ages<-as.numeric(unlist(strsplit(avg.F.ages,""))[c(1,3)])
mean.f<-sum(FF[(avg.F.ages[1]+1):(avg.F.ages[2]+1),]) /(avg.F.ages[2]-avg.F.ages[1]+1)

#
BASIS=data.frame("Basis Reference" = NA,
                 "Basis Value" = NA)

BASIS[1,1] <- paste0("Fsq=F(",TAC.year-1,")")
BASIS[1,2] <- round(mean.f,4)

BASIS[2,1] <- paste0("Yield(",TAC.year-1,")")
BASIS[2,2] <- round(Yield.assess,3)

if (Recruit.in.Assess.year>0){ 
  BASIS[3,1] <- paste0("Recruitment(",TAC.year-1,")")
  BASIS[3,2] <- formatC(Recruit.in.Assess.year/1000000,format='f',digits=6)}

if (Recruit.in.Assess.year==0) {
  BASIS[3,1] <- paste0("Recruitment(",TAC.year-1,")")
  BASIS[3,2] <- formatC(No.recruits.in.assessment.year/1000000,format='f',digits=6)}

if (Recruit.in.Assess.year==-1){ 
  BASIS[3,1] <- paste0("Recruitment(",TAC.year-1,")")
  BASIS[3,2] <- "input"  }


BASIS[4,1] <- paste0("Recruitment(",TAC.year,")=Geom Mean ",rec.string)
BASIS[4,2] <- paste0(formatC(recruit.TAC.year/1000000,format='f',digits=6))

if (Recruit.in.Assess.year>=0) {
  BASIS[5,1] <- paste0("SSB(",TAC.year,")")
  BASIS[5,2] <- round(SSB0,3)
}

write.taf(BASIS,file = "output/forecast_basis.csv")

#####

sink(file.path("./output","forecast_output.csv"))


if (Recruit.in.Assess.year>=0) {
  for (fmult in scale.options) {    
    SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000
    
    if (fmult==scale.options[1]) string<-"F=0," else string<-paste('Fsq*',round(fmult,2),',',sep='')
    if (fmult==scale.options[1]) cat(paste('F multiplier,Basis,F(',TAC.year,'),Catch(',TAC.year,'),SSB(',TAC.year+1,'),%SSB change*,%TAC change**\n',sep=''))
    cat(paste(round(fmult,2),',',string,round(mean.f*fmult,3),',',round(TAC,roundTAC),',',
              round(SSB1,3),',', round((SSB1-SSB0)/SSB0*100),'%,', round((TAC-Yield.assess)/Yield.assess*100),'%','\n'))
  }
  
  # MSY, find F (based on Bpa)
  res<-calc.fterm(RTM=Recruit.in.Assess.year,target.SSB=Bmsy_Bpa,F.default=FF)
  fmult<-res$minimum
  obj<-res$objective
  if (obj<10) {
    SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000  
    cat(paste(round(fmult,3),',',"MSY (Bpa)",',',round(mean.f*fmult,3),',',round(TAC,roundTAC),',',  round(SSB1,3),',', round((SSB1-SSB0)/SSB0*100),'%,', round((TAC-Yield.assess)/Yield.assess*100),'%','\n'))
  } else cat('No conversion for calculation of MSY catch')
  
  # MSY, find F (based on Blim)
  res<-calc.fterm(RTM=Recruit.in.Assess.year,target.SSB=Bmsy_Blim,F.default=FF)
  fmult<-res$minimum
  obj<-res$objective
  if (obj<10) {
    SSB1<-do.prediction(fmult,RTM=Recruit.in.Assess.year,F.default=FF)/1000  
    cat(paste(round(fmult,3),',',"MSY (Blim)",',',round(mean.f*fmult,3),',',round(TAC,roundTAC),',',  round(SSB1,3),',', round((SSB1-SSB0)/SSB0*100),'%,', round((TAC-Yield.assess)/Yield.assess*100),'%','\n'))
  } else cat('No conversion for calculation of MSY catch')
  
}


sink()

