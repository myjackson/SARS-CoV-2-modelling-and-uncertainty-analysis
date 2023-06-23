setwd("C:/Users/myjac/OneDrive - University of Calgary/3. Jangwoo_U of C_SARS-Cov-2/2. Data_Analysis/5_CasePerCapita_Analysis/GitHub") # to set current working directory
data_folder = "data" # to set an address for data folder
result_folder = "result" # set an address for result folder

dat=read.csv(file.path(data_folder,"UofC_Covid_Tracking_Info_Weekly_Data.csv"), sep = ",", row.names = NULL) # wastewater SARS-CoV-2 N2 & PMMoV data
dat$Location  = factor(dat$Location, levels=c('YA','CR','UCE','UCS', 'UCW', 'WWTP'))

# data subset
dat.UCE<-subset(dat,Location=='UCE') # data for the catchment NE
dat.UCS<-subset(dat,Location=='UCS') # data for the catchment SO
dat.UCW<-subset(dat,Location=='UCW') # data for the catchment NW
dat.WWTP<-subset(dat,Location=='WWTP') # data for wastewater treatment plant (WWTP)
#dat.UofC<-subset(dat,Location=='UofC')

# calculating CPC for WWTP
case.tot.WWTP<-dat.WWTP$Weekly_Avr_Cases
case.tot.WWTP.normalized<-case.tot.WWTP/1047622 # divided by serving population of 1,047,622 for WWTP-Bonnybrook

## N2 - the following codes (L22-37) will generate data lists for 'N2'
# the lists for N2 data
conc.N2.UCW<-dat.UCW$N2_Weekly_Avr
conc.N2.UCS<-dat.UCS$N2_Weekly_Avr
conc.N2.UCE<-dat.UCE$N2_Weekly_Avr
conc.N2.WWTP<-dat.WWTP$N2_Weekly_Avr

# to select those dates where 'all-three' catchments were positive
ls.NA.UCW<-which(is.na(conc.N2.UCW))
ls.NA.UCS<-which(is.na(conc.N2.UCS))
ls.NA.UCE<-which(is.na(conc.N2.UCE))

ls.NA<-unique(c(ls.NA.UCW, ls.NA.UCS, ls.NA.UCE))

conc.N2.UCW<-conc.N2.UCW[-ls.NA]
conc.N2.UCS<-conc.N2.UCS[-ls.NA]
conc.N2.UCE<-conc.N2.UCE[-ls.NA]
conc.N2.WWTP<-conc.N2.WWTP[-ls.NA]

## PMMoV - the following codes (L40-55) will generate data lists for 'PMMoV'
conc.PMMoV.UCW<-dat.UCW$PMMoV_Weekly_Avr
conc.PMMoV.UCS<-dat.UCS$PMMoV_Weekly_Avr
conc.PMMoV.UCE<-dat.UCE$PMMoV_Weekly_Avr
conc.PMMoV.WWTP<-dat.WWTP$PMMoV_Weekly_Avr

# to select those dates where 'all-three' catchments were positive
ls.NA.UCW<-which(is.na(conc.PMMoV.UCW))
ls.NA.UCS<-which(is.na(conc.PMMoV.UCS))
ls.NA.UCE<-which(is.na(conc.PMMoV.UCE))

ls.NA<-unique(c(ls.NA.UCW, ls.NA.UCS, ls.NA.UCE))

conc.PMMoV.UCW<-conc.PMMoV.UCW[-ls.NA]
conc.PMMoV.UCS<-conc.PMMoV.UCS[-ls.NA]
conc.PMMoV.UCE<-conc.PMMoV.UCE[-ls.NA]
conc.PMMoV.WWTP<-conc.PMMoV.WWTP[-ls.NA]

case.tot.WWTP.normalized<-case.tot.WWTP.normalized[-ls.NA]

######################### Modelling & Monte-Carlo Uncertainty Simulation (n=1000)
c.UofC.combined<-c() # A list for modelled-UofC concentration (for N2)
c.UofC.f.combined<-c() # A list for modelled-UofC concentration (for PMMoV)
col.name<-c() # A list for column names (for combined data frame)

CPC.UofC.combined<-c() # A list for CPC for UofC

for (i in c(1:1000)) {
  Area_UCW<-440610*runif(1,min=0.1,max=2.0)
  Area_UCE<-105504*runif(1,min=0.1,max=2.0)
  Area_UCS<-83884*runif(1,min=0.1,max=2.0)
  
  exp.UofC.cases.normalized<-c()
  
  c.UofC<-(Area_UCW*conc.N2.UCW+Area_UCE*conc.N2.UCE+Area_UCS*conc.N2.UCS)/(Area_UCW+Area_UCE+Area_UCS)
  c.UofC.f<-(Area_UCW*conc.PMMoV.UCW+Area_UCE*conc.PMMoV.UCE+Area_UCS*conc.PMMoV.UCS)/(Area_UCW+Area_UCE+Area_UCS)
  c.WWTP.f<-conc.PMMoV.WWTP
  
  CPC.UofC<-case.tot.WWTP.normalized*(c.UofC*c.WWTP.f)/(conc.N2.WWTP*c.UofC.f)
  
  c.UofC.combined<-cbind(c.UofC.combined,c.UofC)
  c.UofC.f.combined<-cbind(c.UofC.f.combined,c.UofC.f)
  CPC.UofC.combined<-cbind(CPC.UofC.combined,CPC.UofC)
  col.name<-c(col.name,paste('run',i,sep='.'))
  
}

# data framing (L86-95)
dat.list<-dat.UCW$Date[-ls.NA]

total.UofC.combined<-cbind(dat.list,c.UofC.combined)
total.CPC.UofC.combined<-cbind(dat.list,CPC.UofC.combined)
col.name<-c('date',col.name)

total.UofC.combined<-as.data.frame(total.UofC.combined)
total.CPC.UofC.combined<-as.data.frame(total.CPC.UofC.combined)
colnames(total.UofC.combined)<-col.name
colnames(total.CPC.UofC.combined)<-col.name

# saving the final results
write.table(total.CPC.UofC.combined,file.path(result_folder,"results.CPC.N2_PMMoV.csv"),row.names=FALSE,na="", sep=",")
