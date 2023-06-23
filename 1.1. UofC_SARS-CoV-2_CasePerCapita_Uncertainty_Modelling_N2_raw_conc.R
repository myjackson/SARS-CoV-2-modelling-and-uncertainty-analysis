setwd("C:/Users/myjac/OneDrive - University of Calgary/3. Jangwoo_U of C_SARS-Cov-2/2. Data_Analysis/5_CasePerCapita_Analysis/GitHub") # to set current working directory
data_folder = "data" # to set an address for data folder
result_folder = "result" # set an address for result folder

# data input
dat=read.csv(file.path(data_folder,"UofC_Covid_Tracking_Info_Weekly_Data.csv"), sep = ",", row.names = NULL) # wastewater data
dat$Location  = factor(dat$Location, levels=c('YA','CR','UCE','UCS','UCW','WWTP')) # to designate the order of level

# data subset
dat.UCE<-subset(dat,Location=='UCE') # data for the catchment NE
dat.UCS<-subset(dat,Location=='UCS') # data for the catchment SO
dat.UCW<-subset(dat,Location=='UCW') # data for the catchment NW
dat.WWTP<-subset(dat,Location=='WWTP') # data for wastewater treatment plant (WWTP)

# to calculate cases per capita (for WWTP)
case.tot.WWTP<-dat.WWTP$Weekly_Avr_Cases
case.tot.WWTP.normalized<-case.tot.WWTP/1047622 # divided by serving population of 1,047,622 for WWTP-Bonnybrook

# to select N2 abundance for each catchment
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
case.tot.WWTP.normalized<-case.tot.WWTP.normalized[-ls.NA]

######################### Modelling & Monte-Carlo Uncertainty Simulation (n=1000)
total.CPC.combined<-c() # an empty list where the modelled CPC values will be saved
col.name<-c() # an empty list where the names for each run (run.1, run.2, ...) will be saved

# the following code (L43-55) is for the actual modelling & uncertainty analysis 
for (i in c(1:1000)) {
  Area_UCW<-440610*runif(1,min=0.1,max=2.0)
  Area_UCE<-105504*runif(1,min=0.1,max=2.0)
  Area_UCS<-83884*runif(1,min=0.1,max=2.0)
  
  exp.UofC.cases.normalized<-c()
  exp.UofC.cases.normalized<-case.tot.WWTP.normalized*(Area_UCW*conc.N2.UCW+Area_UCE*conc.N2.UCE+Area_UCS*conc.N2.UCS)/((Area_UCW+Area_UCE+Area_UCS)*conc.N2.WWTP)
  
  # to consolidate Cases per capita (CPC) values for boxplot analysis
  CPC.combined<-c(case.tot.WWTP.normalized,exp.UofC.cases.normalized)
  total.CPC.combined<-cbind(total.CPC.combined,CPC.combined)
  col.name<-c(col.name,paste('run',i,sep='.'))
}

# the following codes are for data-consolidation & framing (L59-67)
CPC.label.combined<-c(rep('WWTP',length(case.tot.WWTP.normalized)),rep('UofC',length(exp.UofC.cases.normalized)))

dat.list<-dat.UCW$Date[-ls.NA]

total.CPC.combined<-cbind(dat.list,CPC.label.combined,total.CPC.combined)
col.name<-c('date','location',col.name)

total.CPC.combined<-as.data.frame(total.CPC.combined)
colnames(total.CPC.combined)<-col.name

# saving the final results
write.table(total.CPC.combined,file.path(result_folder,"results.CPC_N2_raw_conc.csv"),row.names=FALSE,na="", sep=",")
