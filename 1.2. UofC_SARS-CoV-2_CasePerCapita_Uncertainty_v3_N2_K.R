setwd("C:/Users/myjac/OneDrive - University of Calgary/3. Jangwoo_U of C_SARS-Cov-2/2. Data_Analysis/5_CasePerCapita_Analysis/GitHub") # to set current working directory
data_folder = "data" # to set an address for data folder
result_folder = "result" # set an address for result folder

dat=read.csv(file.path(data_folder,"UofC_Covid_Tracking_Info_Weekly_Data.csv"), sep = ",", row.names = NULL) # wastewater data
dat.chem=read.csv(file.path(data_folder,"UofC_Chemical_Data_v3.csv"), sep = ",", row.names = NULL,check.names = FALSE) # chemical data

dat$Location  = factor(dat$Location, levels=c('YA','CR','UCE','UCS', 'UCW', 'WWTP'))

# data subset (for each location) - chemical data
dat.chem.BBW<-subset(dat.chem,Site == 'BBW') # data for wastewater treatment plant (WWTP)
dat.chem.UCE<-subset(dat.chem,Site == 'UCE') # data for the catchment NE
dat.chem.UCS<-subset(dat.chem,Site == 'UCS') # data for the catchment SO
dat.chem.UCW<-subset(dat.chem,Site == 'UCW') # data for the catchment NW

# calculating median values for pottasium
median.Potassium.WWTP<-median(dat.chem.BBW$Potassium_mg.L,na.rm = TRUE)
median.Potassium.NE<-median(dat.chem.UCE$Potassium_mg.L,na.rm = TRUE)
median.Potassium.NW<-median(dat.chem.UCW$Potassium_mg.L,na.rm = TRUE)
median.Potassium.SO<-median(dat.chem.UCS$Potassium_mg.L,na.rm = TRUE)

# data subset (for each location) - main data
dat.UCE<-subset(dat,Location=='UCE') # data for the catchment NE
dat.UCS<-subset(dat,Location=='UCS') # data for the catchment SO
dat.UCW<-subset(dat,Location=='UCW') # data for the catchment NW
dat.WWTP<-subset(dat,Location=='WWTP') # data for wastewater treatment plant (WWTP)

# calculating CPC for WWTP
case.tot.WWTP<-dat.WWTP$Weekly_Avr_Cases
case.tot.WWTP.normalized<-case.tot.WWTP/1047622 # divided by serving population of 1,047,622 for WWTP-Bonnybrook

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
case.tot.WWTP.normalized<-case.tot.WWTP.normalized[-ls.NA]

######################### Modelling & Monte-Carlo Uncertainty Simulation (n=1000)
c.UofC.combined<-c() # A list for modelled-UofC concentration (for N2)
c.UofC.f.combined<-c() # A list for modelled-UofC concentration (for Pottasium)
col.name<-c() # A list for column names (for combined data frame)

CPC.UofC.combined<-c() # A list for CPC for UofC

# the actual model algorithm here (L59 - 73)
for (i in c(1:1000)) {
  Area_UCW<-440610*runif(1,min=0.2,max=2.0)
  Area_UCE<-105504*runif(1,min=0.2,max=2.0)
  Area_UCS<-83884*runif(1,min=0.2,max=2.0)
  
  c.UofC<-(Area_UCW*conc.N2.UCW+Area_UCE*conc.N2.UCE+Area_UCS*conc.N2.UCS)/(Area_UCW+Area_UCE+Area_UCS)
  c.UofC.f<-(Area_UCW*median.Potassium.NW+Area_UCE*median.Potassium.NE+Area_UCS*median.Potassium.SO)/(Area_UCW+Area_UCE+Area_UCS)
  
  CPC.UofC<-case.tot.WWTP.normalized*(c.UofC*median.Potassium.WWTP)/(conc.N2.WWTP*c.UofC.f)
  
  c.UofC.combined<-cbind(c.UofC.combined,c.UofC)
  c.UofC.f.combined<-cbind(c.UofC.f.combined,c.UofC.f)
  CPC.UofC.combined<-cbind(CPC.UofC.combined,CPC.UofC)
  col.name<-c(col.name,paste('run',i,sep='.'))
}

# data framing (L76-85)
dat.list<-dat.UCW$Date[-ls.NA]

total.UofC.combined<-cbind(dat.list,c.UofC.combined)
total.CPC.UofC.combined<-cbind(dat.list,CPC.UofC.combined)
col.name<-c('date',col.name)

total.UofC.combined<-as.data.frame(total.UofC.combined)
total.CPC.UofC.combined<-as.data.frame(total.CPC.UofC.combined)
colnames(total.UofC.combined)<-col.name
colnames(total.CPC.UofC.combined)<-col.name

# saving the final results
write.table(total.CPC.UofC.combined,file.path(result_folder,"results.CPC_N2_Potassium.csv"),row.names=FALSE,na="", sep=",")
