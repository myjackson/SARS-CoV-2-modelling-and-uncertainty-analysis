setwd("C:/Users/myjac/OneDrive - University of Calgary/3. Jangwoo_U of C_SARS-Cov-2/2. Data_Analysis/5_CasePerCapita_Analysis/GitHub") # to set current working directory
data_folder = "data" # to set an address for data folder
result_folder = "result" # set an address for result folder

## Data input & pre-treatment
dat=read.csv(file.path(data_folder,"results.CPC_N2_ALL.csv"), sep = ",", row.names = NULL) # input data - all the modelled results ('results.CPC_N2_raw_conc.csv','results.CPC.N2_PMMoV.csv','results.CPC_N2_Potassium.csv') are manually combined
dat$Normalization  = factor(dat$Normalization, levels=c('WWTP','Raw', 'PMMoV','Potassium'))

## to draw BoxPlot
par(mar = c(7, 4, 7, 2)) # mar = c(bottom, left, top, right)

boxplot(log10(dat$median*10^5+1)~dat$Normalization,
        boxwex=0.75, xlab = "", ylab = "", cex.axis= 1.0, cex.lab = 1.2, outline=FALSE,
        ylim=c(0.0,2.5),
        xaxt="n",col=c('grey60',rep('grey90',7)))
mtext(expression('Log'[10]~'(Cases per Capita Per 100,000 + 1)'),side = 2, line = 2.5, cex=1.0)

tricks<-c(1:4)
axis(1, at=tricks,labels=FALSE)
text(x=tricks,y=par("usr")[3]-0.2,labels=c('WWTP',rep('UofC',3)),xpd = NA,
     font=1,cex=1.2)
text(x=tricks,y=par("usr")[3]-0.4,labels=c('','Raw', 'PMMoV','K'),xpd = NA,
     font=1,cex=1.0)

## data subset
dat.WWTP<-subset(dat,Normalization=='WWTP')
dat.UofC.N2.raw<-subset(dat,Normalization=='Raw')
dat.UofC.N2.PMMoV<-subset(dat,Normalization=='PMMoV')
dat.UofC.N2.Potassium<-subset(dat,Normalization=='Potassium')

## to display each data point
# dat.WWTP
ran.num<-runif(nrow(dat.WWTP), min=0.8, max=1.2)
points(ran.num+0,log10(dat.WWTP$median*10^5+1),
       cex=1, 
       pch=21,
       col='black')

# dat.UofC.N2.raw
ran.num<-runif(nrow(dat.UofC.N2.raw), min=0.8, max=1.2)
points(ran.num+1,log10(dat.UofC.N2.raw$median*10^5+1),
       cex=1, 
       pch=21,
       col='black')
arrows(ran.num+1, log10(dat.UofC.N2.raw$Q1*10^5+1),
       ran.num+1, log10(dat.UofC.N2.raw$Q3*10^5+1),
       length=0.03, angle=90, code=3)

# dat.UofC.N2.PMMoV
ran.num<-runif(nrow(dat.UofC.N2.PMMoV), min=0.8, max=1.2)
points(ran.num+2,log10(dat.UofC.N2.PMMoV$median*10^5+1),
       cex=1, 
       pch=21,
       col='black')
arrows(ran.num+2, log10(dat.UofC.N2.PMMoV$Q1*10^5+1),
       ran.num+2, log10(dat.UofC.N2.PMMoV$Q3*10^5+1),
       length=0.03, angle=90, code=3)

# dat.UofC.N2.Potassium
ran.num<-runif(nrow(dat.UofC.N2.Potassium), min=0.8, max=1.2)
points(ran.num+3,log10(dat.UofC.N2.Potassium$median*10^5+1),
       cex=1, 
       pch=21,
       col='black')
arrows(ran.num+3, log10(dat.UofC.N2.Potassium$Q1*10^5+1),
       ran.num+3, log10(dat.UofC.N2.Potassium$Q3*10^5+1),
       length=0.03, angle=90, code=3)

## Stats - Pairwise Wilcox test & displaying uncertainty analysis results
pairwise.wilcox.test(dat$median,dat$Normalization, p.adjust.method = "none", paired = FALSE,exact=FALSE) # Un-paired test

## Displaying quantile range
quantile(dat.WWTP$median*10^5,na.rm=TRUE)
quantile(dat.UofC.N2.raw$median*10^5,na.rm=TRUE)
quantile(dat.UofC.N2.PMMoV$median*10^5,na.rm=TRUE)
quantile(dat.UofC.N2.Potassium$median*10^5,na.rm=TRUE)

median(dat.WWTP$median*10^5,na.rm=TRUE)
median(dat.UofC.N2.raw$median*10^5,na.rm=TRUE)
median(dat.UofC.N2.PMMoV$median*10^5,na.rm=TRUE)
median(dat.UofC.N2.Potassium$median*10^5,na.rm=TRUE)
