### Title : A deterministic model for case prediction using wastewater SARS-CoV-2 data and uncertainty analysis
## Written by Jangwoo Lee, PhD, Postdoctoral Fellow, University of Calgary, Calgary, AB, Canada (jangwoo.lee@ucalgary.ca)
## Related publication : Lee et al., 2023, Campus node-based wastewater surveillance enables COVID-19 case localization and confirms lower SARS-CoV-2 burden relative to the surrounding community, Submitted to Water Research

### Overview : This package includes datasets and R codes for deterministic model for case prediction using wastewater SARS-CoV-2 data and uncertainty analysis. This model enables to predict the number of cases for a campus (or any catchment consist of multiple sub-catchments) based on theory suggested in Lee et al., 2023 (submitted to Wat Res). Furthermore, uncertainty analysis used in this model estimates variability coming from certain variables employed in this model, such as catchment area using Monte-Carlo permutation simulation. Further details could be found in the abovementioned manuscript.   

### Input Files :
#UofC_Covid_Tracking_Info_Weekly_Data.csv : This data include weekly averaged concentrations for SARS-CoV-2 N2, also PMMoV. This also includes weekly averaged cases for wastewater treatment plant (WWTP) that receives wastewaters coming from the university campus, also surrounding communities.

#UofC_Chemical_Data_v3.csv : The concentrations for chemical marker (i.e., potassium).

#results.CPC_N2_ALL.csv : The combined results for modelled values where all the three result files from the workflow 1.1 - 1.3 were 'manually' consolidated. 

### R scripts for modelling and uncertainty analysis : 
#1.1. UofC_SARS-CoV-2_CasePerCapita_Uncertainty_Modelling_N2_raw_conc.R : This is a R code for calculating cases per capita (CPC) for WWTP, also predicting CPC for a campus (UofC) using raw concantration.
#1.2. UofC_SARS-CoV-2_CasePerCapita_Uncertainty_v3_N2_K.R : This is a R code for predicting CPC for UofC using potassium as a marker
#1.3. UofC_SARS-CoV-2_CasePerCapita_Uncertainty_v3_N2_PMMoV.R : This is a R code for predicting CPC for UofC using PMMoV as a marker
#2.0 UofC_SARS-CoV-2_CasePerCapita_Uncertainty_Boxplot.R : This is a R code for visualizing the abovementioned data in boxplot 