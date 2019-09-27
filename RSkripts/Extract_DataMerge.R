################
#LOAD AND ATTACH ALL RELEVANT PACKAGES
###################################
#library(tidyverse)
#library(car)
#library(sandwich)
library(texreg)
library(stringr)
library(vars)
#library(zoo)
library(strucchange)
library(data.table)
library(openxlsx)
library(plm)
library(Formula)
#library(gmm)
library(reshape2)
library(gdata)
library(foreign)
library(plyr)
library(dplyr)
#library(AER)
#library(systemfit)
#library(datasets)
library("readxl")
#library("xlsx")

#######################################################################################################################
# 1. Import Data Previous Round March 2019
#######################################################################################################################
########################################################################################################################
DataMerge<-read.xlsx("dataset/(DRAFT) REACH_IRQ_CCCM_Sources of data_Camp profiling_SEPT2019.xlsx", sheet="Data merge March 2019")
names(DataMerge)
subset <- c("Camp", "SSID", "611EnrolmentCurrent", "1217EnrolmentCurrent", "PDSAccessCurrent", "HealthServicesCurrent", "CCCMCurrent", 
            "HHDocumentationCurrent", "AvgShelterCovgPPCurrent", "AvgNumperShelterCurrent", "PersonsPerLatrineCurrent", 
            "PersonsPerShowerCurrent", "FreqSolidWasteCurrent", "DateOpened")
DataMerge <- DataMerge[subset]


#DUPLICATE THE JAD'AH CAMP ROWS 5-TIMES TO MERGE IT WITH THE OTHER DATASETS
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
DataMerge[c(49:52), ] <- as.data.frame(rep.row(DataMerge[c(40), ],4))

DataMerge$Camp <- as.character(as.vector(DataMerge$Camp))
DataMerge$SSID <- as.character(as.vector(DataMerge$SSID))
DataMerge$`611EnrolmentCurrent` <- as.character(as.vector(DataMerge$`611EnrolmentCurrent`))
DataMerge$`1217EnrolmentCurrent` <- as.character(as.vector(DataMerge$`1217EnrolmentCurrent`))
DataMerge$PDSAccessCurrent <- as.character(as.vector(DataMerge$PDSAccessCurrent))
DataMerge$HealthServicesCurrent <- as.character(as.vector(DataMerge$HealthServicesCurrent))
DataMerge$CCCMCurrent <- as.character(as.vector(DataMerge$CCCMCurrent))
DataMerge$HHDocumentationCurrent <- as.character(as.vector(DataMerge$HHDocumentationCurrent))
DataMerge$AvgShelterCovgPPCurrent <- as.character(as.vector(DataMerge$AvgShelterCovgPPCurrent))
DataMerge$AvgNumperShelterCurrent <- as.character(as.vector(DataMerge$AvgNumperShelterCurrent))
DataMerge$PersonsPerLatrineCurrent <- as.character(as.vector(DataMerge$PersonsPerLatrineCurrent))
DataMerge$PersonsPerShowerCurrent <- as.character(as.vector(DataMerge$PersonsPerShowerCurrent))
DataMerge$FreqSolidWasteCurrent <- as.character(as.vector(DataMerge$FreqSolidWasteCurrent))
DataMerge$DateOpened <- as.character(as.vector(DataMerge$DateOpened))

#Rename the Qayarah-Jad'ah Camps
DataMerge[40, 1] <- "Qayyarah-Jad'ah 1-2"
DataMerge[49, 1] <- "Qayyarah-Jad'ah 3"
DataMerge[50, 1] <- "Qayyarah-Jad'ah 4"
DataMerge[51, 1] <- "Qayyarah-Jad'ah 5"
DataMerge[52, 1] <- "Qayyarah-Jad'ah 6"


