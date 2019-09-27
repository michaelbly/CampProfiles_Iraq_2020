#######################################################################################################################
# 1. Extract KII Data
#######################################################################################################################
########################################################################################################################
KII<-read.xlsx("dataset/(DRAFT) REACH_IRQ_CCCM_Sources of data_Camp profiling_SEPT2019.xlsx", sheet="CP KIIs data")
names(KII)
subset <- c("camp_MCNA", "cm_agency", "occupied_number", "existing_health_facility", 
            "latrine_number", "shower_number", "waste_disposal_frequency", 
            "camp_opened", "shelter_type", "shelter_total", "total_individuals")
KII <- KII[subset]

#Calculate number of people per latrine and number of people per shower
KII$latrine_people <- KII$total_individuals / KII$latrine_number
KII$shower_people <- KII$total_individuals / KII$shower_number
KII$shower_number <- NULL
KII$latrine_number <- NULL

#Recode the Waste Disposal Frequency Data
KII$waste_disposal_frequency <- ifelse(KII$waste_disposal_frequency %in% c("once_aday") |
                      KII$waste_disposal_frequency  %in% c("2_4days_aweek") |
                      KII$waste_disposal_frequency  %in% c("once_aweek"), 
                      "Yes", "No")  

