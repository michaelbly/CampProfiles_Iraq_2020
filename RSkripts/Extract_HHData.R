############################
#Import HH-Level Dataset
##############
##################
#Import HH-level Data
HH<-read.xlsx("dataset/(DRAFT) REACH_IRQ_CCCM_Sources of data_Camp profiling_SEPT2019.xlsx", sheet="CP HH data")
#Subset relevant individual-level data
names(HH)
subset <- c("Camp_name", "Governorate", "num_hh_member", "tot_male", "tot_female", "male_60", "female_60", "male_18_59", 
            "female_18_59", "male_6_17", "female_6_17", "male_3_5", "female_3_5", "male_0_2", 
            "female_0_2", "FHoHH_ind_data", "no_restriction", "issues_shelter", 
            "shared_latrines", "tot_income", "tot_expenditure", 
            "need_access_healthcare", "issue_accessing_healthcare", "presence_children_not_attending1", 
            "coping_strategy_last_week/cheaper_quality", "have_ID_card_a18", 
            "have_citizenship_certificate_a18", "have_marriage_license_a18", "have_passport_a18", "have_passport_u18", 
            "have_ID_card_u18", "have_citizenship_certificate_u18", "have_marriage_license_u18", 
            "coping_strategy_last_month/selling_assets", "coping_strategy_last_month/borrow_debt", "coping_strategy_last_month/spent_savings", 
            "coping_strategy_last_month/selling_transportation_means", "coping_strategy_last_month/child_droput_school", "coping_strategy_last_month/reduce_spending",                  
            "coping_strategy_last_month/change_place", "coping_strategy_last_month/adults_illigal_acts", "coping_strategy_last_month/child_work",                       
            "coping_strategy_last_month/family_migrating", "coping_strategy_last_month/forced_marriage", "nfi_priority_needs")
HH <- HH[subset]

##########################
#Calculate population percentages
subset <- c("Camp_name", "num_hh_member", "tot_male", "tot_female", "male_60", "female_60", "male_18_59", 
            "female_18_59", "male_6_17", "female_6_17", "male_3_5", "female_3_5", "male_0_2", 
            "female_0_2")
population <- HH[subset]
population <-aggregate(population[-c(1)], by=list(Category=population$Camp_name), FUN=sum)
population[-c(1, 2)] <- population[-c(1, 2)] / population[["num_hh_member"]] 
population$male_0_5 <- population$male_3_5 + population$male_0_2
population$female_0_5 <- population$female_3_5 + population$female_0_2
population[c("male_3_5", "female_3_5")] <- NULL


############################
#Recode non-numerical data 
############################
#Female headed HHs
HH$FHoHH_ind_data <- ifelse(HH$FHoHH_ind_data %in% c("Yes"), 1, 0)
#Freedom of Movement
HH$no_restriction <- ifelse(HH$no_restriction %in% c("Yes"), 1, 0)
#Issues Shelter (1: none, 0: issues)
HH$issues_shelter <- ifelse(HH$issues_shelter %in% c("No issues"), 1, 0)
#Shared Latrine (1: yes, 0: no)
HH$shared_latrines <- ifelse(HH$shared_latrines %in% c("Yes"), 1, 0)
#Percentage of households requiring healthcare services (1: yes, 0: no)
HH$need_access_healthcare <- ifelse(HH$need_access_healthcare %in% c("Yes"), 1, 0)
#Barriers to accessing healthcare (1: facing barriers to accessing healthcare, 0: no barriers)
HH$issue_accessing_healthcare <- ifelse(is.na(HH$issue_accessing_healthcare), NA,
                                  ifelse(HH$issue_accessing_healthcare %in% c("No issues"), 0, 1))
#NFI needs (1: NFI needs  0: none of the above)
HH$nfi_priority_needs <- ifelse(HH$nfi_priority_needs %in% c("None of the above"), 0, 1)

##########################
#Calculations for HH Data
##########################
#Count the number of HH Interviews
countHH<-HH %>% plyr::count("Camp_name")
#Calculate the indicator for missing documentation (1: if missing passport, id card, citizenship certificate, or marriage license --> for both u18 and a18)
HH$documents <- ifelse(HH$have_ID_card_a18 %in% c("Yes") &
                   HH$have_citizenship_certificate_a18  %in% c("Yes") &
                   HH$have_ID_card_u18 %in% c("Yes", "No one under 18 years in household") &
                   HH$have_citizenship_certificate_u18 %in% c("Yes", "No one under 18 years in household"), 1, 0)

#Calculate the percentage of people using some form of consumption based coping strategy in 30 days prior to data collection (1: yes, 0: no)
HH$coping <- ifelse(HH$`coping_strategy_last_month/selling_assets` %in% c("Yes") |
                         HH$`coping_strategy_last_month/borrow_debt`  %in% c("Yes") |
                         HH$`coping_strategy_last_month/spent_savings` %in% c("Yes") |
                         HH$`coping_strategy_last_month/selling_transportation_means` %in% c("Yes") |
                      HH$`coping_strategy_last_month/child_droput_school` %in% c("Yes") |
                      HH$`coping_strategy_last_month/reduce_spending` %in% c("Yes") |
                      HH$`coping_strategy_last_month/change_place` %in% c("Yes") |
                      HH$`coping_strategy_last_month/adults_illigal_acts` %in% c("Yes") |
                      HH$`coping_strategy_last_month/child_work` %in% c("Yes") |
                      HH$`coping_strategy_last_month/family_migrating` %in% c("Yes") |
                      HH$`coping_strategy_last_month/forced_marriage` %in% c("Yes"), 1, 0)  

#Aggregate relevant HH-level data
subset <- c("Camp_name", "FHoHH_ind_data", "no_restriction", "issues_shelter", 
            "shared_latrines", "tot_income", "tot_expenditure", 
            "need_access_healthcare", "issue_accessing_healthcare", "presence_children_not_attending1", 
            "documents", "coping", "nfi_priority_needs")
HHagg <- HH[subset]
names(HHagg)
HHagg<-aggregate(HHagg[, 2:13], list(HHagg$Camp_name), mean, na.rm=T)
#Merge with number of interviews per camp
colnames(HHagg)[colnames(HHagg)=="Group.1"] <- "Camp_name"
HHagg <- merge(HHagg,countHH,by="Camp_name", all=T)
colnames(HHagg)[colnames(HHagg)=="freq"] <- "Interviews"

#Calculate the percentage of private latrines per camp
HHagg$private_latrines <- 1 - HHagg$shared_latrines

#Merge the aggregate dataset with the dataset containing the Governorate names
subset <- c("Camp_name", "Governorate")
governorate <- HH[subset]
governorate <- dplyr::distinct(governorate)
HHagg <- merge(HHagg,governorate,by="Camp_name", all=F)


