#################################
#Extract HH-level Data to calculate top-5 priorities
#################################

#Function to aggregate data and change variable names
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

#Import Dataset on an individual level
HH<-read.xlsx("dataset/(DRAFT) REACH_IRQ_CCCM_Sources of data_Camp profiling_SEPT2019.xlsx", sheet="CP HH data")
names(HH)

#Subset Information Needs
subset <- c("Camp_name","information_needs/assistance",	"information_needs/sponsorship_programs",	"information_needs/return_aoo",	"information_needs/missing_documents",	"information_needs/contact_family",	"information_needs/enrol_children",	"information_needs/lodge_complaint",	"information_needs/job_oppourtunities",	"information_needs/health_facilities",	"information_needs/security_restrictions",	"information_needs/documentation",	"information_needs/none",	"information_needs/other")
infoneed <- HH[subset]

attach(infoneed)
infoneed1 <- celsius(`information_needs/sponsorship_programs`)
colnames(infoneed1)[colnames(infoneed1)=="x"] <- "information_needs/sponsorship_programs"

infoneed2 <- celsius(`information_needs/return_aoo`)
colnames(infoneed2)[colnames(infoneed2)=="x"] <- "information_needs/return_aoo"

infoneed3 <- celsius(`information_needs/missing_documents`)
colnames(infoneed3)[colnames(infoneed3)=="x"] <- "information_needs/missing_documents"

infoneed4 <- celsius(`information_needs/contact_family`)
colnames(infoneed4)[colnames(infoneed4)=="x"] <- "information_needs/contact_family"

infoneed5 <- celsius(`information_needs/enrol_children`)
colnames(infoneed5)[colnames(infoneed5)=="x"] <- "information_needs/enrol_children"

infoneed6 <- celsius(`information_needs/lodge_complaint`)
colnames(infoneed6)[colnames(infoneed6)=="x"] <- "information_needs/lodge_complaint"

infoneed7 <- celsius(`information_needs/job_oppourtunities`)
colnames(infoneed7)[colnames(infoneed7)=="x"] <- "information_needs/job_oppourtunities"

infoneed8 <- celsius(infoneed$`information_needs/assistance`)
colnames(infoneed8)[colnames(infoneed8)=="x"] <- "information_needs/assistance"

infoneed9 <- celsius(`information_needs/health_facilities`)
colnames(infoneed9)[colnames(infoneed9)=="x"] <- "information_needs/health_facilities"

infoneed10 <- celsius(`information_needs/security_restrictions`)
colnames(infoneed10)[colnames(infoneed10)=="x"] <- "information_needs/security_restrictions"

infoneed11 <- celsius(`information_needs/documentation`)
colnames(infoneed11)[colnames(infoneed11)=="x"] <- "information_needs/documentation"

infoneed12 <- celsius(`information_needs/none`)
colnames(infoneed12)[colnames(infoneed12)=="x"] <- "information_needs/none"

infoneed13 <- celsius(`information_needs/other`)
colnames(infoneed13)[colnames(infoneed13)=="x"] <- "information_needs/other"

#Count number of Respondents by camp
countinfoneed<-infoneed %>% count(Camp_name)

#Merge the datasets for information needs
infoneed<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(infoneed1, infoneed2, 
                                                                     infoneed3, infoneed4, 
                                                                     infoneed5, infoneed6, 
                                                                     infoneed7, infoneed8, 
                                                                     infoneed9, infoneed10, 
                                                                     infoneed11, infoneed12, 
                                                                     infoneed13, countinfoneed))

#Calculate Percentages
infoneed[-c(1, 15)] <- infoneed[-c(1, 15)] / infoneed[["n"]] 

#######################################################################################################################
# 2. Priority Needs (Priority1, Priority2, Priority3)
#######################################################################################################################
########################################################################################################################
############################
#Subset Priority Needs
names(HH)
subset <- c("Camp_name", "priority_needs/education", "priority_needs/CFS",	"priority_needs/employment",	"priority_needs/food",	"priority_needs/medical_care",	"priority_needs/psychosocial_support",	"priority_needs/gbv_support",	"priority_needs/eh_clear",	"priority_needs/eh_education",	"priority_needs/shelter_support",	"priority_needs/water",	"priority_needs/sanitation",	"priority_needs/vocational_training",	"priority_needs/clothing_footwear",	"priority_needs/summerization",	"priority_needs/winterization",	"priority_needs/legal_assistance",	"priority_needs/other")
priorityneeds <- HH[subset]
attach(priorityneeds)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

priorityneeds1 <- celsius(`priority_needs/education`)
colnames(priorityneeds1)[colnames(priorityneeds1)=="x"] <- "priority_needs/education"

priorityneeds2 <- celsius(`priority_needs/CFS`)
colnames(priorityneeds2)[colnames(priorityneeds2)=="x"] <- "priority_needs/CFS"

priorityneeds3 <- celsius(`priority_needs/employment`)
colnames(priorityneeds3)[colnames(priorityneeds3)=="x"] <- "priority_needs/employment"

priorityneeds4 <- celsius(`priority_needs/food`)
colnames(priorityneeds4)[colnames(priorityneeds4)=="x"] <- "priority_needs/food"

priorityneeds5 <- celsius(`priority_needs/medical_care`)
colnames(priorityneeds5)[colnames(priorityneeds5)=="x"] <- "priority_needs/medical_care"

priorityneeds6 <- celsius(`priority_needs/psychosocial_support`)
colnames(priorityneeds6)[colnames(priorityneeds6)=="x"] <- "priority_needs/psychosocial_support"

priorityneeds7 <- celsius(`priority_needs/gbv_support`)
colnames(priorityneeds7)[colnames(priorityneeds7)=="x"] <- "priority_needs/gbv_support"

priorityneeds8 <- celsius(`priority_needs/eh_clear`)
colnames(priorityneeds8)[colnames(priorityneeds8)=="x"] <- "priority_needs/eh_clear"

priorityneeds9 <- celsius(`priority_needs/eh_education`)
colnames(priorityneeds9)[colnames(priorityneeds9)=="x"] <- "priority_needs/eh_education"

priorityneeds10 <- celsius(`priority_needs/shelter_support`)
colnames(priorityneeds10)[colnames(priorityneeds10)=="x"] <- "priority_needs/shelter_support"

priorityneeds11 <- celsius(`priority_needs/water`)
colnames(priorityneeds11)[colnames(priorityneeds11)=="x"] <- "priority_needs/water"

priorityneeds12 <- celsius(`priority_needs/sanitation`)
colnames(priorityneeds12)[colnames(priorityneeds12)=="x"] <- "priority_needs/sanitation"

priorityneeds12 <- celsius(`priority_needs/vocational_training`)
colnames(priorityneeds12)[colnames(priorityneeds12)=="x"] <- "priority_needs/vocational_training"

priorityneeds13 <- celsius(`priority_needs/clothing_footwear`)
colnames(priorityneeds13)[colnames(priorityneeds13)=="x"] <- "priority_needs/clothing_footwear"

priorityneeds14 <- celsius(`priority_needs/summerization`)
colnames(priorityneeds14)[colnames(priorityneeds14)=="x"] <- "priority_needs/summerization"

priorityneeds15 <- celsius(`priority_needs/winterization`)
colnames(priorityneeds15)[colnames(priorityneeds15)=="x"] <- "priority_needs/winterization"

priorityneeds16 <- celsius(`priority_needs/legal_assistance`)
colnames(priorityneeds16)[colnames(priorityneeds16)=="x"] <- "priority_needs/legal_assistance"

priorityneeds17 <- celsius(`priority_needs/other`)
colnames(priorityneeds17)[colnames(priorityneeds17)=="x"] <- "priority_needs/other"

#Count number of Respondents by camp
countpriorityneeds<-priorityneeds %>% count(Camp_name)

#Merge the datasets for information needs
priorityneeds<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(priorityneeds1, priorityneeds2, 
                                                                             priorityneeds3, priorityneeds4, 
                                                                             priorityneeds5, priorityneeds6, 
                                                                             priorityneeds7, priorityneeds8, 
                                                                             priorityneeds9, priorityneeds10, 
                                                                             priorityneeds11, priorityneeds12, 
                                                                             priorityneeds13, priorityneeds14,
                                                                             priorityneeds15, priorityneeds16, 
                                                                             priorityneeds17, countpriorityneeds))

#Calculate Percentages
#Merge with count dataset
priorityneeds[-c(1, 19)] <- priorityneeds[-c(1, 19)] / priorityneeds[["n"]] 
rowSums(priorityneeds[-c(1, 19)])


#######################################################################################################################
# 3. Shelter Needs (Shelter 1, Shelter 2, Shelter 3)
#######################################################################################################################
########################################################################################################################
############################
#Subset Priority Needs
subset <- c("Camp_name", "shelter_better/protec_hazards",	"shelter_better/security_tenure",	
            "shelter_better/improve_safety",	"shelter_better/improve_privacy",
            "shelter_better/protect_climate",	"shelter_better/improve_infrastructure",	
            "shelter_better/improve_structure",	"shelter_better/none",	"shelter_better/extra_tent_needed")
shelterneeds <- HH[subset]
shelterneeds[is.na(shelterneeds)] <- 0
attach(shelterneeds)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

shelterneeds1 <- celsius(`shelter_better/protec_hazards`)
colnames(shelterneeds1)[colnames(shelterneeds1)=="x"] <- "shelter_better/protec_hazards"

shelterneeds2 <- celsius(`shelter_better/security_tenure`)
colnames(shelterneeds2)[colnames(shelterneeds2)=="x"] <- "shelter_better/security_tenure"

shelterneeds3 <- celsius(`shelter_better/improve_safety`)
colnames(shelterneeds3)[colnames(shelterneeds3)=="x"] <- "shelter_better/improve_safety"

shelterneeds4 <- celsius(`shelter_better/improve_privacy`)
colnames(shelterneeds4)[colnames(shelterneeds4)=="x"] <- "shelter_better/improve_privacy"

shelterneeds5 <- celsius(`shelter_better/protect_climate`)
colnames(shelterneeds5)[colnames(shelterneeds5)=="x"] <- "shelter_better/protect_climate"

shelterneeds6 <- celsius(`shelter_better/improve_infrastructure`)
colnames(shelterneeds6)[colnames(shelterneeds6)=="x"] <- "shelter_better/improve_infrastructure"

shelterneeds7 <- celsius(`shelter_better/improve_structure`)
colnames(shelterneeds7)[colnames(shelterneeds7)=="x"] <- "shelter_better/improve_structure"

shelterneeds8 <- celsius(`shelter_better/none`)
colnames(shelterneeds8)[colnames(shelterneeds8)=="x"] <- "shelter_better/none"

shelterneeds9 <- celsius(`shelter_better/extra_tent_needed`)
colnames(shelterneeds9)[colnames(shelterneeds9)=="x"] <- "shelter_better/extra_tent_needed"

#Count number of Respondents by camp
countshelterneeds<-shelterneeds %>% count(Camp_name)

#Merge the datasets for information needs
shelterneeds<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(shelterneeds1, shelterneeds2, 
                                                                            shelterneeds3, shelterneeds4, 
                                                                            shelterneeds5, shelterneeds6, 
                                                                            shelterneeds7, shelterneeds8, 
                                                                            shelterneeds9, countshelterneeds))

#Calculate Percentages
#Merge with count dataset
shelterneeds[-c(1, 11)] <- shelterneeds[-c(1, 11)] / shelterneeds[["n"]] 
rowSums(shelterneeds[-c(1, 11)])

#######################################################################################################################
# 4. NFI Needs
#######################################################################################################################
########################################################################################################################
############################
#Subset Priority Needs
names(HH)
subset <- c("Camp_name", "nfi_priority_needs/bedding_items",	"nfi_priority_needs/mattresses_sleeping_mats",	
            "nfi_priority_needs/blankets",	"nfi_priority_needs/cooking_utensils_kitchen_set",	
            "nfi_priority_needs/cooking_fuel",	"nfi_priority_needs/cooking_stove",	"nfi_priority_needs/water_storage",	
            "nfi_priority_needs/source_of_light",	"nfi_priority_needs/clothing",	"nfi_priority_needs/fan",
            "nfi_priority_needs/AWC",	"nfi_priority_needs/cool_box",	"nfi_priority_needs/winter_heaters",	
            "nfi_priority_needs/heating_fuel",	"nfi_priority_needs/fuel_storage",	"nfi_priority_needs/none",	
            "nfi_priority_needs/other")
nfineeds <- HH[subset]
attach(nfineeds)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

nfineeds1 <- celsius(`nfi_priority_needs/bedding_items`)
colnames(nfineeds1)[colnames(nfineeds1)=="x"] <- "nfi_priority_needs/bedding_items"

nfineeds2 <- celsius(`nfi_priority_needs/mattresses_sleeping_mats`)
colnames(nfineeds2)[colnames(nfineeds2)=="x"] <- "nfi_priority_needs/mattresses_sleeping_mats"

nfineeds3 <- celsius(`nfi_priority_needs/blankets`)
colnames(nfineeds3)[colnames(nfineeds3)=="x"] <- "nfi_priority_needs/blankets"

nfineeds4 <- celsius(`nfi_priority_needs/cooking_utensils_kitchen_set`)
colnames(nfineeds4)[colnames(nfineeds4)=="x"] <- "nfi_priority_needs/cooking_utensils_kitchen_set"

nfineeds5 <- celsius(`nfi_priority_needs/cooking_fuel`)
colnames(nfineeds5)[colnames(nfineeds5)=="x"] <- "nfi_priority_needs/cooking_fuel"

nfineeds6 <- celsius(`nfi_priority_needs/cooking_stove`)
colnames(nfineeds6)[colnames(nfineeds6)=="x"] <- "nfi_priority_needs/cooking_stove"

nfineeds7 <- celsius(`nfi_priority_needs/water_storage`)
colnames(nfineeds7)[colnames(nfineeds7)=="x"] <- "nfi_priority_needs/water_storage"

nfineeds8 <- celsius(`nfi_priority_needs/source_of_light`)
colnames(nfineeds8)[colnames(nfineeds8)=="x"] <- "nfi_priority_needs/source_of_light"

nfineeds9 <- celsius(`nfi_priority_needs/clothing`)
colnames(nfineeds9)[colnames(nfineeds9)=="x"] <- "nfi_priority_needs/clothing"

nfineeds10 <- celsius(`nfi_priority_needs/fan`)
colnames(nfineeds10)[colnames(nfineeds10)=="x"] <- "nfi_priority_needs/fan"

nfineeds11 <- celsius(`nfi_priority_needs/AWC`)
colnames(nfineeds11)[colnames(nfineeds11)=="x"] <- "nfi_priority_needs/AWC"

nfineeds12 <- celsius(`nfi_priority_needs/cool_box`)
colnames(nfineeds12)[colnames(nfineeds12)=="x"] <- "nfi_priority_needs/cool_box"

nfineeds13 <- celsius(`nfi_priority_needs/winter_heaters`)
colnames(nfineeds13)[colnames(nfineeds13)=="x"] <- "nfi_priority_needs/winter_heaters"

nfineeds14 <- celsius(`nfi_priority_needs/heating_fuel`)
colnames(nfineeds14)[colnames(nfineeds14)=="x"] <- "nfi_priority_needs/heating_fuel"

nfineeds15 <- celsius(`nfi_priority_needs/fuel_storage`)
colnames(nfineeds15)[colnames(nfineeds15)=="x"] <- "nfi_priority_needs/fuel_storage"

nfineeds16 <- celsius(`nfi_priority_needs/none`)
colnames(nfineeds16)[colnames(nfineeds16)=="x"] <- "nfi_priority_needs/none"

nfineeds17 <- celsius(`nfi_priority_needs/other`)
colnames(nfineeds17)[colnames(nfineeds17)=="x"] <- "nfi_priority_needs/other"

#Count number of Respondents by camp
countnfineeds<-nfineeds %>% count(Camp_name)

#Merge the datasets for NFI needs
nfineeds<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(nfineeds1, nfineeds2, 
                                                                        nfineeds3, nfineeds4, 
                                                                        nfineeds5, nfineeds6, 
                                                                        nfineeds7, nfineeds8, 
                                                                        nfineeds9, nfineeds10,
                                                                        nfineeds11, nfineeds12, 
                                                                        nfineeds13, nfineeds14, 
                                                                        nfineeds15, nfineeds16, 
                                                                        nfineeds17, countnfineeds))

#Calculate Percentages for shelter needs
#Merge with count dataset
nfineeds[-c(1, 19)] <- nfineeds[-c(1, 19)] / nfineeds[["n"]] 
rowSums(nfineeds[-c(1, 19)])

#######################################################################################################################
# 5. Food Coping Strategy
#######################################################################################################################
########################################################################################################################
############################
#Subset Food Coping Strategies
names(HH)
subset <- c("Camp_name", "coping_strategy_last_month/selling_assets",	"coping_strategy_last_month/borrow_debt",
            "coping_strategy_last_month/spent_savings",	"coping_strategy_last_month/selling_transportation_means",	
            "coping_strategy_last_month/child_droput_school",	"coping_strategy_last_month/reduce_spending",	
            "coping_strategy_last_month/change_place",	"coping_strategy_last_month/adults_illigal_acts",	
            "coping_strategy_last_month/child_work",	"coping_strategy_last_month/family_migrating",	
            "coping_strategy_last_month/forced_marriage")
foodcop <- HH[subset]
foodcop[is.na(foodcop)] <- 0

#Recode Dataset 
table(foodcop$`coping_strategy_last_month/adults_illigal_acts`)
foodcop[foodcop=="No, nobody in my HH did"] <- 0
foodcop[foodcop=="Not applicable (I don't have)"] <- 0
foodcop[foodcop=="No, because we already did it (so cannot continue to do it)"] <- 1
foodcop[foodcop=="Yes"] <- 1
foodcop[,2:12] <- mutate_all(foodcop[,2:12], function(x) as.numeric(as.character(x)))
#shelterneeds[is.na(shelterneeds)] <- 0


attach(foodcop)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

foodcop1 <- celsius(`coping_strategy_last_month/selling_assets`)
colnames(foodcop1)[colnames(foodcop1)=="x"] <- "coping_strategy_last_month/selling_assets"

foodcop2 <- celsius(`coping_strategy_last_month/borrow_debt`)
colnames(foodcop2)[colnames(foodcop2)=="x"] <- "coping_strategy_last_month/borrow_debt"

foodcop3 <- celsius(`coping_strategy_last_month/spent_savings`)
colnames(foodcop3)[colnames(foodcop3)=="x"] <- "coping_strategy_last_month/spent_savings"

foodcop4 <- celsius(`coping_strategy_last_month/selling_transportation_means`)
colnames(foodcop4)[colnames(foodcop4)=="x"] <- "coping_strategy_last_month/selling_transportation_means"

foodcop5 <- celsius(`coping_strategy_last_month/child_droput_school`)
colnames(foodcop5)[colnames(foodcop5)=="x"] <- "coping_strategy_last_month/child_droput_school"

foodcop6 <- celsius(`coping_strategy_last_month/reduce_spending`)
colnames(foodcop6)[colnames(foodcop6)=="x"] <- "coping_strategy_last_month/reduce_spending"

foodcop7 <- celsius(`coping_strategy_last_month/change_place`)
colnames(foodcop7)[colnames(foodcop7)=="x"] <- "coping_strategy_last_month/change_place"

foodcop8 <- celsius(`coping_strategy_last_month/adults_illigal_acts`)
colnames(foodcop8)[colnames(foodcop8)=="x"] <- "coping_strategy_last_month/adults_illigal_acts"

foodcop9 <- celsius(`coping_strategy_last_month/child_work`)
colnames(foodcop9)[colnames(foodcop9)=="x"] <- "coping_strategy_last_month/child_work"

foodcop10 <- celsius(`coping_strategy_last_month/family_migrating`)
colnames(foodcop10)[colnames(foodcop10)=="x"] <- "coping_strategy_last_month/family_migrating"

foodcop11 <- celsius(`coping_strategy_last_month/forced_marriage`)
colnames(foodcop11)[colnames(foodcop11)=="x"] <- "coping_strategy_last_month/forced_marriage"

#Count number of Respondents by camp
countfoodcop<-foodcop %>% count(Camp_name)

#Merge the datasets for Income Sources
foodcop<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(foodcop1, foodcop2, 
                                                                       foodcop3, foodcop4, 
                                                                       foodcop5, foodcop6, 
                                                                       foodcop7, foodcop8, 
                                                                       foodcop9, foodcop10,
                                                                       foodcop11, countfoodcop))

#Calculate Percentages for shelter needs
#Merge with count dataset
foodcop[-c(1, 13)] <- foodcop[-c(1, 13)] / foodcop[["n"]] 
rowSums(foodcop[-c(1, 13)])


#######################################################################################################################
# 6. HH Income Sources
#######################################################################################################################
########################################################################################################################
############################
#HH Income Sources
names(HH)
subset <- c("Camp_name", "primary_livelihood/savings",	"primary_livelihood/employment",	
            "primary_livelihood/remittences",	"primary_livelihood/retirement_pension",	
            "primary_livelihood/renting",	"primary_livelihood/selling_assets",	
            "primary_livelihood/selling_assistance_received",	"primary_livelihood/loans_debts",	
            "primary_livelihood/MODM_cash_assistance",	"primary_livelihood/support_from_community",	
            "primary_livelihood/NGO_charity_assistance",	"primary_livelihood/social_service",	"primary_livelihood/illegal_activity",	
            "primary_livelihood/other")
incomesources <- HH[subset]

attach(incomesources)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

incomesources1 <- celsius(`primary_livelihood/savings`)
colnames(incomesources1)[colnames(incomesources1)=="x"] <- "primary_livelihood/savings"

incomesources2 <- celsius(`primary_livelihood/employment`)
colnames(incomesources2)[colnames(incomesources2)=="x"] <- "primary_livelihood/employment"

incomesources3 <- celsius(`primary_livelihood/remittences`)
colnames(incomesources3)[colnames(incomesources3)=="x"] <- "primary_livelihood/remittences"

incomesources4 <- celsius(`primary_livelihood/retirement_pension`)
colnames(incomesources4)[colnames(incomesources4)=="x"] <- "primary_livelihood/retirement_pension"

incomesources5 <- celsius(`primary_livelihood/renting`)
colnames(incomesources5)[colnames(incomesources5)=="x"] <- "primary_livelihood/renting"

incomesources6 <- celsius(`primary_livelihood/selling_assets`)
colnames(incomesources6)[colnames(incomesources6)=="x"] <- "primary_livelihood/selling_assets"

incomesources7 <- celsius(`primary_livelihood/selling_assistance_received`)
colnames(incomesources7)[colnames(incomesources7)=="x"] <- "primary_livelihood/selling_assistance_received"

incomesources8 <- celsius(`primary_livelihood/loans_debts`)
colnames(incomesources8)[colnames(incomesources8)=="x"] <- "primary_livelihood/loans_debts"

incomesources9 <- celsius(`primary_livelihood/MODM_cash_assistance`)
colnames(incomesources9)[colnames(incomesources9)=="x"] <- "primary_livelihood/MODM_cash_assistance"

incomesources10 <- celsius(`primary_livelihood/support_from_community`)
colnames(incomesources10)[colnames(incomesources10)=="x"] <- "primary_livelihood/support_from_community"

incomesources11 <- celsius(`primary_livelihood/NGO_charity_assistance`)
colnames(incomesources11)[colnames(incomesources11)=="x"] <- "primary_livelihood/NGO_charity_assistance"

incomesources12 <- celsius(`primary_livelihood/social_service`)
colnames(incomesources12)[colnames(incomesources12)=="x"] <- "primary_livelihood/social_service"

incomesources13 <- celsius(`primary_livelihood/illegal_activity`)
colnames(incomesources13)[colnames(incomesources13)=="x"] <- "primary_livelihood/illegal_activity"

incomesources14 <- celsius(`primary_livelihood/other`)
colnames(incomesources14)[colnames(incomesources14)=="x"] <- "primary_livelihood/other"

#Count number of Respondents by camp
countincomesources<-incomesources %>% count(Camp_name)

#Merge the datasets for Income Sources
incomesources<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(incomesources1, incomesources2, 
                                                                             incomesources3, incomesources4, 
                                                                             incomesources5, incomesources6, 
                                                                             incomesources7, incomesources8, 
                                                                             incomesources9, incomesources10,
                                                                             incomesources11, incomesources12, 
                                                                             incomesources13, incomesources14, 
                                                                             countincomesources))

#Calculate Percentages for shelter needs
#Merge with count dataset
incomesources[-c(1, 16)] <- incomesources[-c(1, 16)] / incomesources[["n"]] 
rowSums(incomesources[-c(1, 16)])

#######################################################################################################################
# 7. HH Expenditure
#######################################################################################################################
########################################################################################################################
############################
#HH Expenditure
names(HH)
subset <- c("Camp_name", "shelter_exp_basic_needs",	"food_exp_basic_needs",	
            "electric_exp_basic_needs",	"medical_exp_basic_needs",	
            "education_exp_basic_needs",	"water_exp_basic_needs",	"adult_clothing_exp_basic_needs",	
            "children_clothing_exp_basic_needs",	"other_nfis_items_exp_basic_needs",	
            "transportation_exp_basic_needs",	"communication_exp_basic_needs",	"debt_exp_basic_needs")
expenditure <- HH[subset]

attach(expenditure)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

expenditure1 <- celsius(`shelter_exp_basic_needs`)
colnames(expenditure1)[colnames(expenditure1)=="x"] <- "shelter_exp_basic_needs"

expenditure2 <- celsius(`food_exp_basic_needs`)
colnames(expenditure2)[colnames(expenditure2)=="x"] <- "food_exp_basic_needs"

expenditure3 <- celsius(`electric_exp_basic_needs`)
colnames(expenditure3)[colnames(expenditure3)=="x"] <- "electric_exp_basic_needs"

expenditure4 <- celsius(`medical_exp_basic_needs`)
colnames(expenditure4)[colnames(expenditure4)=="x"] <- "medical_exp_basic_needs"

expenditure5 <- celsius(`medical_exp_basic_needs`)
colnames(expenditure5)[colnames(expenditure5)=="x"] <- "education_exp_basic_needs"

expenditure6 <- celsius(`medical_exp_basic_needs`)
colnames(expenditure6)[colnames(expenditure6)=="x"] <- "water_exp_basic_needs"

expenditure7 <- celsius(`medical_exp_basic_needs`)
colnames(expenditure7)[colnames(expenditure7)=="x"] <- "adult_clothing_exp_basic_needs"

expenditure8 <- celsius(`medical_exp_basic_needs`)
colnames(expenditure8)[colnames(expenditure8)=="x"] <- "adult_clothing_exp_basic_needs"

expenditure9 <- celsius(`children_clothing_exp_basic_needs`)
colnames(expenditure9)[colnames(expenditure9)=="x"] <- "children_clothing_exp_basic_needs"

expenditure10 <- celsius(`other_nfis_items_exp_basic_needs`)
colnames(expenditure10)[colnames(expenditure10)=="x"] <- "other_nfis_items_exp_basic_needs"

expenditure11 <- celsius(`transportation_exp_basic_needs`)
colnames(expenditure11)[colnames(expenditure11)=="x"] <- "transportation_exp_basic_needs"

expenditure12 <- celsius(`communication_exp_basic_needs`)
colnames(expenditure12)[colnames(expenditure12)=="x"] <- "communication_exp_basic_needs"

expenditure13 <- celsius(`debt_exp_basic_needs`)
colnames(expenditure13)[colnames(expenditure13)=="x"] <- "debt_exp_basic_needs"

#Count number of Respondents by camp
countexpenditure<-expenditure %>% count(Camp_name)

#Merge the datasets for Income Sources
expenditure<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(expenditure1, expenditure2, 
                                                                           expenditure3, expenditure4, 
                                                                           expenditure5, expenditure6, 
                                                                           expenditure7, expenditure8, 
                                                                           expenditure9, expenditure10,
                                                                           expenditure11, expenditure12, 
                                                                           expenditure13, countexpenditure))

#Calculate Percentages for shelter needs
#Merge with count dataset
expenditure[-c(1, 15)] <- expenditure[-c(1, 15)] / expenditure[["n"]] 
rowSums(expenditure[-c(1, 15)])

#######################################################################################################################
# 8. Education Barriers
#######################################################################################################################
########################################################################################################################
############################
#HH Expenditure
names(HH)
subset <- c("Camp_name", "tot_child", "barriers_to_education/school_stopped_functioning",	
            "barriers_to_education/security_situation_insecurity",	"barriers_to_education/cannot_afford_to_pay",	
            "barriers_to_education/cannot_registered",	"barriers_to_education/child_disabled",	
            "barriers_to_education/no_space_inschool",	"barriers_to_education/lack_trained_teachers",	
            "barriers_to_education/school_bad_condition",	"barriers_to_education/lack_suitable_curriculum",	
            "barriers_to_education/participate_remunerative_activities",	"barriers_to_education/education_not_important",	
            "barriers_to_education/child_disinterested",	"barriers_to_education/other")
edubarrier <- HH[subset]
edubarrier <- edubarrier[ which(edubarrier$tot_child > 0), ]
edubarrier$tot_child <- NULL
edubarrier[is.na(edubarrier)] <- 0


attach(edubarrier)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

edubarrier1 <- celsius(`barriers_to_education/school_stopped_functioning`)
colnames(edubarrier1)[colnames(edubarrier1)=="x"] <- "barriers_to_education/school_stopped_functioning"

edubarrier2 <- celsius(`barriers_to_education/security_situation_insecurity`)
colnames(edubarrier2)[colnames(edubarrier2)=="x"] <- "barriers_to_education/security_situation_insecurity"

edubarrier3 <- celsius(`barriers_to_education/cannot_afford_to_pay`)
colnames(edubarrier3)[colnames(edubarrier3)=="x"] <- "barriers_to_education/cannot_afford_to_pay"

edubarrier4 <- celsius(`barriers_to_education/cannot_registered`)
colnames(edubarrier4)[colnames(edubarrier4)=="x"] <- "barriers_to_education/cannot_registered"

edubarrier5 <- celsius(`barriers_to_education/child_disabled`)
colnames(edubarrier5)[colnames(edubarrier5)=="x"] <- "barriers_to_education/child_disabled"

edubarrier6 <- celsius(`barriers_to_education/no_space_inschool`)
colnames(edubarrier6)[colnames(edubarrier6)=="x"] <- "barriers_to_education/no_space_inschool"

edubarrier7 <- celsius(`barriers_to_education/lack_trained_teachers`)
colnames(edubarrier7)[colnames(edubarrier7)=="x"] <- "barriers_to_education/lack_trained_teachers"

edubarrier8 <- celsius(`barriers_to_education/school_bad_condition`)
colnames(edubarrier8)[colnames(edubarrier8)=="x"] <- "barriers_to_education/school_bad_condition"

edubarrier9 <- celsius(`barriers_to_education/lack_suitable_curriculum`)
colnames(edubarrier9)[colnames(edubarrier9)=="x"] <- "barriers_to_education/lack_suitable_curriculum"

edubarrier10 <- celsius(`barriers_to_education/participate_remunerative_activities`)
colnames(edubarrier10)[colnames(edubarrier10)=="x"] <- "barriers_to_education/participate_remunerative_activities"

edubarrier11 <- celsius(`barriers_to_education/education_not_important`)
colnames(edubarrier11)[colnames(edubarrier11)=="x"] <- "barriers_to_education/education_not_important"

edubarrier12 <- celsius(`barriers_to_education/child_disinterested`)
colnames(edubarrier12)[colnames(edubarrier12)=="x"] <- "barriers_to_education/child_disinterested"

edubarrier13 <- celsius(`barriers_to_education/other`)
colnames(edubarrier13)[colnames(edubarrier13)=="x"] <- "barriers_to_education/other"

#Count number of Respondents by camp
countedubarrier<-edubarrier %>% count(Camp_name)

#Merge the datasets for Income Sources
edubarrier<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(edubarrier1, edubarrier2, 
                                                                           edubarrier3, edubarrier4, 
                                                                           edubarrier5, edubarrier6, 
                                                                           edubarrier7, edubarrier8, 
                                                                           edubarrier9, edubarrier10,
                                                                           edubarrier11, edubarrier12, 
                                                                           edubarrier13, countedubarrier))

#Calculate Percentages for shelter needs
#Merge with count dataset
edubarrier[-c(1, 15)] <- edubarrier[-c(1, 15)] / edubarrier[["n"]] 
rowSums(edubarrier[-c(1, 15)])

#######################################################################################################################
# 9. Barriers to accessing health facilities
#######################################################################################################################
########################################################################################################################
############################
names(HH)
subset <- c("Camp_name", "healthcare_difficulties/none",	"healthcare_difficulties/medecine_cost_high",	"healthcare_difficulties/unqualified_staff",	
            "healthcare_difficulties/refused_treatment",	"healthcare_difficulties/no_medicine_available",	"healthcare_difficulties/not_inclusive_for_disabled",	
            "healthcare_difficulties/treatment_needed_unavailable",	"healthcare_difficulties/civ_docs_problems",	"healthcare_difficulties/no_referral_phc",	
            "healthcare_difficulties/phc_not_open",	"healthcare_difficulties/distance_to_treatmentcenter",	"healthcare_difficulties/not_applicable")
healthbarrier <- HH[subset]

healthbarrier[is.na(healthbarrier)] <- 0


attach(healthbarrier)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

healthbarrier1 <- celsius(`healthcare_difficulties/none`)
colnames(healthbarrier1)[colnames(healthbarrier1)=="x"] <- "healthcare_difficulties/none"

healthbarrier2 <- celsius(`healthcare_difficulties/medecine_cost_high`)
colnames(healthbarrier2)[colnames(healthbarrier2)=="x"] <- "healthcare_difficulties/medecine_cost_high"

healthbarrier3 <- celsius(`healthcare_difficulties/unqualified_staff`)
colnames(healthbarrier3)[colnames(healthbarrier3)=="x"] <- "healthcare_difficulties/unqualified_staff"

healthbarrier4 <- celsius(`healthcare_difficulties/refused_treatment`)
colnames(healthbarrier4)[colnames(healthbarrier4)=="x"] <- "healthcare_difficulties/refused_treatment"

healthbarrier5 <- celsius(`healthcare_difficulties/no_medicine_available`)
colnames(healthbarrier5)[colnames(healthbarrier5)=="x"] <- "healthcare_difficulties/no_medicine_available"

healthbarrier6 <- celsius(`healthcare_difficulties/not_inclusive_for_disabled`)
colnames(healthbarrier6)[colnames(healthbarrier6)=="x"] <- "healthcare_difficulties/not_inclusive_for_disabled"

healthbarrier7 <- celsius(`healthcare_difficulties/treatment_needed_unavailable`)
colnames(healthbarrier7)[colnames(healthbarrier7)=="x"] <- "healthcare_difficulties/treatment_needed_unavailable"

healthbarrier8 <- celsius(`healthcare_difficulties/civ_docs_problems`)
colnames(healthbarrier8)[colnames(healthbarrier8)=="x"] <- "healthcare_difficulties/civ_docs_problems"

healthbarrier9 <- celsius(`healthcare_difficulties/no_referral_phc`)
colnames(healthbarrier9)[colnames(healthbarrier9)=="x"] <- "healthcare_difficulties/no_referral_phc"

healthbarrier10 <- celsius(`healthcare_difficulties/phc_not_open`)
colnames(healthbarrier10)[colnames(healthbarrier10)=="x"] <- "healthcare_difficulties/phc_not_open"

healthbarrier11 <- celsius(`healthcare_difficulties/distance_to_treatmentcenter`)
colnames(healthbarrier11)[colnames(healthbarrier11)=="x"] <- "healthcare_difficulties/distance_to_treatmentcenter"

healthbarrier12 <- celsius(`healthcare_difficulties/not_applicable`)
colnames(healthbarrier12)[colnames(healthbarrier12)=="x"] <- "healthcare_difficulties/not_applicable"

#Count number of Respondents by camp
counthealthbarrier<-healthbarrier %>% count(Camp_name)

#Merge the datasets for Health Barriers
healthbarrier<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(healthbarrier1, healthbarrier2, 
                                                                             healthbarrier3, healthbarrier4, 
                                                                             healthbarrier5, healthbarrier6, 
                                                                             healthbarrier7, healthbarrier8, 
                                                                             healthbarrier9, healthbarrier10,
                                                                             healthbarrier11, healthbarrier12, 
                                                                             counthealthbarrier))

#Calculate Percentages for health barriers
#Merge with count dataset
healthbarrier[-c(1, 14)] <- healthbarrier[-c(1, 14)] / healthbarrier[["n"]] 
rowSums(healthbarrier[-c(1, 14)])

#######################################################################################################################
# 10. WASH Indicators: Primary Sources of Drinking Water
#######################################################################################################################
########################################################################################################################
############################
#Primary Sources of Water
names(HH)
subset <- c("Camp_name", "primary_water_source", "primary_water_source/piped_water_into_compound", 
            "primary_water_source/piped_water_connected_public_tap",
            "primary_water_source/borehole", "primary_water_source/water_trucking",
            "primary_water_source/prot_rainwater_tank", "primary_water_source/prot_spring", 
            "primary_water_source/other", "primary_water_source/bottled_water", 
            "primary_water_source/illegal_connection_piped_network", "primary_water_source/unprot_rainwater_tank", 
            "primary_water_source/unprot_well", "primary_water_source/unprot_spring", 
            "primary_water_source/surface_water", "primary_water_source/prot_well")
            
wash <- HH[subset]

attach(wash)
celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}

wash1 <- celsius(`primary_water_source/piped_water_into_compound`)
colnames(wash1)[colnames(wash1)=="x"] <- "primary_water_source/piped_water_into_compound"

wash2 <- celsius(`primary_water_source/piped_water_connected_public_tap`)
colnames(wash2)[colnames(wash2)=="x"] <- "primary_water_source/piped_water_connected_public_tap"

wash3 <- celsius(`primary_water_source/borehole`)
colnames(wash3)[colnames(wash3)=="x"] <- "primary_water_source/borehole"

wash4 <- celsius(`primary_water_source/water_trucking`)
colnames(wash4)[colnames(wash4)=="x"] <- "primary_water_source/water_trucking"

wash5 <- celsius(`primary_water_source/prot_rainwater_tank`)
colnames(wash5)[colnames(wash5)=="x"] <- "primary_water_source/prot_rainwater_tank"

wash6 <- celsius(`primary_water_source/prot_spring`)
colnames(wash6)[colnames(wash6)=="x"] <- "primary_water_source/prot_spring"

wash7 <- celsius(`primary_water_source/bottled_water`)
colnames(wash7)[colnames(wash7)=="x"] <- "primary_water_source/bottled_water"

wash8 <- celsius(`primary_water_source/illegal_connection_piped_network`)
colnames(wash8)[colnames(wash8)=="x"] <- "primary_water_source/illegal_connection_piped_network"

wash9 <- celsius(`primary_water_source/unprot_rainwater_tank`)
colnames(wash9)[colnames(wash9)=="x"] <- "primary_water_source/unprot_rainwater_tank"

wash10 <- celsius(`primary_water_source/unprot_well`)
colnames(wash10)[colnames(wash10)=="x"] <- "primary_water_source/unprot_well"

wash11 <- celsius(`primary_water_source/unprot_spring`)
colnames(wash11)[colnames(wash11)=="x"] <- "primary_water_source/unprot_spring"

wash12 <- celsius(`primary_water_source/surface_water`)
colnames(wash12)[colnames(wash12)=="x"] <- "primary_water_source/surface_water"

wash13 <- celsius(`primary_water_source/prot_well`)
colnames(wash13)[colnames(wash13)=="x"] <- "primary_water_source/prot_well"

#Count number of Respondents by camp
countwash<-wash %>% count(Camp_name)

#Merge the datasets for Income Sources
wash<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(wash1, wash2, 
                                                                    wash3, wash4, 
                                                                    wash5, wash6, 
                                                                    wash7, wash8, 
                                                                    wash9, wash10,
                                                                    wash11, wash12,
                                                                    wash13, countwash))

#Calculate Percentages for wash
names(wash)
wash[-c(1, 13)] <- wash[-c(1, 13)] / wash[["n"]] 
rowSums(wash[-c(1, 15)])

#######################################################################################################################
#Export all relevant priority datasets individually 
#######################################################################################################################
########################################################################################################################
############################
expenditure$n <- NULL
foodcop$n <- NULL
incomesources$n <- NULL
infoneed$n <- NULL
nfineeds$n <- NULL
priorityneeds$n <- NULL
shelterneeds$n <- NULL
healthbarrier$n <- NULL
edubarrier$n <- NULL
wash$n <- NULL
write.xlsx(expenditure, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/expenditure.xlsx")
write.xlsx(foodcop, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/foodcop.xlsx")
write.xlsx(incomesources, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/incomesources.xlsx")
write.xlsx(infoneed, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/infoneed.xlsx")
write.xlsx(nfineeds, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/nfineeds.xlsx")
write.xlsx(priorityneeds, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/priorityneeds.xlsx")
write.xlsx(shelterneeds, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/shelterneeds.xlsx")
write.xlsx(healthbarrier, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/healthbarrier.xlsx")
write.xlsx(edubarrier, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/edubarrier.xlsx")
write.xlsx(wash, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/wash.xlsx")

#######################################################################################################################
#10. Calculate Food Consumption Score and Percentages by group
#######################################################################################################################
########################################################################################################################
############################
subset <- c("Camp_name", "num_days_consumed/cereals", "num_days_consumed/nuts_seed", "num_days_consumed/milk_dairy",
            "num_days_consumed/meat", "num_days_consumed/vegetables", "num_days_consumed/fruits",                                    
            "num_days_consumed/oil_fats", "num_days_consumed/sweets", "num_days_consumed/spices_condiments")
FoodCons <- HH[subset]
names(FoodCons)
#FoodCons<-aggregate(FoodCons[, 2:10], list(FoodCons$Camp_name), mean, na.rm=T)
FoodCons$fcs <- FoodCons$`num_days_consumed/cereals` * 2 + FoodCons$`num_days_consumed/nuts_seed` * 3 + 
  FoodCons$`num_days_consumed/milk_dairy` * 4 + FoodCons$`num_days_consumed/meat` * 4 + FoodCons$`num_days_consumed/vegetables` + 
  FoodCons$`num_days_consumed/fruits` + FoodCons$`num_days_consumed/oil_fats` * 0.5 + 
  FoodCons$`num_days_consumed/sweets` * 0.5
summary(FoodCons$fcs)
FoodCons$fcs[FoodCons$fcs > 35] <- "Acceptable"
FoodCons$fcs[(FoodCons$fcs >= 21.5) & (FoodCons$fcs <= 35)] <- "Borderline"
FoodCons$fcs[FoodCons$fcs <= 21] <- "Poor"
FoodCons<-plyr::count(FoodCons, c("Camp_name", "fcs"))
CountFoodCons<-plyr::count(FoodCons, c("Camp_name"))
FoodCons <- merge(CountFoodCons, FoodCons, by=c("Camp_name"), all=T)
colnames(FoodCons)[colnames(FoodCons)=="freq.x"] <- "CountCamp"
colnames(FoodCons)[colnames(FoodCons)=="freq.y"] <- "CountCategory"
FoodCons$FCSPC <- FoodCons$CountCategory / FoodCons$CountCamp
FoodCons$CountCamp <- NULL
FoodCons$CountCategory <- NULL
FoodCons <- reshape(FoodCons, idvar = "Camp_name", timevar = "fcs", direction = "wide")
FoodCons[is.na(FoodCons)] <- 0





