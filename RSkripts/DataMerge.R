#######################
#Merge all datasets and clean them
########################
rm(list=ls(all=T))
setwd("~/REACH24919/IDP Camp Directory/2020/RSkripts")
source("Extract_DataMerge.R")
source("Extract_HHData.R")
source("Extract_IntentionsData.R")
source("Extract_KIIData.R")
source("RScript_extracting_Individual.R")
source("Priorities_HH_level.R")
source("Extract_GISData.R")
source("RankingPriorities.R")

#Merge all relevant datasets ()
names(KII)
names(MovementIntentions)
names(HHagg)
names(DataMerge)
names(Individual)
names(GISData)
names(PrioritiesRanking)
colnames(KII)[colnames(KII)=="camp_MCNA"] <- "Camp_name"
colnames(DataMerge)[colnames(DataMerge)=="Camp"] <- "Camp_name"
colnames(GISData)[colnames(GISData)=="Camp"] <- "Camp_name"
colnames(population)[colnames(population)=="Category"] <- "Camp_name"


#same Camp_names: MovementIntentions, HHagg, Individual
#other Camp_names: KII, DataMerge
#Change Camp Names in MovementIntentions, HHagg and Individual datasets to fit the Camp names for the final product
MovementIntentions$Camp_name
MovementIntentions[MovementIntentions=="As Salamyiah"] <- "As Salamyiah (1-2)"
MovementIntentions[MovementIntentions=="Debaga 1"] <- "Debaga"
MovementIntentions[MovementIntentions=="Al-Alam (all)"] <- "Al Alam"
MovementIntentions[MovementIntentions=="Al-Kawthar Camp"] <- "Al Kawthar Camp"
MovementIntentions[MovementIntentions=="Amriyat Al-Fallujah Camp"] <- "Amriyat Al Fallujha (AAF)"
MovementIntentions[MovementIntentions=="Debaga 1"] <- "Debaga"
MovementIntentions[MovementIntentions=="Habbaniya Tourist City camps"] <- "Habbaniya Tourist City (HTC)"
MovementIntentions[MovementIntentions=="Laylan IDP (1)"] <- "Laylan 1"
MovementIntentions[MovementIntentions=="Shooting Camp / Al Ahel"] <- "Al-Ahal Camp"
MovementIntentions[MovementIntentions=="Karamah"] <- "Al-Karama Camp"
MovementIntentions[MovementIntentions=="Hasansham_U2"] <- "Hasansham U2"

Individual$Camp_name
Individual[Individual=="As Salamyiah"] <- "As Salamyiah (1-2)"
Individual[Individual=="Debaga 1"] <- "Debaga"
Individual[Individual=="Al-Alam (all)"] <- "Al Alam"
Individual[Individual=="Al-Kawthar Camp"] <- "Al Kawthar Camp"
Individual[Individual=="Amriyat Al-Fallujah Camp"] <- "Amriyat Al Fallujha (AAF)"
Individual[Individual=="Debaga 1"] <- "Debaga"
Individual[Individual=="Habbaniya Tourist City camps"] <- "Habbaniya Tourist City (HTC)"
Individual[Individual=="Laylan IDP (1)"] <- "Laylan 1"
Individual[Individual=="Shooting Camp / Al Ahel"] <- "Al-Ahal Camp"
Individual[Individual=="Karamah"] <- "Al-Karama Camp"
Individual[Individual=="Hasansham_U2"] <- "Hasansham U2"

HHagg$Camp_name
HHagg[HHagg=="As Salamyiah"] <- "As Salamyiah (1-2)"
HHagg[HHagg=="Debaga 1"] <- "Debaga"
HHagg[HHagg=="Al-Alam (all)"] <- "Al Alam"
HHagg[HHagg=="Al-Kawthar Camp"] <- "Al Kawthar Camp"
HHagg[HHagg=="Amriyat Al-Fallujah Camp"] <- "Amriyat Al Fallujha (AAF)"
HHagg[HHagg=="Debaga 1"] <- "Debaga"
HHagg[HHagg=="Habbaniya Tourist City camps"] <- "Habbaniya Tourist City (HTC)"
HHagg[HHagg=="Laylan IDP (1)"] <- "Laylan 1"
HHagg[HHagg=="Shooting Camp / Al Ahel"] <- "Al-Ahal Camp"
HHagg[HHagg=="Karamah"] <- "Al-Karama Camp"
HHagg[HHagg=="Hasansham_U2"] <- "Hasansham U2"

population$Camp_name
population[population=="As Salamyiah"] <- "As Salamyiah (1-2)"
population[population=="Debaga 1"] <- "Debaga"
population[population=="Al-Alam (all)"] <- "Al Alam"
population[population=="Al-Kawthar Camp"] <- "Al Kawthar Camp"
population[population=="Amriyat Al-Fallujah Camp"] <- "Amriyat Al Fallujha (AAF)"
population[population=="Debaga 1"] <- "Debaga"
population[population=="Habbaniya Tourist City camps"] <- "Habbaniya Tourist City (HTC)"
population[population=="Laylan IDP (1)"] <- "Laylan 1"
population[population=="Shooting Camp / Al Ahel"] <- "Al-Ahal Camp"
population[population=="Karamah"] <- "Al-Karama Camp"
population[population=="Hasansham_U2"] <- "Hasansham U2"

PrioritiesRanking$Camp_name
PrioritiesRanking[PrioritiesRanking=="As Salamyiah"] <- "As Salamyiah (1-2)"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Debaga 1"] <- "Debaga"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Al-Alam (all)"] <- "Al Alam"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Al-Kawthar Camp"] <- "Al Kawthar Camp"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Amriyat Al-Fallujah Camp"] <- "Amriyat Al Fallujha (AAF)"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Debaga 1"] <- "Debaga"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Habbaniya Tourist City camps"] <- "Habbaniya Tourist City (HTC)"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Laylan IDP (1)"] <- "Laylan 1"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Shooting Camp / Al Ahel"] <- "Al-Ahal Camp"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Karamah"] <- "Al-Karama Camp"
PrioritiesRanking$Camp_name[PrioritiesRanking$Camp_name=="Hasansham_U2"] <- "Hasansham U2"

FoodCons$Camp_name
FoodCons[FoodCons=="As Salamyiah"] <- "As Salamyiah (1-2)"
FoodCons[FoodCons=="Debaga 1"] <- "Debaga"
FoodCons[FoodCons=="Al-Alam (all)"] <- "Al Alam"
FoodCons[FoodCons=="Al-Kawthar Camp"] <- "Al Kawthar Camp"
FoodCons[FoodCons=="Amriyat Al-Fallujah Camp"] <- "Amriyat Al Fallujha (AAF)"
FoodCons[FoodCons=="Debaga 1"] <- "Debaga"
FoodCons[FoodCons=="Habbaniya Tourist City camps"] <- "Habbaniya Tourist City (HTC)"
FoodCons[FoodCons=="Laylan IDP (1)"] <- "Laylan 1"
FoodCons[FoodCons=="Shooting Camp / Al Ahel"] <- "Al-Ahal Camp"
FoodCons[FoodCons=="Karamah"] <- "Al-Karama Camp"
FoodCons[FoodCons=="Hasansham_U2"] <- "Hasansham U2"

KII$Camp_name
KII[KII=="As_Salamyiah1_2"] <- "As Salamyiah (1-2)"
GISData$Camp_name
GISData[GISData=="As Salamyiah 1-2"] <- "As Salamyiah (1-2)"


allDataMerge<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=T) ,list(DataMerge, KII, MovementIntentions, 
                                                                             HHagg, Individual,
                                                                             GISData, population, 
                                                                             FoodCons, PrioritiesRanking))

names(allDataMerge)
allDataMerge$pds
#Change Names of Variables based on last round's excel spreadsheet
allDataMerge <- allDataMerge %>% dplyr::rename(
"Camp name" = "Camp_name",
"Area" = "Governorate",
"NumberHH" =	"HHs",
"611EnrolmentLast" =	"611EnrolmentCurrent",
"1217EnrolmentLast" =	"1217EnrolmentCurrent",
"PDSAccessPrev" =	"PDSAccessCurrent",
"HealthServicesPrevious" =	"HealthServicesCurrent",
"CCCMPrevious" =	"CCCMCurrent",
"HHDocumentationPrevious" =	"HHDocumentationCurrent",
"AvgShelterCovPPPrevious" =	"AvgShelterCovgPPCurrent",
"AvgNumperShelterPrevious" =	"AvgNumperShelterCurrent",
"PersonsPerLatrinePrevious" =	"PersonsPerLatrineCurrent",
"PersonsPerShowerPrevious" =	"PersonsPerShowerCurrent",
"FreqSolidWastePrevious" =	"FreqSolidWasteCurrent",
"NumIndividual" =	"Individual",
"Lat" =	"Latitude",
"Long" =	"Longitude",
"CampArea" =	"AREA (M2)",
"CCCMCurrent" =	"Average open area per HH",
"AvgShelterCovgPPCurrent" =	"Average covered area per person",
'PCMale'	=	'tot_male',
'PCFemale'	=	'tot_female',
'PCMale60'	=	'male_60',
'PCFemale60'	=	'female_60',
'PCMale1859'	=	'male_18_59',
'PCFemale1859'	=	'female_18_59',
'PCMale617'	=	'male_6_17',
'PCFemale617'	=	'female_6_17',
'PCMale35'	=	'male_0_5',
'PCFemale35'	=	'female_0_5',
'HHDocumentationCurrent'	=	'documents',
'FHoH'	=	'FHoHH_ind_data',
'Freedom'	=	'no_restriction',
'ShelterNo'	=	'issues_shelter',
'CommunalLatrine'	=	'shared_latrines',
'PrivateLatrine'	=	'private_latrines',
'FoodCopingHHPC'	=	'coping',
'MedianHHIncomeIQD'	=	'tot_income',
'HHExpenditureIQD'	=	'tot_expenditure',
'HHPCHealthcare'	=	'need_access_healthcare',
'HHHCHealthBarriers'	=	'issue_accessing_healthcare',
'HHNoEd'	=	'presence_children_not_attending1',
'PregnantWoman'	=	'pregnant_lactating_Yes',
'Disabilities'	=	'individual_disability1_nodisability0_Yes',
'AdultsWorking'	=	'adult_working_yes1_no0_Yes',
'PCMale611Att'	=	'male_6_11_attending_school_Yes',
'PCFemale611Att'	=	'female_6_11_attending_school_Yes',
'PCMale1217Att'	=	'male_12_17_attending_school_Yes',
'PCFemale1217Att'	=	'female_12_17_attending_school_Yes',
'Management Agency'	=	'cm_agency',
'OccupiedNumber'	=	'occupied_number',
'PlannedCapacity'	=	'shelter_total',
'HealthServicesCurrent'	=	'existing_health_facility',
'PersonsPerLatrineCurrent'	=	'latrine_people',
'PersonsPerShowerCurrent'	=	'shower_people',
'FreqSolidWasteCurrent'	=	'waste_disposal_frequency',
'ShelterType'	=	'shelter_type', 
'NoNFI'	=	'nfi_priority_needs',
'611EnrolmentCurrent'	=	'child_6_11_attending_school_Yes',
'1217EnrolmentCurrent'	=	'child_12_17_attending_school_Yes',
'Chronic'	=	'chronic_health_condition_Yes',
'PCMaleAtt'	=	'male_6_17_attending_school_Yes',
'PCFemaleAtt'	=	'female_6_17_attending_school_Yes',
'FCSPC3'	=	'FCSPC.Acceptable',
'FCSPC2'	=	'FCSPC.Borderline',
'FCSPC1'	=	'FCSPC.Poor',
"AvgNumperShelterCurrent" = "Average nb individuals by shelter",
"HealthServicesCurrent" =	"existing_health_facility", 
"HH Interviews" = "Interviews",
"Camp name" = "Camp_name")

#Export Merged Dataset
write.xlsx(allDataMerge, "~/REACH24919/IDP Camp Directory/2020/RSkripts/FinalMerged.xlsx")


