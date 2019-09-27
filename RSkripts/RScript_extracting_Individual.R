############################
#Extract Individual-Level Dataset
##############
##################
#Import Individual-Level Dataset
Ind<-read.xlsx("dataset/(DRAFT) REACH_IRQ_CCCM_Sources of data_Camp profiling_SEPT2019.xlsx", sheet="CP Ind data")

#Subset relevant individual-level data
names(Ind)

subset <- c("_submission__uuid", "Camp_name", "sex", "pregnant_lactating", 
            "adult_working_yes1_no0", "individual_disability1_nodisability0", 
            "male_6_11_attending_school", "female_6_11_attending_school", 
            "male_12_17_attending_school", "female_12_17_attending_school", 
            "male_6_17_attending_school", "female_6_17_attending_school", 
            "health_condition/chronic_health_condition", "child_6_11", 
            "child_12_17", "male_6_11", "female_6_11", "male_12_17", 
            "female_12_17", "male_6_17", "female_6_17", "child_12_17_attending_school", 
            "child_6_11_attending_school")
Ind <- Ind[subset]
Ind$pregnant_lactating[Ind$pregnant_lactating == 9999] <- NA
Ind$adult_working_yes1_no0[Ind$adult_working_yes1_no0 == 9999] <- NA

#Subset females for calculating the percentage of pregnant/lactating women
Indfemale <- subset(Ind, sex == "Female")
Indfemale <- subset(Indfemale, Indfemale$pregnant_lactating=="No" | Indfemale$pregnant_lactating=="Yes")
subset <- c("_submission__uuid", "Camp_name", "sex", "pregnant_lactating")
Indfemale <- Indfemale[subset]

Indlactating <- plyr::count(Indfemale, c("sex", "Camp_name", "pregnant_lactating"))
Indlactating$pregnant_lactating[Indlactating$pregnant_lactating == "Yes"] <- 1
Indlactating$pregnant_lactating[Indlactating$pregnant_lactating == "No"] <- 0
#library(plyr)
#detach("package:dplyr", unload = TRUE)
#detach("package:plyr", unload = TRUE)
#library("plyr")
#library("dplyr")
#attach(Indlactating)
library(dplyr)
Indlactating <- group_by(Indlactating, Camp_name) %>% transmute(pregnant_lactating, percent = freq/sum(freq))
colnames(Indlactating)[colnames(Indlactating)=="pregnant_lactating"] <- "binary"
colnames(Indlactating)[colnames(Indlactating)=="percent"] <- "pregnant_lactating"

#Subset the percentage of adults working
Indworking <- subset(Ind, Ind$adult_working_yes1_no0==1 | Ind$adult_working_yes1_no0==0)
Indworking<-plyr::count(Indworking, c("Camp_name", "adult_working_yes1_no0"))
Indworking <- group_by(Indworking, Camp_name) %>% transmute(adult_working_yes1_no0, percent = freq/sum(freq))
colnames(Indworking)[colnames(Indworking)=="adult_working_yes1_no0"] <- "binary"
colnames(Indworking)[colnames(Indworking)=="percent"] <- "adult_working_yes1_no0"

#Subset the percentage of people with a disability
Inddisabled <- subset(Ind, Ind$individual_disability1_nodisability0==1 | Ind$individual_disability1_nodisability0==0)
Inddisabled<-plyr::count(Inddisabled, c("Camp_name", "individual_disability1_nodisability0"))
Inddisabled <- group_by(Inddisabled, Camp_name) %>% transmute(individual_disability1_nodisability0, percent = freq/sum(freq))
colnames(Inddisabled)[colnames(Inddisabled)=="individual_disability1_nodisability0"] <- "binary"
colnames(Inddisabled)[colnames(Inddisabled)=="percent"] <- "individual_disability1_nodisability0"

#Subset the percentage of 6-11 year old male attending school
Indschoolmale611 <- subset(Ind, Ind$male_6_11 == 1)
Indschoolmale611<-plyr::count(Indschoolmale611, c("Camp_name", "male_6_11_attending_school"))
Indschoolmale611 <- group_by(Indschoolmale611, Camp_name) %>% transmute(male_6_11_attending_school, percent = freq/sum(freq))
colnames(Indschoolmale611)[colnames(Indschoolmale611)=="male_6_11_attending_school"] <- "binary"
colnames(Indschoolmale611)[colnames(Indschoolmale611)=="percent"] <- "male_6_11_attending_school"

#Subset the percentage of 6-11 year old female attending school
Indschoolfemale611 <- subset(Ind, Ind$female_6_11==1)
Indschoolfemale611<-plyr::count(Indschoolfemale611, c("Camp_name", "female_6_11_attending_school"))
Indschoolfemale611 <- group_by(Indschoolfemale611, Camp_name) %>% transmute(female_6_11_attending_school, percent = freq/sum(freq))
colnames(Indschoolfemale611)[colnames(Indschoolfemale611)=="female_6_11_attending_school"] <- "binary"
colnames(Indschoolfemale611)[colnames(Indschoolfemale611)=="percent"] <- "female_6_11_attending_school"

#Subset the percentage of 12 to 17 year old male attending school
Indschoolmale1217 <- subset(Ind, Ind$male_12_17==1)
Indschoolmale1217<-plyr::count(Indschoolmale1217, c("Camp_name", "male_12_17_attending_school"))
Indschoolmale1217 <- group_by(Indschoolmale1217, Camp_name) %>% transmute(male_12_17_attending_school, percent = freq/sum(freq))
colnames(Indschoolmale1217)[colnames(Indschoolmale1217)=="male_12_17_attending_school"] <- "binary"
colnames(Indschoolmale1217)[colnames(Indschoolmale1217)=="percent"] <- "male_12_17_attending_school"

#Subset the percentage of 12 to 17 year old female attending school
Indschoolfemale1217 <- subset(Ind, Ind$female_12_17==1)
Indschoolfemale1217<-plyr::count(Indschoolfemale1217, c("Camp_name", "female_12_17_attending_school"))
Indschoolfemale1217 <- group_by(Indschoolfemale1217, Camp_name) %>% transmute(female_12_17_attending_school, percent = freq/sum(freq))
colnames(Indschoolfemale1217)[colnames(Indschoolfemale1217)=="female_12_17_attending_school"] <- "binary"
colnames(Indschoolfemale1217)[colnames(Indschoolfemale1217)=="percent"] <- "female_12_17_attending_school"

#Subset the percentage of 6-17 year old male attending school
Indschoolmale617 <- subset(Ind, Ind$male_6_17==1)
Indschoolmale617<-plyr::count(Indschoolmale617, c("Camp_name", "male_6_17_attending_school"))
Indschoolmale617 <- group_by(Indschoolmale617, Camp_name) %>% transmute(male_6_17_attending_school, percent = freq/sum(freq))
colnames(Indschoolmale617)[colnames(Indschoolmale617)=="male_6_17_attending_school"] <- "binary"
colnames(Indschoolmale617)[colnames(Indschoolmale617)=="percent"] <- "male_6_17_attending_school"

#Subset the percentage of 6-17 year old female attending school
Indschoolfemale617 <- subset(Ind, Ind$female_6_17==1)
Indschoolfemale617<-plyr::count(Indschoolfemale617, c("Camp_name", "female_6_17_attending_school"))
Indschoolfemale617 <- group_by(Indschoolfemale617, Camp_name) %>% transmute(female_6_17_attending_school, percent = freq/sum(freq))
colnames(Indschoolfemale617)[colnames(Indschoolfemale617)=="female_6_17_attending_school"] <- "binary"
colnames(Indschoolfemale617)[colnames(Indschoolfemale617)=="percent"] <- "female_6_17_attending_school"

#Subset the percentage of 6-11 year old children attending school
Indschoolchild611 <- subset(Ind, Ind$child_6_11==1)
Indschoolchild611<-plyr::count(Indschoolchild611, c("Camp_name", "child_6_11_attending_school"))
Indschoolchild611 <- group_by(Indschoolchild611, Camp_name) %>% transmute(child_6_11_attending_school, percent = freq/sum(freq))
colnames(Indschoolchild611)[colnames(Indschoolchild611)=="child_6_11_attending_school"] <- "binary"
colnames(Indschoolchild611)[colnames(Indschoolchild611)=="percent"] <- "child_6_11_attending_school"

#Subset the percentage of 12-17 year old children attending school
Indschoolchild1217 <- subset(Ind, Ind$child_12_17==1)
Indschoolchild1217<-plyr::count(Indschoolchild1217, c("Camp_name", "child_12_17_attending_school"))
Indschoolchild1217 <- group_by(Indschoolchild1217, Camp_name) %>% transmute(child_12_17_attending_school, percent = freq/sum(freq))
colnames(Indschoolchild1217)[colnames(Indschoolchild1217)=="child_12_17_attending_school"] <- "binary"
colnames(Indschoolchild1217)[colnames(Indschoolchild1217)=="percent"] <- "child_12_17_attending_school"

#Subset the percentage of chronically ill people
Indchronic <- subset(Ind, Ind$`health_condition/chronic_health_condition`==1 | Ind$`health_condition/chronic_health_condition`==0)
colnames(Ind)[colnames(Ind)=="health_condition/chronic_health_condition"] <- "chronic_health_condition"
Indchronic<-plyr::count(Ind, c("Camp_name", "chronic_health_condition"))
Indchronic <- group_by(Indchronic, Camp_name) %>% transmute(chronic_health_condition, percent = freq/sum(freq))
colnames(Indchronic)[colnames(Indchronic)=="chronic_health_condition"] <- "binary"
colnames(Indchronic)[colnames(Indchronic)=="percent"] <- "chronic_health_condition"

#Merge all individual datasets together
Individual <- merge(Inddisabled, Indlactating, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolfemale1217, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolfemale611, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolfemale617, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolmale1217, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolmale611, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolmale617, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indworking, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolchild1217, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indschoolchild611, by=c("Camp_name", "binary"), all=T)
Individual <- merge(Individual, Indchronic, by=c("Camp_name", "binary"), all=T)

#Rename the binary indicator from 1/0 to Yes/No
Individual$binary[Individual$binary == 1] <- "Yes"
Individual$binary[Individual$binary == 0] <- "No"

#Reshape from long to wide format
Individual<-dcast(melt(Individual, id.vars=c("Camp_name", "binary")), Camp_name~variable+binary)

