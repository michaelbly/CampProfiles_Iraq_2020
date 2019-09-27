#######################################################################################################################
# 1. Import Intentions Dataset
#######################################################################################################################
########################################################################################################################
Intentions<-read.xlsx("dataset/(DRAFT) REACH_IRQ_CCCM_Sources of data_Camp profiling_SEPT2019.xlsx", sheet="Intentions")

#Identify the main priority need in order to return for each of the districts
subset <- c("Camp_name", "needs_return/access_to_information_on_the_c", "needs_return/increased_safety_and_security", 
            "needs_return/basic_services__water__electri", "needs_return/healthcare_services", "needs_return/education_services__schooling", 
            "needs_return/transportation_services", "needs_return/psychosocial_services", "needs_return/legal_assistance_needed_regard", 
            "needs_return/functioning_justice_mechanisms", "needs_return/personal_identification_docume", "needs_return/rehabilitation_reconstruction",
             "needs_return/furniture_nfi", "needs_return/food_items", "needs_return/livelihood_income_generating_o", 
            "needs_return/functioning_markets")
Intentions <- Intentions[subset]

celsius <- function(x) {
  firstdata<- aggregate(x, by=list(Category=Camp_name), FUN=sum)
  firstdata <- data.frame(firstdata)
  colnames(firstdata) <- c("Camp_name", "x")
  return(firstdata)
}
attach(Intentions)
Needintention1 <- celsius(`needs_return/access_to_information_on_the_c`)
colnames(Needintention1)[colnames(Needintention1)=="x"] <- "needs_return/access_to_information_on_the_c"

Needintention2 <- celsius(`needs_return/increased_safety_and_security`)
colnames(Needintention2)[colnames(Needintention2)=="x"] <- "needs_return/increased_safety_and_security"

Needintention3 <- celsius(`needs_return/basic_services__water__electri`)
colnames(Needintention3)[colnames(Needintention3)=="x"] <- "needs_return/basic_services__water__electri"

Needintention4 <- celsius(`needs_return/healthcare_services`)
colnames(Needintention4)[colnames(Needintention4)=="x"] <- "needs_return/healthcare_services"

Needintention5 <- celsius(`needs_return/education_services__schooling`)
colnames(Needintention5)[colnames(Needintention5)=="x"] <- "needs_return/education_services__schooling"

Needintention6 <- celsius(`needs_return/transportation_services`)
colnames(Needintention6)[colnames(Needintention6)=="x"] <- "needs_return/transportation_services"

Needintention7 <- celsius(`needs_return/psychosocial_services`)
colnames(Needintention7)[colnames(Needintention7)=="x"] <- "needs_return/psychosocial_services"

Needintention8 <- celsius(`needs_return/legal_assistance_needed_regard`)
colnames(Needintention8)[colnames(Needintention8)=="x"] <- "needs_return/legal_assistance_needed_regard"

Needintention9 <- celsius(`needs_return/functioning_justice_mechanisms`)
colnames(Needintention9)[colnames(Needintention9)=="x"] <- "needs_return/functioning_justice_mechanisms"

Needintention10 <- celsius(`needs_return/personal_identification_docume`)
colnames(Needintention10)[colnames(Needintention10)=="x"] <- "needs_return/personal_identification_docume"

Needintention11 <- celsius(`needs_return/rehabilitation_reconstruction`)
colnames(Needintention11)[colnames(Needintention11)=="x"] <- "needs_return/rehabilitation_reconstruction"

Needintention12 <- celsius(`needs_return/furniture_nfi`)
colnames(Needintention12)[colnames(Needintention12)=="x"] <- "needs_return/furniture_nfi"

Needintention13 <- celsius(`needs_return/food_items`)
colnames(Needintention13)[colnames(Needintention13)=="x"] <- "needs_return/food_items"

Needintention14 <- celsius(`needs_return/livelihood_income_generating_o`)
colnames(Needintention14)[colnames(Needintention14)=="x"] <- "needs_return/livelihood_income_generating_o"

Needintention15 <- celsius(`needs_return/functioning_markets`)
colnames(Needintention15)[colnames(Needintention15)=="x"] <- "needs_return/functioning_markets"



#Count number of Respondents by camp
countneedintention<-Intentions %>% count(Camp_name)

#Merge the datasets for Income Sources
Intentions2<-Reduce(function(x,y) merge(x,y,by="Camp_name",all=TRUE) ,list(Needintention1, Needintention2, 
                                                                           Needintention3, Needintention4, 
                                                                           Needintention5, Needintention6, 
                                                                           Needintention7, Needintention8, 
                                                                           Needintention9, Needintention10,
                                                                           Needintention11, Needintention12, 
                                                                           Needintention13, Needintention14,
                                                                           Needintention15, countneedintention))

#Calculate Percentages for shelter needs
#Identify priority movement
names(Intentions2)
Intentions2[-c(1, 17)] <- Intentions2[-c(1, 17)] / Intentions2[["n"]] 
Intentions2$n<-NULL
Rankintentions <- cbind(Intentions2[1], t(apply(-Intentions2[-1], 1, rank)))
Rankintentions[c(2:16)][Rankintentions[c(2:16)] > 1] <- 0
Rankintentions2 <- Rankintentions
Rankintentions2$Movement <- colnames(Rankintentions2)[apply(Rankintentions2,1,which.max)]
subset <- c("Camp_name", "Movement")
Rankintentions2 <- Rankintentions2[subset]

#Add percentages for priority needs
Rankintentions3 <- Rankintentions
Rankintentions3 <- as.data.frame(mapply("*", Rankintentions3[,-1], Intentions2[,-1]))
Rankintentions3 <- cbind(Intentions2$Camp_name, Rankintentions3)
names(Rankintentions3)
Rankintentions3$PCMovement <- rowSums(Rankintentions3 [,2:16], na.rm=FALSE) 
colnames(Rankintentions3)[colnames(Rankintentions3)=="Intentions2$Camp_name"] <- "Camp_name"
subset <- c("Camp_name", "PCMovement")
Rankintentions3 <- Rankintentions3[subset]

#Merge the two datasets together
MovementIntentions <- merge(Rankintentions2,Rankintentions3,by="Camp_name", all=T)

