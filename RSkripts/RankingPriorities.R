library("readxl")
library("reshape2")

getwd()
setwd("~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities")

#LOAD DATA
expenditure <- read_excel("expenditure.xlsx")
foodcop <- read_excel("foodcop.xlsx")
incomesources <- read_excel("incomesources.xlsx")
infoneed <- read_excel("infoneed.xlsx")
nfineeds <- read_excel("nfineeds.xlsx")
priorityneeds <- read_excel("priorityneeds.xlsx")
shelterneeds <- read_excel("shelterneeds.xlsx")
edubarrier <- read_excel("edubarrier.xlsx")
healthbarrier <- read_excel("healthbarrier.xlsx")

######################################################################1
############RANK VALUES: VERSION 2.0##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5)
#direction == write: "highest" or "lowest" : highest (top X) or lowest (bottom X) X indicators
rank_money2 <- function(df, aggunit, toprank, direction) {
  callag <- melt(df, id.vars = c(aggunit))
  print(callag)
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag)) #FIND ID OF GEOGRAPHIC/AGGREGATION UNIT
  unique_units <- unique(callag[id_index]) #UNIQUE GEOGRAPHIC UNITS FOR JOINING LATER
  unique_units<-as.data.frame(unique_units) 
  if(direction == "highest"){ #CHOOSE IF TOP OR BOTTOM X INDICATORS
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  for (i in 1:nrow(unique_units)){   #SUBSET DATA BY GEOGRAPHIC UNIQUE (LIST OF DATAFRAMES)
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ]) #REMOVE DUPLICATE ROWS (PROBABLY NOT NECESSARY ANYMORE)
  sorted_dataframes_list <- lapply(snowflakes, function(df){   #SORT EACH DATAFRAMES IN THE LIST
    df[order(df$value,decreasing = direction),]   #WHERE TOP OR BOTTOM X IS DEFINED
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank) #TAKE THE TOP X ROWS FROM EACH GEOGRAPHIC/AGGREGATION UNIT
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))  #REMOVE SPECIAL CHARACTERS FROM NAME
  }) 
  for (k in 1: nrow(unique_units)){  #CREATE SEPARATE DATAFRAMES FOR VALUES AND NAMES
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){  
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){  #ADD COLUMN HEADERS TO THE VALUE COLUMNS
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){   #ADD COLUMN HEADERS TO THE NAME COLUMNS
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed) #ADD HEADER TO GEOGRAPHIC/AGGREGATION COLUMN
  castedd <- lapply(castedd, setNames, titles)  #COMBINE GEO-AGG and RANK NAME, & VALUES
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))] 
  locations <- unique(locations) #ENSURE LOCATIONS ARE UNIQUE
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)  #UNLIST LIST OF MERGED DATAFRAMES
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)  #ROUND RANKED VALUES
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank]) #REMOVE NAMES IF ASSOCIATED WITH AN "NA" VALUE
  }
  return(ordername)
}

#RUN SCRIPTS AND RENAME VARIABLES
rankedexpenditure <- rank_money2(expenditure,"Camp_name",3,"highest")
names(rankedexpenditure) <- c("Camp_name", "ExpenditurePC1", "ExpenditurePC2", "ExpenditurePC3", "Expenditure1", "Expenditure2", "Expenditure3")
rankedfoodcop <- rank_money2(foodcop,"Camp_name",3,"highest")
names(rankedfoodcop) <- c("Camp_name", "FoodCopingPC1", "FoodCopingPC2", "FoodCopingPC3", "FoodCoping1", "FoodCoping2", "FoodCoping3")
rankedincomesources <- rank_money2(incomesources,"Camp_name",3,"highest")
names(rankedincomesources) <- c("Camp_name", "HHIncomePC1", "HHIncomePC2", "HHIncomePC3", "HHIncome1", "HHIncome2", "HHIncome3")
rankedinfoneed <- rank_money2(infoneed,"Camp_name",3,"highest")
names(rankedinfoneed) <- c("Camp_name", "InfoPC1", "InfoPC2", "InfoPC3", "Info1", "Info2", "Info3")
rankednfineeds <- rank_money2(nfineeds,"Camp_name",3,"highest")
names(rankednfineeds) <- c("Camp_name", "NFIPC1", "NFIPC2", "NFIPC3", "NFI1", "NFI2", "NFI3")
rankedpriorityneeds <- rank_money2(priorityneeds,"Camp_name",3,"highest")
names(rankedpriorityneeds) <- c("Camp_name", "PriorityPC1", "PriorityPC2", "PriorityPC3", "Priority1", "Priority2", "Priority3")
rankedshelterneeds <- rank_money2(shelterneeds,"Camp_name",3,"highest")
names(rankedshelterneeds) <- c("Camp_name", "ShelterPC1", "ShelterPC2", "ShelterPC3", "Shelter1", "Shelter2", "Shelter3")
rankededubarrier <- rank_money2(edubarrier,"Camp_name",3,"highest")
names(rankededubarrier) <- c("Camp_name", "EBPC1", "EBPC2", "EBPC3", "EB1", "EB2", "EB3")
rankedhealthbarrier <- rank_money2(healthbarrier,"Camp_name",3,"highest")
names(rankedhealthbarrier) <- c("Camp_name", "HBPC1", "HBPC2", "HBPC3", "HB1", "HB2", "HB3")
rankedwash <- rank_money2(wash,"Camp_name",3,"highest")
names(rankedwash) <- c("Camp_name", "WASHPC1", "WASHPC2", "WASHPC3", "WASH1", "WASH2", "WASH3")

#CHANGE TO DELIVERABLE NAMES: LOAD FILE FORMATTED THE SAME AS THE EXAMPLE EXCEL FILE (KOBO HEADER AND DESIRED NAME FOR OUTPUT)
setwd("~/REACH24919/IDP Camp Directory/2020/RSkripts/Dataset")
old_new_names <- read_excel("name_translate1.xlsx")
old_new_names$...2 <- NULL
#LOOP THROUGH THE ENTIRE NAME CHANGE SHEET AND APPLY TO THE WHOLE RANKED OUTPUT
names(old_new_names)
#Incomesources
for(i in 1:nrow(old_new_names)){
  rankedincomesources <- as.data.frame(lapply(rankedincomesources, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#Expenditure
for(i in 1:nrow(old_new_names)){
  rankedexpenditure <- as.data.frame(lapply(rankedexpenditure, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankedfoodcop
for(i in 1:nrow(old_new_names)){
  rankedfoodcop <- as.data.frame(lapply(rankedfoodcop, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankedincomesources
for(i in 1:nrow(old_new_names)){
  rankedincomesources <- as.data.frame(lapply(rankedincomesources, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankedinfoneed
for(i in 1:nrow(old_new_names)){
  rankedinfoneed <- as.data.frame(lapply(rankedinfoneed, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankednfineeds
for(i in 1:nrow(old_new_names)){
  rankednfineeds <- as.data.frame(lapply(rankednfineeds, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankedpriorityneeds
for(i in 1:nrow(old_new_names)){
  rankedpriorityneeds <- as.data.frame(lapply(rankedpriorityneeds, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankedshelterneeds
for(i in 1:nrow(old_new_names)){
  rankedshelterneeds <- as.data.frame(lapply(rankedshelterneeds, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankededubarrier
for(i in 1:nrow(old_new_names)){
  rankededubarrier <- as.data.frame(lapply(rankededubarrier, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankedhealthbarrier
for(i in 1:nrow(old_new_names)){
  rankedhealthbarrier <- as.data.frame(lapply(rankedhealthbarrier, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
#rankedwash
for(i in 1:nrow(old_new_names)){
  rankedwash <- as.data.frame(lapply(rankedwash, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}

PrioritiesRanking <- cbind(rankedexpenditure, rankedfoodcop[,-1], rankedincomesources[,-1], 
               rankedinfoneed[,-1], rankednfineeds[,-1], rankedpriorityneeds[,-1],
               rankededubarrier[,-1], rankedhealthbarrier[,-1], rankedwash[,-1])

PrioritiesRanking$Camp_name <-  unlist(PrioritiesRanking$Camp_name)


library(writexl)
write_xlsx(PrioritiesRanking, "~/REACH24919/IDP Camp Directory/2020/Datasets/Priorities/habibi.xlsx")
