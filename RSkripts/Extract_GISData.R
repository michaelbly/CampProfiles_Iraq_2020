###########################
#Import GIS Data
##########################
GISData<-read_excel("dataset/GISData.xlsx", sheet=1)
names(GISData)
subset <- c("Camp", "AREA (M2)", "Individual", "HHs", "Latitude", "Longitude", "Average open area per HH", "Average covered area per person", "Number of ppl per HHs", "Average nb individuals by shelter")
GISData <- GISData[subset]
