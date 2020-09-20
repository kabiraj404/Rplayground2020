
##in this code I have made a simple approach of repeating the data that is present in certain date until there is next date with the data. 

rm(list = ls())
library(zoo)
library(dplyr)
library(tidyr)

#creating daily time series
suru <- as.Date("2002-01-01")
antya <- as.Date("2018-12-31")
dainikMiti <- data.frame(date = seq(suru, antya, "days"))

#reading the input modis data with the actual date
inputModis <- read.csv("D:\\OneDrive\\WORKING_Doc\\modis_snow_cover_area_with_8day_gap.csv", header = T)

#conforming with the required column only
chaineycolumnMatrai <- inputModis[,c(3,4)]

#keeping the same names for the date 
names(chaineycolumnMatrai) <- c("modis", "date")
#converting to the date format
chaineycolumnMatrai$date <- as.Date(chaineycolumnMatrai$date)

#merging the date with the actual data based on date 
YeautaTable <- left_join(dainikMiti, chaineycolumnMatrai, by = "date")
#View(YeautaTable)

#filling the NA with the earlier data ##Direction in which to fill missing values. Currently "down" (the default)
FilledDaysM <- YeautaTable %>% fill(modis)

#saving the file
write.csv(FilledDaysM, "regularModis.csv")



