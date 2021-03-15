rm(list=ls())
#install libraries 
# install.packages(c("data.table","dplyr","ggplot2"))
library(data.table)
library(tidyverse)
library(ggplot2)

#read input folder
indir <- "C:\\Users\\kkhatiwada\\Dropbox\\J2000_Savakhola\\Future_models\\45CanESM\\input\\local\\"

#Reading the data 
#############change the tmean to other variable based on the need 
tmean <- fread(paste(indir, "tmean.dat", sep= ""),skip = 16) 
#fixing the first column as the date
tmean$V1 <- as.Date(tmean$V1, "%d.%m.%Y")
tmean$month <- format(tmean$V1, "%m")
tmean$month <- as.numeric(tmean$month)
#selecting the required column
########if you have multiple column just chage the number of the column 
tmean <- tmean[,2:15]
#creating a new table for the correction 
month <- seq(from = 1, to= 12, by = 1 )
##########keep the correction factor of each month here. 
diff <- c (10, 10,10,10,10,20,20,20,20,20,30,30)
corMat <- data.frame(month, diff)

#correcting the data based on the correction factor 
tmean_new <- tmean %>% 
			left_join(corMat, by = "month") %>%
			mutate_each(funs(.+diff), starts_with("V"))
#selecting only the required column and rounding it to 2 digits. 
tmean_new2 <- tmean_new[,1:13]
tmean_new3 <- round(tmean_new2,2)

# adding the column of date in the data 
DateRow <- data.frame(Date=seq(as.Date("1981-01-01"),length.out = nrow(tmean_new3),by="day"))
DateRow$Date <- paste(as.character(DateRow$Date),"00:00")
temp_new4 <- cbind(DateRow, tmean_new3)

#saving the file with just the data 
##Sory folks, its just the pure data only. YOu can update the other rows or columns based on your requirement. 
write.csv(temp_new4, paste(indir, "tmeanNew.csv", sep = ""))

	