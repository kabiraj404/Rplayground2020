### Details

#This is document is for repeating the monthly average for the lenth of the time. This will be useful when we have the montly data and have to repeat the same value for the whole month and even for the future scenarios eg:upto 2100

rm(list = ls())
library(zoo)

#Setting up of the time range of the table we need 
sd = as.Date("1981-01-01")
ed = as.Date("2100-12-31")

#Create daily and monthly data frame
daily.df   = data.frame(date = seq(sd, ed, "days"))
monthly.df = data.frame(date = seq(sd, ed, "months"))


#I have here prodided 1 to 12 for each month. This is the location where the user can define the monthly value.
#eg: humidity
rhum <- c(2.53, 2.42, 2.86, 3.14, 4.42, 5.31, 5.14, 4.14, 3.42, 2.89, 2.19, 2.28)

ntimes <- nrow(monthly.df)/12

#Now keeping the value for each month 
monthly.df$value = rep(rhum, times = ntimes)

#Now merging the two data frames 
df <- merge(daily.df, monthly.df, by = "date", all = TRUE)

#Now filling the NA's of the table with the data of that month
df <- transform(df, value = na.locf(value))

#now Saving the file 
write.csv(df, "futuredata5.csv")
