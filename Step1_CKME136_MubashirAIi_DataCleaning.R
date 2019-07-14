
# ********  Program:  Data Analytics, Big Data and Predictive Analytics  ********
# ********  Course:   CKME136 - Data Analytics: Capstone Course          ********
# ********  Name: Mubashir Ali                                           ********
# ********  Student ID: 500932569                                        ********
# ********  Email: mian1.ali@ryerson.ca                                  ********
# ********  Section: Data Cleaning                                       ********

# ***** Import all records that happened in 2007 ***** #

data07 <- read.csv("C:/Users/Mubashir/Desktop/2007.csv", sep = ",", header = TRUE, 
          col.names = c("Year", "Month", "DayofMonth", "DayofWeek", "DepTime", "CRSDepTime", "ArrTime", "CRSArrTime", 
          "Carrier", "FlightNo", "TailNum", "ActualElapsedTime", "CRSElapsedTime","AirTime", "ArrDelay", "DepDelay", 
          "Origin", "Dest", "Distance", "TaxiIn", "TaxiOut", "Cancelled", "CancellationCode", "Diverted", 
          "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay"))


# ***** Import all records that happened in 2008 ***** #

data08 <- read.csv("C:/Users/Mubashir/Desktop/2008.csv", sep = ",", header = TRUE, 
          col.names = c("Year", "Month", "DayofMonth", "DayofWeek", "DepTime", "CRSDepTime", "ArrTime", "CRSArrTime", 
          "Carrier", "FlightNo", "TailNum", "ActualElapsedTime", "CRSElapsedTime","AirTime", "ArrDelay", "DepDelay", 
          "Origin", "Dest", "Distance", "TaxiIn", "TaxiOut", "Cancelled", "CancellationCode", "Diverted", 
          "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay"))


# ***** Concatenate both imported datasets data07 and data08 ***** #

totaldata <- rbind(data07, data08)
str(totaldata)

filtered.f <- totaldata[, !(colnames(totaldata) %in% 
                            c("CRSDepTime", "CRSArrTime", "FlightNo", "ActualElapsedTime", "CRSElapsedTime", 
                              "AirTime", "TaxiIn", "TaxiOut", "Cancelled", "CancellationCode", "Diverted"))]  
                               
filtered.f[c("CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", 
             "LateAircraftDelay")][is.na(filtered.f[c("CarrierDelay", "WeatherDelay", 
             "NASDelay", "SecurityDelay", "LateAircraftDelay")])] <- 0

summary(filtered.f)

data0708 <- na.omit(filtered.f)


# ***** Data Inspection ***** #
str(data0708)
summary(data0708)
colSums(data0708==0)


# ***** Data Conversion ***** #

data0708$Year = as.factor(data0708$Year)

data0708$Month = as.factor(x = data0708$Month)
levels(data0708$Month) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


data0708$DayofMonth = as.factor(data0708$DayofMonth)

data0708$DayofWeek = as.factor(data0708$DayofWeek)
levels(data0708$DayofWeek) = c("Monday", "Tuesday", "Wednesday", 
                               "Thursday", "Friday", "Saturday", "Sunday")

data0708$DepTime = floor(data0708$DepTime/100)
data0708$DepTime = as.factor(data0708$DepTime)

data0708$ArrTime = floor(data0708$ArrTime/100)
data0708$ArrTime = as.factor(data0708$ArrTime)

data0708$ArrDelay = as.numeric(gsub(",", "", data0708$ArrDelay))
data0708$DepDelay = as.numeric(gsub(",", "", data0708$DepDelay))
data0708$Distance = as.numeric(gsub(",", "", data0708$Distance))

data0708$CarrierDelay = as.numeric(gsub(",", "", data0708$CarrierDelay))
data0708$WeatherDelay = as.numeric(gsub(",", "", data0708$WeatherDelay))
data0708$NASDelay = as.numeric(gsub(",", "", data0708$NASDelay))
data0708$SecurityDelay = as.numeric(gsub(",", "", data0708$SecurityDelay))
data0708$LateAircraftDelay = as.numeric(gsub(",", "", data0708$LateAircraftDelay))

# ********* Reclassify ArrivalDelay and Departure Delay to 0 and 1 values ********* #
library(dplyr)
data0708L <- mutate(data0708, DepDelL = ifelse(DepDelay>0, 1, 0),  
                              ArrDelL = ifelse(ArrDelay>0, 1, 0),
                              LateAircraft = ifelse(LateAircraftDelay>0, 1, 0))


filteredD <- data0708L[, !(colnames(data0708L) %in% 
                           c("Year", "DayofMonth", "TailNum", "ArrDelay", "DepDelay", "Cancelled", "Diverted", 
                             "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay"))]

filteredD <- filteredD[filteredD$DepTime != '0' & filteredD$ArrTime != '0',]

# ***** Data Inspection ***** #
str(data0708)
