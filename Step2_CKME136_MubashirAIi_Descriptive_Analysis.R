
# ******************************************************************************* #
# ********  Program:  Data Analytics, Big Data and Predictive Analytics  ******** #
# ********  Course:   CKME136 - Data Analytics: Capstone Course          ******** #
# ********  Name: Mubashir Ali                                           ******** #
# ********  Student ID: 500932569                                        ******** #
# ********  Email: mian1.ali@ryerson.ca                                  ******** #
# ********  Section: Descriptive Analysis                                ******** #
# ******************************************************************************* #

str(data0708)
summary(data0708)

# ***** Yearly Delay ***** #

total_yArrDelay <- tapply(data0708$ArrDelay, data0708$Year, FUN = sum)
print(total_yArrDelay)
barplot(total_yArrDelay/1000, lwd = 2, col = "cyan4",
        main = "Total Yearly Arrival Delay", 
        xlab = "Year", ylab = "Total Arrival Delay Mins ('000)", ylim = c(0,170000))


total_yDepDelay <- tapply(data0708$DepDelay, data0708$Year, FUN = sum)
print(total_yDepDelay)
barplot(total_yDepDelay/1000, lwd = 2, col = "green3",
        main = "Total Yearly Departure Delay", 
        xlab = "Year", ylab = "Total Departure Delay Mins ('000)", ylim = c(0,170000))

total_yDelay <- total_yArrDelay + total_yDepDelay
print(total_yDelay)
barplot(total_yDelay/1000, lwd = 2, col = "aquamarine",
        main = "Total Yearly Delay",
        xlab = "Year", ylab = "Total Delay in Mins ('000)", ylim = c(0,170000))



# ***** Daily Delay ***** #

total_dArrDelay <- tapply(data0708$ArrDelay, data0708$DayofWeek, sum)
print(total_dArrDelay)
barplot(total_dArrDelay/1000, lwd = 2, col = "cyan4",
        main = "Total Daily Arrival Delay",
        xlab = "Week Days", ylab = "Total Arrival Delay in Min (000's)", ylim = c(0,60000))

total_dDepDelay <- tapply(data0708$DepDelay, data0708$DayofWeek, sum)
print(total_dDepDelay)
barplot(total_dDepDelay/1000, lwd = 2, col = "green3",
        main = "Total Daily Departure Delay",
        xlab = "Week Days", ylab = "Total Departure Delay in Min (000's)", ylim = c(0,60000))

total_dDelay <- total_dArrDelay + total_dDepDelay
print(total_dDelay)
barplot(total_dDelay/1000, lwd = 2, col = "aquamarine",
        main = "Total Daily Delay",
        xlab = "Week Days", ylab = "Total Delay in Min (000's)", ylim = c(0,60000))



# ***** Monthly Delay ***** #

total_mArrDelay <- tapply(data0708$ArrDelay, data0708$Month, sum)
print(total_mArrDelay)
barplot(total_mArrDelay/1000, lwd = 2, col = "cyan4",
        main = "Total Monthly Arrival Delay",
        xlab = "Months", ylab = "Total Arrival Delay in Min (000's)", ylim = c(0, 40000))

total_mDepDelay <- tapply(data0708$DepDelay, data0708$Month, sum)
print(total_mDepDelay)
barplot(total_mDepDelay/1000, lwd = 2, col = "green3",
        main = "Total Monthly Departure Delay",
        xlab = "Months", ylab = "Total Departure Delay in Min (000's)", ylim = c(0, 40000))

total_mDelay <- total_mArrDelay + total_mDepDelay
print(total_mDelay)
barplot(total_mDelay/1000, lwd = 2, col = "aquamarine",
        main = "Total Monthly Delay",
        xlab = "Months", ylab = "Total Delay in Min (000's)", ylim = c(0, 40000))



# ***** Delay by Carrier ***** #

total_ArrDelay_car <- (tail(sort(tapply(data0708$ArrDelay, data0708$Carrier, sum)),10))
print(total_ArrDelay_car)
barplot(total_ArrDelay_car/1000, lwd = 2, col = "cyan4",
        main = "Total Arrival Delay by Carrier",
        xlab = "Carrier", ylab = "Total Arrival Delay in Min (000's)", ylim = c(0,45000))


total_DepDelay_car <- (tail(sort(tapply(data0708$DepDelay, data0708$Carrier, sum)),10))
print(total_DepDelay_car)
barplot(total_DepDelay_car/1000, lwd = 2, col = "green3",
        main = "Total Departure Delay by Carrier",
        xlab = "Carrier", ylab = "Total Departure Delay in Mins (000's)", ylim = c(0,45000))


total_Delay_car <- (tail(sort(tapply(data0708$DepDelay, data0708$Carrier, sum) + 
                          tapply(data0708$ArrDelay, data0708$Carrier, sum)),10))
print(total_Delay_car)
barplot(total_Delay_car/1000, lwd = 2, col = "aquamarine",
        main = "Total Delay by Carrier",
        xlab = "Carrier", ylab = "Total Delay in Mins (000's)", ylim = c(0,45000))


# ------------------------------------------------------------------------------------------ #

# ***** Delay by Origin ***** #

total_orgArrDelay <- (tail(sort(tapply(data0708$ArrDelay, data0708$Origin, sum)),10))
print(total_orgArrDelay)
barplot(total_orgArrDelay/1000, lwd = 2, col = "cyan4",
        main = "Total Arrival Delay by Origin",
        xlab = "Carrier", ylab = "Total Arrival Delay in Min (000's)", ylim = c(0,15000))


total_DepDelay_car <- (tail(sort(tapply(data0708$DepDelay, data0708$Carrier, sum)),20))
print(total_Dep_Dly_car)
barplot(total_Dep_Dly_car/1000, lwd = 2, col = "blue",
        main = "Total Departure Delay by Carrier",
        xlab = "Carrier", ylab = "Total Departure Delay in 000 Min", ylim = c(0,45000))

total_Dly <- (tail(sort(tapply(Data0708$DepDelay, Data0708$Carrier, sum) + 
                          tapply(Data0708$ArrDelay, Data0708$Carrier, sum)),20))
print(total_Dly)
barplot(total_Dly/1000, lwd = 2, col = "lightblue",
        main = "Total Delay by Carrier",
        xlab = "Carrier", ylab = "Total Delay in 000 Min", ylim = c(0,45000))

# ------------------------------------------------------------------------------------------ #


# ***** Delays more than 15 Minutes ***** #

depDelay_15min <- subset(data0708, DepDelay >= 15, 
                        select = c(Carrier, Year, Dest, Month, DayofWeek))
arrDelay_15min <- subset(data0708, ArrDelay >= 15,
                        select = c(Carrier, Year, Dest, Month, DayofWeek))

library(plyr)

# ***** Per Year ***** #
depDelay_15min_yr <- count(depDelay_15min$Year)
print(depDelay_15min_yr)

arrDelay_15min_yr <- count(arrDelay_15min$Year)
print(arrDelay_15min_yr)

# ***** Per Day ***** #
depDelay_15min_dy <- count(depDelay_15min$DayofWeek)
print(depDelay_15min_dy)

arrDelay_15min_dy <- count(arrDelay_15min$DayofWeek)
print(arrDelay_15min_dy)

# ***** Per Month ***** #
depDelay_15min_month <- count(depDelay_15min$Month)
print(depDelay_15min_month)

arrDelay_15min_month <- count(arrDelay_15min$Month)
print(arrDelay_15min_month)

# ***** Per Carrier ***** #
depDelay_15min_car <- count(depDelay_15min$Carrier)
print(depDelay_15min_car)

arrDelay_15min_car <- count(arrDelay_15min$Carrier)
print(arrDelay_15min_car)

# ***** Per Destination ***** #
depDelay_15min_dest <- count(depDelay_15min$Dest)
print(depDelay_15min_dest)


# ***** Number of Flights ***** #

library(plyr)


# ***** Per Year ***** #
numflight_year <- count(data0708$Year)
print(numflight_year)
plot(numflight_year$x, numflight_year$freq/1000, col = "Red",
     main = "Number of Flights per Year", xlab = "Year", 
     ylab = "Frequency in '000", ylim = c(0, 7500))


# ***** Per Month ***** #
numflight_month <- count(data0708$Month)
print(numflight_month)
plot(numflight_month$x, numflight_month$freq/1000, col = "Red",
     main = "Number of Flights per Month", xlab = "Month", 
     ylab = "Frequency in '000", ylim = c(0, 1300))

total_mDelay_min <- (total_mDelay / numflight_month$freq) # ***** total min/flight delays per month ***** #
print(total_mDelay_min)


# ***** Per Day ***** #
numflight_day <- count(data0708$DayofWeek)
print(numflight_day)
plot(numflight_day$x, numflight_day$freq/1000, col = "Red",
     main = "Number of Flights per Day", xlab = "Weekday", 
     ylab = "Frequency in '000", ylim = c(0, 2200))

total_dDelay_min <- (total_dDelay / numflight_day$freq) # ***** total min/flight delays per weekday ***** #
print(total_dDelay_min)


# ***** Per Carrier ***** #
numflight_carrier <- count(data0708$Carrier)
print(numflight_carrier)
plot(numflight_carrier$x, numflight_carrier$freq/1000, col = "Red",
     main = "Number of Flights per Carrier", xlab = "Carrier", 
     ylab = "Frequency in '000")


total_Delay_car <- as.numeric(total_Delay_car)
total_Delaycar_min <- total_Delay_car / numflight_carrier$freq # ***** total min/flight delays per Carrier ***** #
str(total_Delay_car)
str(total_Delaycar_min)
print(total_Delay_car)
print(total_Delaycar_min)


# ***** No Delay ***** #
noDelay <- subset(data0708, ArrDelay == 0 & DepDelay == 0, select = c(Carrier, Year, Dest))
head(noDelay, n = 100)

noDelay_yr <- count(noDelay$Year)
print(noDelay_yr)

noDelay_car <- count(noDelay$Carrier)
print(noDelay_car)

noDelay_dest <- count(noDelay$Dest)
print(noDelay_dest)


# ***** Cancelled ***** #

cancelled_yr <- count(data0708$Year)
print(cancelled_yr)

cancelled_car <- count(data0708$Carrier)
str(cancelled_car)
cancelled_car_num = as.numeric(gsub(",", "", cancelled_car$freq))
str(cancelled_car_num)
plot(cancelled_car_num/1000 ~ cancelled_car$x, lwd = 2, col = "Red", 
     main = "Total Number of Cancelled Flights by Carrier",
     xlab = "Carrier", ylab = "Total Cancellations '000")


# ***** Diverted ***** #
diverted <- count(data0708$Diverted)
print(diverted)


# ***** Top Destinations ***** #
top_dest <- count(data0708$Dest)
print(top_dest)


# ***** Most Influential Delay Factor ***** #

sum(data0708$LateAircraftDelay)
sum(data0708$NASDelay)
sum(data0708$CarrierDelay)
sum(data0708$WeatherDelay)
sum(data0708$SecurityDelay)


# ***** Departure Time Analysis ***** #
deptime_view <- count(data0708$DepTime)
print(deptime_view)
deptime_view_num = as.numeric(gsub(",", "", deptime_view$freq))
plot(deptime_view_num/1000 ~ deptime_view$x, lwd = 2, col = "Red", 
     main = "Departure Time Analysis",
     xlab = "Departure Hour", ylab = "Total Number of Departures '000")

# ***** Arrival Time Analysis ***** #
arrtime_view <- count(data0708$ArrTime)
print(arrtime_view)
arrtime_view_num = as.numeric(gsub(",", "", arrtime_view$freq))
plot(arrtime_view_num/1000 ~ arrtime_view$x, lwd = 2, col = "Red", 
     main = "Arrival Time Analysis",
     xlab = "Arrival Hour", ylab = "Total Number of Arrivals '000")

# ***** Arrival and Departure Time (Delays) Analysis ***** #

arrtime_delay <- filteredD[(which(filteredD$ArrDelL=='1')),]
head(arrtime_delay)
arrtime_delayed <- count(arrtime_delay$ArrTime)
print(arrtime_delayed)

deptime_delayed <- count(arrtime_delay$DepTime)
print(deptime_delayed)



# ***** Departure Delay and Arrival delay phi coefficient ***** #

A <- count(which(filteredD$DepDelL==1 & filteredD$ArrDelL==1))
B <- count(which(filteredD$DepDelL==0 & filteredD$ArrDelL==0))
C <- count(which(filteredD$DepDelL==1 & filteredD$ArrDelL==0))
D <- count(which(filteredD$DepDelL==0 & filteredD$ArrDelL==1))

AA <- sum(A$freq)
BB <- sum(B$freq)
CC <- sum(C$freq)
DD <- sum(D$freq)

A1D1 <- AA/sum(AA, BB, CC, DD)
A0D1 <- CC/sum(AA, BB, CC, DD)
A1D0 <- DD/sum(AA, BB, CC, DD)
A0D0 <- BB/sum(AA, BB, CC, DD)

library(psych)

x <- matrix(c(AA, CC, DD, BB), ncol=2)
phi(x)
x

y <- matrix(c(A1D1, A0D1, A1D0, A0D0), ncol = 2)
y
phi(y)

plot(FilteredTest)
