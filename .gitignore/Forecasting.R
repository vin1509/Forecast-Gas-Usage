#Set Java Home
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_181')
library(dplyr)
library(rJava)
library(openxlsx)
library(ggplot2)
library(zoo)
library(readxl)
library(corrplot)
library(reshape)
library(lubridate)
library('forecast')
library('tseries')
library(lmtest)


weather_tmp <- read_xlsx('Weather Data for Drexel 9_28_2018.xlsx', sheet = 1)
weather_tmp <- weather_tmp[-c(36:37)]
weather_tmp <- weather_tmp %>% filter(Dt > as.POSIXct("2016-09-30") & Dt <  as.POSIXct("2018-09-30"))
weather_tmp$Dt <- (format(weather_tmp$Dt, format = "%m/%d/%Y"))
weather_tmp <- weather_tmp[!is.na(weather_tmp$Avg),]

weather_tmp$monthYear  <- format(as.Date(weather_tmp$Dt, "%m/%d/%Y"), "%m/%Y")

avgtemp_month <- weather_tmp %>% group_by(monthYear) %>% summarise(avg_temp = mean(Avg)) %>%
  arrange(monthYear)

avgtemp_month$monthYear <- factor(avgtemp_month$monthYear,
                        levels = c("10/2016","11/2016","12/2016","01/2017","02/2017","03/2017",
                            "04/2017","05/2017","06/2017","07/2017","08/2017","09/2017","10/2017",
                            "11/2017","12/2017","01/2018","02/2018","03/2018","04/2018","05/2018",
                            "06/2018","07/2018","08/2018","09/2018"))
ggplot( data = avgtemp_month, aes( monthYear, avg_temp, group =1 )) +geom_line(col = "steelblue", size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Temperature for all months" ,x ="month of the Year", y="Average Temperature")

#Winter Months
winter_tmp <- weather_tmp %>% filter(monthYear %in% c("10/2016","11/2016","12/2016","01/2017","02/2017","03/2017",
                                      "04/2017","10/2017","11/2017","12/2017","01/2018","02/2018","03/2018","04/2018"))


weather_hum <- read_xlsx('Weather_humidity.xlsx', sheet = 1)
weather_hum <- weather_hum %>% filter(Dt > as.POSIXct("2016-09-30") & Dt <  as.POSIXct("2018-09-30"))
weather_hum$Dt <- (format(weather_hum$Dt, format = "%m/%d/%Y"))
weather_hum$monthYear  <- format(as.Date(weather_hum$Dt, "%m/%d/%Y"), "%m/%Y")

winter_hum <- weather_hum %>% filter(monthYear %in% c("10/2016","11/2016","12/2016","01/2017","02/2017","03/2017",
                                                      "04/2017","10/2017","11/2017","12/2017","01/2018","02/2018","03/2018","04/2018"))

weather_cc <- read_xlsx('Weather_cc.xlsx', sheet = 1)
weather_cc <- weather_cc %>% filter(Dt > as.POSIXct("2016-09-30") & Dt <  as.POSIXct("2018-09-30"))
weather_cc$Dt <- (format(weather_cc$Dt, format = "%m/%d/%Y"))
weather_cc$monthYear  <- format(as.Date(weather_cc$Dt, "%m/%d/%Y"), "%m/%Y")
weather_cc <- weather_cc %>% filter(monthYear %in% c("10/2016","11/2016","12/2016","01/2017","02/2017","03/2017",
                                                      "04/2017","10/2017","11/2017","12/2017","01/2018","02/2018","03/2018","04/2018"))


weather_Wsp <- read_xlsx('weather_wsp.xlsx')
weather_Wsp <- weather_Wsp %>% filter(Dt > as.POSIXct("2016-09-30") & Dt <  as.POSIXct("2018-09-30"))
weather_Wsp$Dt <- (format(weather_Wsp$Dt, format = "%m/%d/%Y"))
weather_Wsp$monthYear  <- format(as.Date(weather_Wsp$Dt, "%m/%d/%Y"), "%m/%Y")

weather_Wsp <- weather_Wsp %>% filter(monthYear %in% c("10/2016","11/2016","12/2016","01/2017","02/2017","03/2017",
                                                      "04/2017","10/2017","11/2017","12/2017","01/2018","02/2018","03/2018","04/2018"))

#Read daily data
account <- read_xlsx('PECO Zip Customer 2018.10.01 v2.xlsx',  sheet = 1)
#account <- account %>% filter(FUELTYPE == "GAS")
#Residential customers
residential <- account %>% filter(REVENUCODE %in% c('01','12'))
#industrial
commercial <- account %>% filter(REVENUCODE %in% c('03','05'))
#No information
otherUsers <-  account %>% filter(is.na(REVENUCODE))
#filter for gas usage
filter_CCF <- function(df){
  df$monthYear  <- format(as.Date(df$METERREADDATE, "%m/%d/%Y"), "%m/%Y")
  df$AVG_GAS_USAGE <- rowMeans(df[,c(5:29)])
  df$DAILY_GAS_USAGE <- rowSums(df[,c(5:29)])
  df <- df[df$UOM == "CCF",]
  df <- df %>% select(-starts_with("INTERVAL"))
 # df <- df %>% filter(DMETERNO %in% residential$DMETERNO) %>% select(-DACCOUNTID)
}


##Analyze for winter months
hourly_2016.10 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2016.10.xlsx',  sheet = 1)) 
hourly_2016.11 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2016.11.xlsx',  sheet = 1))
hourly_2016.12 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2016.12.xlsx',  sheet = 1))
#No data from above 3 months in 2016

#2017 data - use as training set for forecasting 2018
hourly_2017.01 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.01.xlsx',  sheet = 1))
hourly_2017.02 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.02.xlsx',  sheet = 1))
hourly_2017.02 <- hourly_2017.02 %>% select(-DAILY_INTERVAL_USAGE)
hourly_2017.03 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.03.xlsx',  sheet = 1))
hourly_2017.03 <- hourly_2017.03 %>% select(-c(Min_Usage, Avg_Usage, Max_Usage))
hourly_2017.04 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.04.xlsx',  sheet = 1))
hourly_2017.04 <- hourly_2017.04 %>% select(-c(Min_Usage, Avg_Usage, Max_Usage))
hourly_2017.05 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.05.xlsx',  sheet = 1))
hourly_2017.06 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.06.xlsx',  sheet = 1))
hourly_2017.07 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.07.xlsx',  sheet = 1))
hourly_2017.08 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.08.xlsx',  sheet = 1))
hourly_2017.09 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.09.xlsx',  sheet = 1))
hourly_2017.10 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.10.xlsx',  sheet = 1))
hourly_2017.11 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.11.xlsx',  sheet = 1))
hourly_2017.12 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2017.12.xlsx',  sheet = 1))
####
data_2017 <- rbind(hourly_2017.01, hourly_2017.02, hourly_2017.03,hourly_2017.04,
                    hourly_2017.05, hourly_2017.06, hourly_2017.07, hourly_2017.08,
                    hourly_2017.09, hourly_2017.10, hourly_2017.11, hourly_2017.12)

# unique_customers <- unique(data_2017[c('DACCOUNTID', 'DMETERNO')])

#2018 data
hourly_2018.01 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.01.xlsx',  sheet = 1))
hourly_2018.02 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.02.xlsx',  sheet = 1))
hourly_2018.03 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.03.xlsx',  sheet = 1))
hourly_2018.04 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.04.xlsx',  sheet = 1))
hourly_2018.05 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.05.xlsx',  sheet = 1))
hourly_2018.06 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.06.xlsx',  sheet = 1))
hourly_2018.07 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.07.xlsx',  sheet = 1))
hourly_2018.08 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.08.xlsx',  sheet = 1))
hourly_2018.09 <- filter_CCF(read.xlsx('PECO Zip HourlyUsage_2018.09.xlsx',  sheet = 1))

data_2018 <- rbind(hourly_2018.01,hourly_2018.02,hourly_2018.03,hourly_2018.04,hourly_2018.05,
                   hourly_2018.06, hourly_2018.07, hourly_2018.08, hourly_2018.09)
unique_customers_2018 <- unique(data_2018[c('DACCOUNTID', 'DMETERNO')])


#Prepare the training set
#Model1 - all months
users_2017 <- data_2017[(unique_customers_2018$DACCOUNTID %in% data_2017$DACCOUNTID &
                                  unique_customers_2018$DMETERNO %in% data_2017$DMETERNO),]
final_customers <- unique(users_2017[c('DACCOUNTID', 'DMETERNO')])
#Testing set
data_2018 <- data_2018[(final_customers$DACCOUNTID %in% data_2018$DACCOUNTID &
                          final_customers$DMETERNO %in% data_2018$DMETERNO),]

#Model2 - Nov, Dec, Jan, Feb,March
#Model3 - April, May,Jun,July,Aug,Sep
#Model4 - OCtober

#Daily data
daily_data <- users_2017 %>% group_by(METERREADDATE) %>% summarise(DAILY_USAGE = sum(DAILY_GAS_USAGE))
daily_data$METERREADDATE <- mdy(daily_data$METERREADDATE)
ggplot(daily_data, aes(METERREADDATE, DAILY_USAGE, group = 1)) + geom_line() +
  ylab("Daily Gas Usage") +   xlab("")

#With smoothening curve
ggplot(daily_data, aes(METERREADDATE, DAILY_USAGE, group = 1)) + geom_line() +stat_smooth(colour = "green")+
  ylab("Daily Gas Usage") +   xlab("")


testing_data <- data_2018 %>% group_by(METERREADDATE) %>% summarise(DAILY_USAGE = sum(DAILY_GAS_USAGE))
testing_data$METERREADDATE <- mdy(testing_data$METERREADDATE)
#With smoothening curve
ggplot(testing_data, aes(METERREADDATE, DAILY_USAGE, group = 1)) + geom_line() +stat_smooth(colour = "green")+
  ylab("Daily Gas Usage") +   xlab("")


#Moving Average to get a smoother curve
daily_data$cnt_ma = ma(daily_data$DAILY_USAGE, order=7) 
daily_data$cnt_ma30 = ma(daily_data$DAILY_USAGE, order=30)

ggplot() +
  geom_line(data = daily_data, aes(x = METERREADDATE, y = DAILY_USAGE, colour = "Usage")) +
  geom_line(data = daily_data, aes(x = METERREADDATE, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = METERREADDATE, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Gas Usage')


#modeling daily level data might require specifying multiple seasonality levels,
#such day of the week, week of the year, month of the year, holidays, etc.
#For the sake of simplicity, we will model the smoothed series of weekly moving average

# First, we calculate seasonal component of the data using stl(). 
# STL is a flexible function for decomposing and forecasting the series.
# It calculates the seasonal component of the series using smoothing,
# and adjusts the original series by subtracting seasonality in two simple lines:
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=52)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#Test if series is stationary
#Null Hypothesis - series is not stationary
adf.test(count_ma, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  count_ma
# Dickey-Fuller = 0.047295, Lag order = 7, p-value = 0.99
# alternative hypothesis: stationary
#p-value is 0.99 we do not reject the null hypothesis, confirming our visual inspection

Acf(count_ma, main='')
Pacf(count_ma, main='')

#Start with d=1 and re-evaluate whether further differencing is needed
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  count_d1
# Dickey-Fuller = -6.8256, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

#Rejects the null hypothesis of non-stationary

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#Fit ARIMA Model
fit1 <- auto.arima(deseasonal_cnt, seasonal=FALSE)
fit1
# ARIMA(5,1,2) 
# 
# Coefficients:
#   ar1     ar2      ar3     ar4      ar5     ma1      ma2
# 0.6756  0.5542  -0.4915  0.1946  -0.2352  0.0934  -0.6361
# s.e.  0.1151  0.0899   0.0996  0.0682   0.0607  0.1122   0.1053
# 
# sigma^2 estimated as 2929069:  log likelihood=-3170.54
# AIC=6357.09   AICc=6357.5   BIC=6388.13
tsdisplay(residuals(fit1), lag.max=30, main='(5,1,2) Model Residuals')
# There is a clear pattern present in ACF/PACF and model residuals plots repeating at lag 7. 
# This suggests that our model may be better off with a different specification, 
# such as p = 7 or q = 7. 

fit2 = arima(deseasonal_cnt, order=c(5,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=45, main='(5,1,7) Model Residuals')
# arima(x = deseasonal_cnt, order = c(5, 1, 7))
# 
# Coefficients:
#   ar1     ar2      ar3     ar4      ar5      ma1      ma2     ma3      ma4     ma5      ma6
# 0.6742  0.0359  -0.3485  0.3181  -0.1934  -0.0305  -0.0353  0.3326  -0.0841  0.2039  -0.0371
# s.e.  0.1174  0.1387   0.1496  0.1054   0.1098   0.1097   0.1242  0.1102   0.0991  0.0967   0.0802
# ma7
# -0.538
# s.e.   0.068
# 
# sigma^2 estimated as 2500009:  log likelihood = -3147.19,  aic = 6320.38

fit3 = arima(deseasonal_cnt, order=c(6,1,7))
fit3
tsdisplay(residuals(fit3), lag.max=45, main='(6,1,7) Model Residuals')
# arima(x = deseasonal_cnt, order = c(6, 1, 7))
# 
# Coefficients:
#   ar1      ar2      ar3     ar4      ar5     ar6      ma1     ma2     ma3      ma4     ma5
# 0.8321  -0.1639  -0.1904  0.3491  -0.3953  0.2529  -0.1895  0.0569  0.2567  -0.1475  0.2941
# s.e.  0.0949   0.1248   0.1209  0.1040   0.1105  0.0818   0.0835  0.0847  0.0808   0.0751  0.0658
# ma6     ma7
# -0.1623  -0.606
# s.e.   0.0626   0.053
# 
# sigma^2 estimated as 2406843:  log likelihood = -3143.05,  aic = 6314.11
coeftest(fit3)
#Forecast
fcast <- forecast(fit3, h=39)
plot(fcast)

hold <- window(ts(deseasonal_cnt), start=330)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(330:359)]), order=c(6,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=29)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))


#Seasonal data
my_ts = ts(na.omit(daily_data$cnt_ma), frequency=52)
plot(my_ts)
adf.test(my_ts)

Acf(my_ts)
Pacf(my_ts)
dfit1 <- auto.arima(my_ts, seasonal = TRUE)
plot(residuals(dfit1))
Acf(residuals(dfit1))
Pacf(residuals(dfit1))
dfit1
coeftest(dfit1)

hold <- window(ts(my_ts), start=330)

fit_no_holdout = arima(ts(my_ts[-c(330:359)]), order =c(2,1,2), seasonal = list(order = c(1,0,0), period = 52))

fcast_no_holdout <- forecast(fit_no_holdout,h=29)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))


plot(seas_fcast)


##Monthly Non-seasonal data
count_ma30 = ts(na.omit(daily_data$cnt_ma30), frequency=12)
decomp = stl(count_ma30, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#Test if series is stationary
#Null Hypothesis - series is not stationary
adf.test(count_ma30, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  count_ma
# Dickey-Fuller = 0.047295, Lag order = 7, p-value = 0.99
# alternative hypothesis: stationary
#p-value is 0.99 we do not reject the null hypothesis, confirming our visual inspection

Acf(count_ma30, main='')
Pacf(count_ma30, main='')

#Start with d=1 and re-evaluate whether further differencing is needed
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  count_d1
# Dickey-Fuller = -6.8256, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

#Rejects the null hypothesis of non-stationary

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#Fit ARIMA Model
fit1 <- auto.arima(deseasonal_cnt, seasonal=FALSE)
fit1
# ARIMA(4,2,3) 
# 
# Coefficients:
#   ar1     ar2      ar3      ar4     ma1      ma2      ma3
# -0.0505  0.3334  -0.0520  -0.0110  0.7874  -0.8723  -0.7364
# s.e.   0.1532  0.1336   0.0815   0.0758  0.1424   0.0678   0.1251
# 
# sigma^2 estimated as 48483:  log likelihood=-2266.83
# AIC=4549.67   AICc=4550.11   BIC=4580.13
tsdisplay(residuals(fit1), lag.max=30, main='(4,2,3) Model Residuals')
# There is a clear pattern present in ACF/PACF and model residuals plots repeating at lag 7. 
# This suggests that our model may be better off with a different specification, 
# such as p = 7 or q = 7. 

coeftest(fit1)
#Forecast
fcast <- forecast(fit1, h=9)
plot(fcast)

hold <- window(ts(deseasonal_cnt), start=300)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(300:359)]), order=c(4,2,3))

fcast_no_holdout <- forecast(fit_no_holdout,h=59)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

f_values <- forecast(fit1, h=9)
plot(f_values)
