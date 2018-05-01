data("AirPassengers") # load the dataset
ap<-AirPassengers



- Exploratory Analysis


sum(is.na(ap)) # Number of NAs present in the dataset

start(ap) # start of time series

end(ap)  # end of time series

frequency(ap) # Number of cycles per year

cycle(ap)    # Year wise cycles

summary(ap)  # Table Summary

# Add a regression line to the time series plot
plot(ap)+
  abline(reg = lm(AirPassengers~time(ap)))



# Plot the raw data
# Using ggfortify package extension of ggplot2

autoplot(ap) + labs(x ="Date", y = "Passenger numbers (1000's)",
                    title="Air Passengers from 1949 to 1961") 

# Use of boxplot to see seasonal changes

boxplot(ap~cycle(ap),xlab="Date", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")

# Check for stationary

temp<-log(AirPassengers)  # makes the variances equal

plot(temp)

stationaryAp<-diff(temp) # makes the mean equal

plot(stationaryAp)

# Hypothesis testing for stationary time series using Dickey fuller test

adf.test(stationaryAp)






- From these exploratory plots, we can make some initial inferences:
  
  - The passenger numbers increase over time with each year which may be indicative of an increasing linear trend, perhaps due to increasing demand for flight travel and commercialisation of airlines in that time period.
- In the boxplot there are more passengers travelling in months 6 to 9 with higher means and higher variances than the other months, indicating seasonality with a apparent cycle of 12 months. The rationale for this could be more people taking holidays and fly over the summer months in the US.
- AirPassengers appears to be multiplicative time series as the passenger numbers increases, it appears so does the pattern of seasonality.
- There do not appear to be any outliers and there are no missing values. Therefore no data cleaning is required.




# Decompose of time series can be of two types
#  Additive- x(t)=trend+seasonal+error
#  Multiplicative- x(t)=trend*seasonal+error

decomposeAp<-decompose(ap,"additive")

plot(decomposeAp)


# Number of differentiations required to make the series stationary and find 'd'
ndiffs(AirPassengers)  
acf(diff(AirPassengers))  # Auto correlation to find the 'p' value
pacf(diff(AirPassengers)) # partial auto correlation to find 'q' value

# From ndiffs,acf,pacf we get the values of of p,d,q as 0,1,1 respectively
# Model is created and trained using arima()
fit<-arima(x = AirPassengers,order = c(0,1,1),seasonal = list(order = c(0, 1, 1),period = 12))
plot(forecast(fit,12)) # Data is forecasted for a year

