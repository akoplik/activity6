library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

# Water security (developing sustainable and reliable access to clean water)
# Data: Satellite observations of evapotranspiration (cumulative amount of water that is evapoated per unit time)
  # Total amount of water lost to atmosphere from land surface

ETdat <- read.csv("ETdata.csv")

almonds <- ETdat %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET,na.rm=T))

ggplot(almonds, aes(x = ymd(date), y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(x = "year",
       y = "Monthly evapotranspiration (in)")

# Seasonality: low levels in winter, high in late spring/early summer

# Form data as a time series object (data ordered in equal, consistent intervals)

almond_ts <- ts(almonds$ET.in, # data
                start = c(2016, 1), # year 2016 (unit), month 1 (obs)
                frequency = 12) # freq of observations per unit

# Decomposing a time series:
  # y_t = Trend_t + seasonality_t * error_t

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

# Autocorrelation: correlation from one observation to ones closest

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

# Lags: one unit of time into the past

# Autoregressive models:
  # Considers previous values of y in the equations
  # First order AR model (one lag period, t - 1)
    # y_t = B0 + B1(y_(t-1)) + e_t
  # y_t = B0 + B1(y_(t-1)) +...+Bn(y_t-n) + e_t

# Often stick to 3-4 orders. How to select best order? 
# Partial ACF represents potential lag periods well

pacf.plot <- pacf(na.omit(almond_ts))
almond_y <- na.omit(almond_ts)

# ARIMA: Autoregressive Integrated Moving Average
  # Builds coefficients into AR model

# 1st order AR
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

# 4th order AR MODEL
model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

# AIC drppped a lot

# calculate fitted values
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almonds, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almonds$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
