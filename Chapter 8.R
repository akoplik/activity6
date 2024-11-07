# Data
  # ch4: Methane Flux 
  # co2: Carbone Dioxide Flux
    # Negative: Lake takes in more CO2 than emits
  # chlorophyllA: Measures how much photosynthizing organisms in water
  # DIP: Dissolved Inorganic Phosphorous
  # Runoff: total amount of water from percipitation that flows into drainage

# Tutorial
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

ghg <- read.csv("Deemer_GHG_Data.csv")

# 8.4.1: log transform 
ghg$log.ch4 <- log(ghg$ch4+1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

# 8.4.2: binary variables
unique(ghg$Region)

# Isolate boreal and tropical regions via binary variables
ghg$BorealV <-ifelse(ghg$Region == "Boreal", 1, 0)
ghg$TropicalV <-ifelse(ghg$Region == "Tropical", 1, 0)

# Check if alpine or hydropower
ghg$AlpineV <-ifelse(ghg$Alpine == "yes", 1, 0)
ghg$HydroV <-ifelse(ghg$hydropower == "yes", 1, 0)

# 8.4.3: Multiple Regression
# multiple regression
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg)
summary(mod.full)

# 8.4.4 Cechk assumptions (linearity, normally distributed, equal variance, independent, no multicollinearity)
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# 8.4.4.1 Check Normality

qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
shapiro.test(res.full)
  # Data mainly follows line, meaning residuals are mostly normal

# 8.4.4.2 Residuals
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)
  # Residuals aren't in any major violation

# 8.4.5 Multicollinearity
# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)
# Generally, Correlation > ~0.7 is too correlated. None here that far

# 8.5: Model Selection
  # Process of eliminating variables and paramters that are not critical to objective of analysis and model outcomes
full.step <- ols_step_forward_aic(mod.full)
full.step
full.step$model
plot(full.step )

mod_2 <- lm(log.ch4 ~ .,
            data = ghg[11:29])

full.step2 <- ols_step_forward_aic(mod_2)
full.step2
full.step2$model
plot(full.step2 )

# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

# look at prediction with 95% confidence interval of the mean

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")
