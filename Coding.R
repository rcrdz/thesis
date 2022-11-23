library(forecast)
library(fpp2)
library(dplyr)
library(magrittr)
library(xts)
library(tseries)
library(astsa)
library(tidyverse)
library(car)
library(feasts)

source("GrangerTests.R")
source("ConditionalGrangerCausality.R")


###################
### Import data ###
###################
data_raw <- read.csv(file = 'working_db_Trafo4_Daily_Lagged.csv')
dim(data_raw)
head(data_raw)

# Looking at variable names and rename wrongly named Date
names(data_raw)
#names(data_raw)[1] <- 'Date' # Not necessary anymore i guess..


##############################
### Converting to .ts-data ###
##############################
# Converting Date from character representation to class 'Date'
#data_conv <- data_raw
#data_conv %<>%
#  mutate(Date = as.Date(Date, format= "%d.%m.%y"))

# Converting to .ts-data # yearly seasonality
#DA_Price_DE.ts <- ts(data_conv$DA_Price_DE, start = c(as.numeric(format(data_conv$Date[1], "%Y")), as.numeric(format(data_conv$Date[1], "%j"))), frequency = 365)


###############################
### Converting to .xts-data ###
###############################
data_raw.xts <- xts(data_raw[,-1], order.by = as.Date(data_raw[,1], "%d.%m.%y"))


#####################
### Cleaning data ###
#####################
# Remove Time-variables ('DayofWeek' etc.)
data.xts <- data_raw.xts[,! names(data_raw.xts) %in% c("DayofWeek", "Is_Weekday", "Seasons", "Holiday", "Year")]
# Remove 'ShareOf's'
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("shareOf", names(data.xts))])]


################
### Plotting ###
################
# a few plots..
# Plotting: https://rpubs.com/odenipinedo/visualizing-time-series-data-in-R
# Example: DK_2_P_spread_to_DE
plot(data.xts$DK_2_P_spread_to_DE, main = "DK_2_P_spread_to_DE")

# Plot two charts on same graphical window
#par(mfrow = c(2,1))
#plot(data$lignite_shareOf_production, main = "lignite_shareOf_production")
#plot(data$DA_Price_DE, main = "DA_Price_DE")

# Example: Plot two charts in same plot
plot(data.xts$actual_load, main = "Actual load + Forecast load")
lines(data.xts$forecast_load, col = "red")

# Example: Plot with Legend
plot(data.xts[,4:6], main = "Forecast Solar, Wind On- and Offshore Generation", col = c("black", "tomato", "blue"))
addLegend(legend.loc = "topleft",
          legend.names = c(names(data.xts)[4], names(data.xts)[5], names(data.xts)[6]),
          #col = c("black", "tomato", "blue"),
          lty=1, lwd=1)

# Netherlands export, import
plot(data.xts[,60:61], main = "Netherlands export, import")
addLegend(legend.loc = "topleft",
          legend.names = c(names(data.xts$Netherlands_export), names(data.xts$Netherlands_import)),
          lty=1, lwd=1)

# Plot of Power productions
plot(data.xts[,27:35], main = "Power production from different sources")


########################
### Data Exploration ###
########################
# Variables with negative values?
#data_without_date <- data[ , !(names(data) %in% "Date")]
#vals_greater_zero <- data_without_date %>%
#  gather(var, val) %>%
#  group_by(var) %>%
#  summarise(greater_zero = all(val[!is.na(val)] >= 0))
# Number of variables with only positive values: 63
#sum(vals_greater_zero$greater_zero)
# # of variables with also negative values: 18
#dim(vals_greater_zero)[1]-sum(vals_greater_zero$greater_zero)
# Variables with also negative values:
#vals_greater_zero$var[vals_greater_zero$greater_zero == FALSE]

# It is also way easier to get the non-negatives than above
non_negatives.xts = data.xts[,colSums(data.xts<0)==0]
names(non_negatives.xts)
# variables with also negative values
negatives.xts <- data.xts[,! names(data.xts) %in% names(non_negatives.xts)]
names(negatives.xts)

# Which variables start later in time?
# Example: physical_net_export
var_bool <- data.xts$physical_net_export != 0
time_index <- min(which(var_bool == TRUE))
plot(data.xts$physical_net_export)
timepoint <- time(data.xts$physical_net_export)[time_index]
addEventLines(events = xts(x = '', order.by = timepoint), lty = 2, col = 'tomato', lwd = 1.5)

# List: When is first nonzero entry
first_nonzero <- data.frame(matrix(ncol = 1, nrow = dim(data.xts)[2]))
colnames(first_nonzero) <- "First nonzero index"
rownames(first_nonzero) <- colnames(data.xts)
for (i in 1:dim(first_nonzero)[1]) {
  bool <- data.xts[,i] != 0
  first_nonzero[i,1] <- min(which(bool == TRUE))
}
# Number of variables which "start not at first date"
sum(first_nonzero != 1) #14
# Names of variables which "start not at first date"
rownames(first_nonzero)[first_nonzero$`First nonzero index` != 1]


# Zero-inflated data
zeros_count <- data.frame(matrix(ncol = 1, nrow = dim(data.xts)[2]))
rownames(zeros_count) <- colnames(data.xts)
colnames(zeros_count) <- "# of Zeros"
for (i in 1:dim(zeros_count)[1]) {
  #zeros_count[1,i] <- sum(data.xts[,i]==0)
  zeros_count[i,1] <- sum(data.xts[first_nonzero[i,]:dim(data.xts)[1],i]==0)
}
# Zero-inflated variables
zero_infl_vars <- subset(zeros_count, `# of Zeros` != 0)
rownames(zero_infl_vars)
# Non-Zero-inflated variables
non_zero_infl_vars <- subset(zeros_count, `# of Zeros` == 0)
rownames(non_zero_infl_vars)

# Histograms of zero-inflated variables (removed zeros before "start")
for (i in 1:dim(zero_infl_vars)[1]) {
  hist(data.xts[first_nonzero[rownames(zero_infl_vars)[i],]:dim(data.xts)[1], rownames(zero_infl_vars)[i]], xlab = rownames(zero_infl_vars)[i], main = paste("Histogram of ", rownames(zero_infl_vars)[i]), probability = TRUE)
}

# Histograms of "non-zero-inflated" variables (removed zeros before "start")
for (i in 1:dim(non_zero_infl_vars)[1]) {
  hist(data.xts[first_nonzero[rownames(non_zero_infl_vars)[i],]:dim(data.xts)[1], rownames(non_zero_infl_vars)[i]], xlab = rownames(non_zero_infl_vars)[i], main = paste("Histogram of ", rownames(non_zero_infl_vars)[i]), probability = TRUE)
}

# Which columns are zero-inflated? #Different approach than above
#zero_inf <- data %>% 
#  select_if(function(col) length(which(col==0)) > 7)
#names(zero_inf)
#zero_inf <- select(zero_inf, -c(DayofWeek, Is_Weekday, Holiday))
#zero_inf <- cbind(data[,1], zero_inf)
#zero_inf.xts <- xts(zero_inf[,-1], order.by = as.Date(zero_inf[,1], "%d.%m.%y"))
#colSums(zero_inf.xts==0) # # of zeros in each column


# Covariance matrix (with lags)
nl <- 2 # number of lags
data_with_lags <- embed(as.matrix(data.xts), nl+1) #produce a matrix with M columns containing the original series and lagged versions of it
names <- colnames(data.xts)
ndfs <- paste(rep(names,nl), "[t-", rep(1:nl, each=ncol(data.xts)), "]", sep = "")
colnames(data_with_lags) <- c(names, ndfs)
cov_matrix_with_lags <- cov(data_with_lags)
corr_matrix_with_lags <- cov2cor(cov_matrix_with_lags) # Correlation matrix
which(corr_matrix_with_lags > 0.9, arr.ind = T) # which entries are >0.7?
#corr_matrix_with_lags[which(corr_matrix_with_lags <0.9)] <- 0
# auxiliary matrix to get variables with highest correlation
auxiliary_matrix1 <- corr_matrix_with_lags
auxiliary_matrix1[which(auxiliary_matrix1 == 1)] <- 0
s <- which(auxiliary_matrix1 == max(auxiliary_matrix1), arr.ind = TRUE) # index of maximal value in matrix
rownames(corr_matrix_with_lags)[s[1]]
colnames(corr_matrix_with_lags)[s[2]] #NG_storage is highly correlated to its 1st lag


# Correlation matrix (without lags)
corr_matrix_no_lags <- cor(data.xts)
corr_matrix_no_lags[which(corr_matrix_no_lags == 1)] <- 0
s2 <- which(corr_matrix_no_lags == max(corr_matrix_no_lags), arr.ind = TRUE)
# find the 10 largest values
x <- which(corr_matrix_no_lags >= sort(corr_matrix_no_lags, decreasing = T)[10], arr.ind = T)
# determine the order of the 10 largest values in decreasing order
x.order <- order(corr_matrix_no_lags[x], decreasing = T)
x[x.order, ]
# Plot the ones with highest correlation
plot(data.xts$NG_CleanPrice, main = "NG_CleanPrice and NG_TTF") # example of 2 correlated time series
lines(data.xts$NG_TTF, col = "red") # Plot of the highly correlated variables
# It turns out that (obviously) NG_CleanPrice and NG_TTF is highly correlated

# Plot of another two highly correlated time series (solar_production and GHI)
plot(data.xts$solar_production, main = "Solar production and GHI")
lines(data.xts$GHI, col = "red")


# QQ-plots (Example: DA_Price_DE)
qqnorm(data.xts$DA_Price_DE, main='Normal')
qqline(rnorm(dim(data.xts)[1]))
qqPlot(data.xts$DA_Price_DE)


# Shapiro-Wilk-Test (Test for Normality)
# H_0: Variable is normally distributed
# p-value not >0.05 -> significantly different from normal distribution
p_values_shapiro <- data.frame(matrix(ncol = 1, nrow = dim(data.xts)[2]))
colnames(p_values_shapiro) <- "p-values of Shapiro-Wilk-Test"
rownames(p_values_shapiro) <- colnames(data.xts)
for (i in 1:dim(p_values_shapiro)[1]) {
  t <- shapiro.test(as.numeric(data.xts[,i]))
  p_values_shapiro[i,1] <- t$p.value
}
# none of the variables is Gaussian (atm)
row.names(p_values_shapiro)[which(p_values_shapiro[,1]>0.05)]
# variable with highest p-value and its histogram
rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])]
hist(data.xts$Poland_export, xlab = "Poland_export", main = paste("Histogram of Poland_export"), probability = TRUE)
# alternative: ks.test(as.numeric(data.xts[,76]), "pnorm") (Kolmogorov-Smirnov-Test)

# Are the non-zero variables Gaussian after applying log-transform?
p_values_shapiro_log <- data.frame(matrix(ncol = 1, nrow = dim(non_negatives.xts)[2]))
colnames(p_values_shapiro_log) <- "p-values of Shapiro-Wilk-Test"
rownames(p_values_shapiro_log) <- colnames(non_negatives.xts)
for (i in 1:dim(p_values_shapiro_log)[1]) {
  t <- shapiro.test(log(as.numeric(non_negatives.xts[,i])))
  p_values_shapiro_log[i,1] <- t$p.value
}
# changes not that much..
row.names(p_values_shapiro_log)[which(p_values_shapiro_log[,1]>0.05)]

# Standardizing data
# histogram of non-standardized data (Example: Poland_export)
hist(data.xts[,rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])]], xlab = rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])], main = paste("Histogram of ", rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])]), probability = TRUE)
# histogram of standardized data (Example: Poland_export)
standardized <- scale(data.xts[,rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])]])
hist(standardized, xlab = rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])], main = paste("Histogram of ", rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])]), probability = TRUE)
qqnorm(standardized, main=colnames(data.xts[,rownames(p_values_shapiro)[which.max(p_values_shapiro[,1])]]))
qqline(rnorm(dim(data.xts)[1]))

# Decomposition 
# frequency = 365 means: 365 obs. until season repeats (yearly seasonality)
# Example: forecast_residual_load
decomp_actual_load <- decompose(ts(data.xts$actual_load, frequency = 365))
plot(decomp_actual_load)
# Seasonally Adjusting
actual_load_SeasonAdj <- ts(data.xts$actual_load, frequency = 365) - decomp_actual_load$seasonal
plot(actual_load_SeasonAdj)


####################
### Stationarity ###
####################

# Checking Stationarity. Example: "DA_Price_DE"
#plot.new()
#frame()
#par(mfcol=c(2,2))
# the stationary signal and ACF
plot(data.xts$DA_Price_DE,
     type='l',col='tomato',
     #xlab = "time (t)",
     #ylab = "Y(t)",
     main = "DA_Price_DE")
acf(data.xts$DA_Price_DE,lag.max = length(data.xts$DA_Price_DE),
    xlab = "lag #", ylab = 'ACF', main=' ')

# Ljung-Box test for independence
# H_0: Independence in a given time series 
# (a non-stationary signal will have a low p-value)
Box.test(data.xts$DA_Price_DE, lag=25, type="Ljung-Box")


# ACF- and PACF-plots of all variables
# could include max.lag	= xx
for (i in 1:dim(data.xts)[2]) {
  acf2(data.xts[,i], main = paste("ACF and PACF of ", colnames(data.xts)[i]))
}


# Tests
# https://stats.stackexchange.com/questions/88407/adf-test-pp-test-kpss-test-which-test-to-prefer
# Unit root tests:
# H_0: Unit root
# H_1: Process has root outside the unit circle, which is usually equivalent to stationarity or trend stationarity
# adf.test and pp.test correct for lags (compared to df-test)
# Example: DA_Price_DE
adf.test(data.xts$DA_Price_DE)
pp.test(data.xts$DA_Price_DE)

# stationarity test:
# H_0: (Trend) Stationarity
# H_1: There is a unit root.
# kpss.test: non-parametric test
kpss.test(data.xts$DA_Price_DE)

# How unit-root test and stationarity-test complement each other:
# https://stats.stackexchange.com/questions/30569/what-is-the-difference-between-a-stationary-test-and-a-unit-root-test/235916#235916(I

# Phillips-Perron Test for Unit Roots
# H_0: unit root of a univariate time series x (equivalently, x is a non-stationary time series)
pp_stationary <- data.frame(matrix(ncol = 1, nrow = length(colnames(data.xts))))
colnames(pp_stationary) <- "Stationary?"
rownames(pp_stationary) <- colnames(data.xts)
for (i in 1:dim(pp_stationary)[1]) {
  pp <- pp.test(data.xts[,i])
  if (pp$p.value < 0.05) { # SIGNIFICANCE LEVEL
    pp_stationary[i,1] <- TRUE
  } else {
    pp_stationary[i,1] <- FALSE
  }
}
# non-stationary ts according to PP (significance level .05)
rownames(pp_stationary)[which(pp_stationary==FALSE)]


# Phillips-Perron Test when variables with zeros removed and log applied
#pp_stationary_log <- data.frame(matrix(ncol = 1, nrow = length(colnames(non_negatives.xts))))
#colnames(pp_stationary_log) <- "Stationary?"
#rownames(pp_stationary_log) <- colnames(non_negatives.xts)
#for (i in 1:dim(pp_stationary_log)[1]) {
#  pp <- pp.test(log(non_negatives.xts[,i]))
#  if (pp$p.value < 0.05) {
#    pp_stationary[i,1] <- TRUE
#  } else {
#    pp_stationary[i,1] <- FALSE
#  }
#}

# ADF test
# H_0: unit root of a univariate time series x (equivalently, x is a non-stationary time series)
adf_stationary <- data.frame(matrix(ncol = 1, nrow = length(colnames(data.xts))))
colnames(adf_stationary) <- "Stationary?"
rownames(adf_stationary) <- colnames(data.xts)
for (i in 1:dim(adf_stationary)[1]) {
  adf <- adf.test(data.xts[,i])
  if (adf$p.value < 0.05) { # SIGNIFICANCE LEVEL
    adf_stationary[i,1] <- TRUE
  } else {
    adf_stationary[i,1] <- FALSE
  }
}
# non-stationry ts according to ADF 
rownames(adf_stationary)[which(adf_stationary==FALSE)]


# Results are different for each of the tests when compared
# Which ones are different?
rownames(adf_stationary)[which(adf_stationary != pp_stationary)]
# Is it because of structural break?
# Zivot and Andrews Unit Root Test # because of structural break
library(urca)
za.gnp <- ur.za(data.xts$DA_Price_DE)
summary(za.gnp)


# Inspecting the variables where adf.test and pp.test differ in their outcomes
# Luxembourg_export has a structural break @ 2017-06-28
# This is, because data starts @ 2017-06-28
Luxembourg_export <- data.xts$Luxembourg_export
# Get data from 2017-06-28 until end
Luxembourg_export_clean <- Luxembourg_export["2017-06-28/"]
adf.test(Luxembourg_export_clean)
pp.test(Luxembourg_export_clean)
# Now the tests show the same result :)

# Can we test for structural breaks in general? We can!
library(strucchange)
# DA_Price_DE
DA_Price_DE <- data.xts$DA_Price_DE
plot(DA_Price_DE)
time <- c(1:length(DA_Price_DE))
breakpoints <- breakpoints(DA_Price_DE ~ time, h = 372, breaks = 1)
# We limit the breakpoints to 1, is that reasonable?
breakpoints
breaktime <- time(DA_Price_DE)[breakpoints$breakpoints]
plot(DA_Price_DE)
addEventLines(events = xts(x = '', order.by = breaktime), lty = 2, col = 'red', lwd = 1.5)
# Test for stationarity in "DA_Price_DE" truncated @ 2020-10-14 
DA_Price_DE_clean <- DA_Price_DE["/2020-10-14"]
adf.test(DA_Price_DE_clean)
pp.test(DA_Price_DE_clean)
# Now the tests show the same result :)

# NG_storage
NG_storage <- data.xts$NG_storage
plot(NG_storage)
time <- c(1:length(NG_storage))
breakpoints_storage <- breakpoints(NG_storage ~ time, h = 372, breaks = 1)
breaktime2 <- time(NG_storage)[breakpoints_storage$breakpoints]
plot(NG_storage)
addEventLines(events = xts(x = '', order.by = breaktime2), lty = 2, col = 'red', lwd = 1.5)

# GHI
GHI <- data.xts$GHI
plot(GHI)
# Is periodicity the problem?

# hydropower_production
hydropower_production <- data.xts$hydropower_production
plot(hydropower_production)
time <- c(1:length(hydropower_production))
breakpoints_hydropower_production <- breakpoints(hydropower_production ~ time, h = 372, breaks = 1)
breaktime3 <- time(hydropower_production)[breakpoints_hydropower_production$breakpoints]
plot(hydropower_production)
addEventLines(events = xts(x = '', order.by = breaktime3), lty = 2, col = 'red', lwd = 1.5)

# solar_production
solar_production <- data.xts$solar_production
plot(solar_production)

# Switzerland_P_spread_to_DE
Switzerland_P_spread_to_DE <- data.xts$Switzerland_P_spread_to_DE
plot(Switzerland_P_spread_to_DE)

# How can we overcome structural breaks? 
# Logarithm? 
# But: Problem with negative and zeros as argument of log


#######################
### Differentiation ###
#######################

# Numbers of diff's necessary for stationarity (for entire dataset)
# (PP Test)
no_diffs <- data.frame(matrix(ncol = 1, nrow = dim(data.xts)[2]))
colnames(no_diffs) <- "# of Diffs"
rownames(no_diffs) <- colnames(data.xts)
for (i in 1:dim(no_diffs)[1]) {
  no_diffs[i,1] <- ndiffs(data.xts[,i], test = "pp")
}

# Differentiation
NG_TTF_diff <- diff(data.xts$NG_TTF, no_diffs["NG_TTF",])
NG_TTF_diff <- as.numeric((NG_TTF_diff)[!is.na(NG_TTF_diff)])

#acf(NG_TTF_diff, lag.max = 50, xlab = "lag #", ylab = 'ACF', main=' ')
acf2(NG_TTF_diff, max.lag = 20) # with PACF's
# Plot somehow indicates stationarity


#####################
### Cointegration ###
#####################
# Johansen cointegration test:
# H_1 for the eigenvalue test: here are r+1 cointegration relations
# If test fails to reject H_1 for the first time when r=1, then you have 1 cointegration relationship

#cointegration <- ca.jo(data.xts, type = "eigen", ecdet = "const", spec = "transitory", K = 2, dumvar = NULL)
# Not possible: Matrix not invertible, since linearly dependent columns (i.e. stronlgy correlated variables)

# cointegration of exports
cointegration <- ca.jo(data.xts[,c("Netherlands_export", "Switzerland_export", "Denmark_export", 
                                   "Czech_Republic_export", "Luxembourg_export", "Sweden_export", 
                                   "Austria_export", "France_export", "Poland_export", "Norway_export", "Belgium_export")], 
                       type = "trace", ecdet = "const", spec = "transitory", K = 3, dumvar = NULL)
#summary(cointegration)
# Results:
cbind(cointegration@teststat, cointegration@cval)

#cointegrated = cointegration@V["constant",1] + cointegration@V[1,1]*data.xts[,1] + cointegration@V[2,1]*data.xts[,2] + cointegration@V[3,1]*data.xts[,3]+cointegration@V[4,1]*data.xts[,4]+cointegration@V[5,1]*data.xts[,5]+cointegration@V[6,1]*data.xts[,6]+
#  cointegration@V[7,1]*data.xts[,7]+cointegration@V[8,1]*data.xts[,8]+cointegration@V[9,1]*data.xts[,9]+cointegration@V[10,1]*data.xts[,10]
#plot(cointegrated, type="l")
#adf.test(s)



######################
### Fitting models ###
######################

# Fitting 'Netherlands export' with auto.arima()
fit <- auto.arima(data.xts[,60], trace=TRUE) # Choose ARIMA(2,1,2)
#par(mar=c(1,1,1,1)) # Standard: par(mar=c(5.1, 4.1, 4.1, 2.1))
# dev.off()
tsdiag(fit)
qqnorm(fit$residuals)
TSstudio::arima_diag(ts.obj = fit)

# Diagnostics from different package, therefore we need model in different type
fitv2 <- arima(data.xts[,60],order = c(2,1,2))
aTSA::ts.diag(fitv2)
acf(data.xts[,60])

fitv3 <- sarima(data.xts[,60], 2,1,2) # sarima also plots nice diagnostic plots

# Plot differentiated time series
plot(diff(data.xts[,60], differences = ndiffs(data.xts[,60])))

# Histogram (distribution of data)
hist(data.xts[,60], main = "TITLE", probability = TRUE) #not stationary
hist(diff(data.xts[,60], differences = ndiffs(data.xts[,60])), main = "TITLE", probability = TRUE)

# (Maybe) good packages for causal discovery for TS-data:
#library(bsts)
library(CausalImpact)
library(NlinTS)


######################
#### Energy paper ####
######################
library(tsDyn)
library(vars)
library(strucchange)

### log-scale??? ###

# optimal lag order of the unrestricted VAR
opt_lag <- VARselect(data.xts, lag.max = 10, type = "const") # Akaike: 8
opt_lag$selection 

# different function (don't know the exact difference but leads to different results...)
# lag length + rank
rk_sel <- lags.select(data.xts)
summary(rk_sel)

# p-values for PP-test (original data and after 1st difference)
p_values_pp <- data.frame(matrix(ncol = 2, nrow = length(colnames(data_without_timevariables.xts))))
colnames(p_values_pp) <- c("p-value (original data)", "p-value (1st difference)")
rownames(p_values_pp) <- colnames(data_without_timevariables.xts)
for (i in 1:dim(p_values_pp)[1]) {
  pp <- pp.test(data_without_timevariables.xts[,i])
  p_values_pp[i,1] <- pp$p.value
  # removing first value to get no NA's
  pp2 <- pp.test(diff(data_without_timevariables.xts[,i], differences = 1)[-1])
  p_values_pp[i,2] <- pp2$p.value
}

# p-values for KPSS-test (original data and after 1st difference)
# H_0: time series is stationary
# p-value < 0.05 indicates rejection of H_0 
p_values_kpss <- data.frame(matrix(ncol = 2, nrow = length(colnames(data_without_timevariables.xts))))
colnames(p_values_kpss) <- c("p-value (original data)", "p-value (1st difference)")
rownames(p_values_kpss) <- colnames(data_without_timevariables.xts)
for (i in 1:dim(p_values_kpss)[1]) {
  kpss <- kpss.test(data_without_timevariables.xts[,i])
  p_values_kpss[i,1] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(data_without_timevariables.xts[,i], differences = 1)[-1])
  p_values_kpss[i,2] <- kpss2$p.value
}


# cointegration test
# Therefore we should already select a few of the variables
# and should not do it with the entire data.xts dataset




### VAR model ###
# lag length
var.model <- VAR(y = data_without_timevariables.xts, type = "const", lag.max = 10)
summary(var.model)





VECM_tsDyn <- VECM(data = data.xts[,1:3], lag=3, r=2,
                   estim = "ML",
                   LRinclude = "none")




vec2var_ca.jo <- vec2var(vecm.model, r=2)




