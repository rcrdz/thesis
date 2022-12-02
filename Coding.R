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
library(tsDyn)
library(vars)
library(strucchange)
library(pcalg)
library(igraph)
library(seasonal)
library(seastests)
library(urca)
library(strucchange)
library(corrplot)
library(tsbox)


#source("GrangerTests.R")
#source("ConditionalGrangerCausality.R")


###################
### Import data ###
###################
data_raw <- read.csv(file = 'working_db_Trafo4_Daily_Lagged.csv')
dim(data_raw)
head(data_raw)

# Looking at variable names and rename wrongly named Date
names(data_raw)
#names(data_raw)[1] <- 'Date' # Not necessary anymore i guess..


###############################
### Converting to .xts-data ###
###############################
data_raw.xts <- xts(data_raw[,-1], order.by = as.Date(data_raw[,1], "%d.%m.%y"))


##############################
### Converting to .ts-data ###
##############################
# Converting Date from character representation to class 'Date'
#data_conv <- data_raw
#data_conv %<>%
#  mutate(Date = as.Date(Date, format= "%d.%m.%y"))
# Converting to .ts-data # yearly seasonality
#data.ts <- ts(data_conv, start = c(as.numeric(format(data_conv$Date[1], "%Y")), as.numeric(format(data_conv$Date[1], "%j"))), frequency = 365)
data.ts <-ts_ts(data.xts)


#####################
### Preprocessing ###
#####################
# Remove Time-variables ('DayofWeek' etc.)
data.xts <- data_raw.xts[,! names(data_raw.xts) %in% c("DayofWeek", "Is_Weekday", "Seasons", "Holiday", "Year")]
# Remove 'ShareOf's'
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("shareOf", names(data.xts))])]
# Remove 'Clean prices': combination of "raw prices" and EUA price
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("CleanPrice", names(data.xts))])]
# Remove 'renewables_forecast_error': 'renewables_forecast_error' = sum('biomass_production',...)-sum('gen_forecast_solar',...)
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("renewables_forecast_error", names(data.xts))])]
# Remove 'Total_production' b/c of redundancy
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("Total_production", names(data.xts))])]
# Important variables: 'forecast_residual_load', 'COAL_API2', 'NG_TTF', 'EUA_price'


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

# It is way easier to get the non-negatives than above
non_negatives.xts = data.xts[,colSums(data.xts<0)==0]
names(non_negatives.xts)
# variables with also negative values
negatives.xts <- data.xts[,! names(data.xts) %in% names(non_negatives.xts)]
names(negatives.xts)
# Positive valued variables (BUT: removed also the ones which only "start later")
positives.xts = data.xts[,colSums(data.xts<=0)==0]
names(positives.xts)

# Which variables start later in time?
# Example: physical_net_export
var_bool <- data.xts$physical_net_export != 0
time_index <- min(which(var_bool == TRUE))
plot(data.xts$physical_net_export)
timepoint <- time(data.xts$physical_net_export)[time_index]
addEventLines(events = xts(x = 'first nonzero', order.by = timepoint), lty = 2, col = 'tomato', lwd = 1.5)

# List: When is first nonzero entry
first_nonzero <- data.frame(matrix(ncol = 1, nrow = dim(data.xts)[2]))
colnames(first_nonzero) <- "First nonzero index"
rownames(first_nonzero) <- colnames(data.xts)
for (i in 1:dim(first_nonzero)[1]) {
  boolean <- data.xts[,i] != 0
  first_nonzero[i,1] <- min(which(boolean == TRUE))
}
# Names of variables which "start later"
cat("--------------------------", "VARIABLES 'STARTING LATER'", "--------------------------", rownames(first_nonzero)[first_nonzero$`First nonzero index` != 1], "--------------------------", paste("total:", sum(first_nonzero != 1), "/", dim(data.xts)[2]), sep='\n')



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
cat("-----------------------", "ZERO-INFLATED VARIABLES", "-----------------------", rownames(zero_infl_vars), "-----------------------", paste("total:", dim(zero_infl_vars)[1], "/", dim(data.xts)[2]), sep='\n')
# Non-Zero-inflated variables
non_zero_infl_vars <- subset(zeros_count, `# of Zeros` == 0)
cat("---------------------------", "NOT ZERO-INFLATED VARIABLES", "---------------------------", rownames(non_zero_infl_vars), "---------------------------", paste("total:", dim(non_zero_infl_vars)[1], "/", dim(data.xts)[2]), sep='\n')

# Histograms zero-inflated variables (removed zeros before "start")
for (i in 1:dim(zero_infl_vars)[1]) {
  hist(data.xts[first_nonzero[rownames(zero_infl_vars)[i],]:dim(data.xts)[1], rownames(zero_infl_vars)[i]], xlab = rownames(zero_infl_vars)[i], main = paste("Histogram of ", rownames(zero_infl_vars)[i]), probability = TRUE)
}

# Histograms "non-zero-inflated" variables (removed zeros before "start")
for (i in 1:dim(non_zero_infl_vars)[1]) {
  hist(data.xts[first_nonzero[rownames(non_zero_infl_vars)[i],]:dim(data.xts)[1], rownames(non_zero_infl_vars)[i]], xlab = rownames(non_zero_infl_vars)[i], main = paste("Histogram of ", rownames(non_zero_infl_vars)[i]), probability = TRUE)
}


# Correlation matrix (without lags)
cor_threshold <- 0.9

cor_matrix <- cor(data.xts)
cat("--------------------------------", "Range of Correlations (w/o lags)", "--------------------------------", range(cor_matrix[cor_matrix<1]), sep='\n')
cor_matrix_triang <- cor_matrix
cor_matrix_triang[lower.tri(cor_matrix, diag=TRUE)] <- 0 # make upper triangular matrix
high_cor <- subset(as.data.frame.table(cor_matrix_triang), abs(Freq) > cor_threshold)
colnames(high_cor)[3] <- "Corr"
high_cor <- high_cor[order(-high_cor$Corr),]
rownames(high_cor) <- c()
cat("---------------------", "Top 5 corr (w/o lags)", "---------------------", sep='\n')
print(high_cor[1:5,])


# Covariance + correlation matrix (with lags)
nl <- 2 # number of lags
data_with_lags <- embed(as.matrix(data.xts), nl+1) #produce a matrix with M columns containing the original series and lagged versions of it
ndfs <- paste(rep(colnames(data.xts),nl), "[t-", rep(1:nl, each=ncol(data.xts)), "]", sep = "")
colnames(data_with_lags) <- c(colnames(data.xts), ndfs)
cov_matrix_with_lags <- cov(data_with_lags)

cor_matrix_with_lags <- cov2cor(cov_matrix_with_lags) # Correlation matrix
cat("-------------------------------", "Range of Correlations (w/ lags)", "-------------------------------", range(cor_matrix_with_lags[cor_matrix_with_lags<1]), sep='\n')

cor_matrix_with_lags_triang <- cor_matrix_with_lags
cor_matrix_with_lags_triang[lower.tri(cor_matrix_with_lags, diag=TRUE)] <- 0 # make upper triangular matrix

high_cor_with_lags <- subset(as.data.frame.table(cor_matrix_with_lags_triang), abs(Freq) > cor_threshold)
colnames(high_cor_with_lags)[3] <- "Corr"
high_cor_with_lags <- high_cor_with_lags[order(-high_cor_with_lags$Corr),]
rownames(high_cor_with_lags) <- c()
cat("--------------------", "Top 5 corr (w/ lags)", "--------------------", sep='\n')
print(high_cor_with_lags[1:5,])
#unique(as.vector(as.matrix(high_cor_with_lags[,1:2])))


# QQ-plots (Example: DA_Price_DE)
qqnorm(data.xts$DA_Price_DE, main='Normal')
qqline(rnorm(dim(data.xts)[1]))
qqPlot(data.xts$DA_Price_DE)


# Normality tests
# Shapiro-Wilk-Test
# H_0: Variable is normally distributed
# p-value not >0.05 -> significantly different from normal distribution
# Jarque-Bera test
# H_O: normality

pvals_normality <- data.frame(matrix(ncol = 2, nrow = dim(data.xts)[2]))
colnames(pvals_normality) <- c("pvals Shapiro-Wilk", "pvals Jarque-Bera")
rownames(pvals_normality) <- colnames(data.xts)
for (i in 1:dim(pvals_normality)[1]) {
  t <- shapiro.test(as.numeric(data.xts[,i]))
  pvals_normality[i,1] <- t$p.value
  s <- jarque.bera.test(as.numeric(data.xts[,i]))
  pvals_normality[i,2] <- s$p.value
}
# none of the variables is Gaussian (Shapiro-Wilk)
row.names(pvals_normality)[which(pvals_normality[,1]>0.05)]
# variable with highest p-value and its histogram (Shapiro-Wilk)
rownames(pvals_normality)[which.max(pvals_normality[,1])]
hist(data.xts$Poland_export, xlab = "Poland_export", main = paste("Histogram of Poland_export"), probability = TRUE)
# alternative: ks.test(as.numeric(data.xts[,76]), "pnorm") (Kolmogorov-Smirnov-Test)

# Are the non-zero variables Gaussian after applying log-transform?
pvals_shapiro_log <- data.frame(matrix(ncol = 1, nrow = dim(non_negatives.xts)[2]))
colnames(pvals_shapiro_log) <- "p-values of Shapiro-Wilk-Test"
rownames(pvals_shapiro_log) <- colnames(non_negatives.xts)
for (i in 1:dim(pvals_shapiro_log)[1]) {
  t <- shapiro.test(log(as.numeric(non_negatives.xts[,i])))
  pvals_shapiro_log[i,1] <- t$p.value
}
# changes not that much..
row.names(pvals_shapiro_log)[which(pvals_shapiro_log[,1]>0.05)]



# Standardizing data
# histogram of non-standardized data (Example: Poland_export)
hist(data.xts[,rownames(pvals_normality)[which.max(pvals_normality[,1])]], xlab = rownames(pvals_normality)[which.max(pvals_normality[,1])], main = paste("Histogram of ", rownames(pvals_normality)[which.max(pvals_normality[,1])]), probability = TRUE)
# histogram of standardized data (Example: Poland_export)
standardized <- scale(data.xts[,rownames(pvals_normality)[which.max(pvals_normality[,1])]])
hist(standardized, xlab = rownames(pvals_normality)[which.max(pvals_normality[,1])], main = paste("Histogram of ", rownames(pvals_normality)[which.max(pvals_normality[,1])]), probability = TRUE)
qqnorm(standardized, main=colnames(data.xts[,rownames(pvals_normality)[which.max(pvals_normality[,1])]]))
qqline(rnorm(dim(data.xts)[1]))


###################
### Seasonality ###
###################

# Decomposition 
# frequency = 365 means: 365 obs. until season repeats (yearly seasonality)
# Example: forecast_residual_load
plot(data.xts$actual_load)
decomp_actual_load <- stats::decompose(ts(data.xts$actual_load, frequency = 365.25))
plot(decomp_actual_load)
# Seasonally Adjusting
actual_load_SeasonAdj <- ts(data.xts$actual_load, frequency = 365.25) - decomp_actual_load$seasonal
plot(actual_load_SeasonAdj, main = paste("Deseasonalized", colnames(data.xts$actual_load)))

#stlm <- stl(ts(data_raw$actual_load, frequency = 365.25), s.window = "periodic")
#plot(stlm)

seasonplot(ts(data_raw$actual_load, frequency = 365.25))

seasonality <- data.frame(matrix(ncol = 1, nrow = length(colnames(data.xts))))
colnames(seasonality) <- "Seasonal"
rownames(seasonality) <- colnames(data.xts)
for (i in 1:dim(seasonality)[1]) {
  seasonality[i,1] <- isSeasonal(data.xts[,i], test = "combined")
}



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


# Stationarity Tests
# https://stats.stackexchange.com/questions/88407/adf-test-pp-test-kpss-test-which-test-to-prefer
# Unit root tests: ADF, PP
# H_0: Unit root (equivalently, x is a non-stationary time series)
# H_1: Process has root outside the unit circle, which is usually equivalent to stationarity or trend stationarity
# adf.test and pp.test correct for lags (compared to df-test)


# Stationarity test: KPSS (non-parametric)
# H_0: (Trend) Stationarity
# H_1: There is a unit root

# How unit-root test and stationarity-test complement each other:
# https://stats.stackexchange.com/questions/30569/what-is-the-difference-between-a-stationary-test-and-a-unit-root-test/235916#235916(I

# Log transforming a non-stationary variable can not make it stationary

pvals_tests <- data.frame(matrix(ncol = 7, nrow = length(colnames(data.xts))))
colnames(pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff")
rownames(pvals_tests) <- colnames(data.xts)
for (i in 1:dim(pvals_tests)[1]) {
  adf <- adf.test(data.xts[,i])
  pvals_tests[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(data.xts[,i], differences = 1)[-1])
  pvals_tests[i,2] <- adf1$p.value
  
  pp <- pp.test(data.xts[,i])
  pvals_tests[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(data.xts[,i], differences = 1)[-1])
  pvals_tests[i,4] <- pp1$p.value
  
  kpss <- kpss.test(data.xts[,i])
  pvals_tests[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(data.xts[,i], differences = 1)[-1])
  pvals_tests[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(data.xts[,i], differences = 2)[-c(1, 2)])
  pvals_tests[i,7] <- kpss3$p.value
}

sign.lvl <- 0.05
stationarity <- data.frame(matrix(ncol = 7, nrow = length(colnames(data.xts))))
colnames(stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff")
rownames(stationarity) <- colnames(data.xts)
for (i in 1:dim(stationarity)[1]) {
  if(pvals_tests[i,1] < sign.lvl){
    stationarity[i,1] <- "stat"
  } else {
    stationarity[i,1] <- "non-stat"
  }
  if(pvals_tests[i,2] < sign.lvl){
    stationarity[i,2] <- "stat"
  } else {
    stationarity[i,2] <- "non-stat"
  }
  if(pvals_tests[i,3] < sign.lvl){
    stationarity[i,3] <- "stat"
  } else {
    stationarity[i,3] <- "non-stat"
  }
  if(pvals_tests[i,4] < sign.lvl){
    stationarity[i,4] <- "stat"
  } else {
    stationarity[i,4] <- "non-stat"
  }
  if(pvals_tests[i,5] < sign.lvl){
    stationarity[i,5] <- "non-stat"
  } else {
    stationarity[i,5] <- "stat"
  }
  if(pvals_tests[i,6] < sign.lvl){
    stationarity[i,6] <- "non-stat"
  } else {
    stationarity[i,6] <- "stat"
  }
  if(pvals_tests[i,7] < sign.lvl){
    stationarity[i,7] <- "non-stat"
  } else {
    stationarity[i,7] <- "stat"
  }
}

# Compare ADF and PP
cat("---------", "PP != ADF", "---------", rownames(stationarity)[which(stationarity$ADF!=stationarity$PP)], "---------", paste("total:", length(rownames(stationarity)[which(stationarity$ADF!=stationarity$PP)]), "/", dim(data.xts)[2]), sep='\n')

# Compare ADF, PP and KPSS
cat("--------------------", "!(ADF == PP == KPSS)", "--------------------", rownames(stationarity)[which(stationarity$ADF!=stationarity$PP | stationarity$ADF!=stationarity$KPSS | stationarity$PP!=stationarity$KPSS)], "--------------------", paste("total:", length(rownames(stationarity)[which(stationarity$ADF!=stationarity$PP | stationarity$ADF!=stationarity$KPSS | stationarity$PP!=stationarity$KPSS)]), "/", dim(data.xts)[2]), sep='\n')
cat("-----------------", "ADF == PP == KPSS", "-----------------", rownames(stationarity)[which(stationarity$ADF==stationarity$PP & stationarity$ADF==stationarity$KPSS & stationarity$PP==stationarity$KPSS)], "-----------------", paste("total:", length(rownames(stationarity)[which(stationarity$ADF==stationarity$PP & stationarity$ADF==stationarity$KPSS & stationarity$PP==stationarity$KPSS)]), "/", dim(data.xts)[2]), sep='\n')
cat("-------------------------", "ADF == PP == KPSS == stat", "-------------------------", rownames(stationarity)[which(stationarity$ADF=="stat" & stationarity$PP=="stat" & stationarity$KPSS=="stat")], "-------------------------", paste("total:", length(rownames(stationarity)[which(stationarity$ADF=="stat" & stationarity$PP=="stat" & stationarity$KPSS=="stat")]), "/", dim(data.xts)[2]), sep='\n')
cat("-----------------------------", "ADF == PP == KPSS == non-stat", "-----------------------------", rownames(stationarity)[which(stationarity$ADF=="non-stat" & stationarity$PP=="non-stat" & stationarity$KPSS=="non-stat")], "-----------------------------", paste("total:", length(rownames(stationarity)[which(stationarity$ADF=="non-stat" & stationarity$PP=="non-stat" & stationarity$KPSS=="non-stat")]), "/", dim(data.xts)[2]), sep='\n')

# Unit-root-tests do not detect non-stationarity on basis of seasonality, see:
# https://stats.stackexchange.com/questions/225087/seasonal-data-deemed-stationary-by-adf-and-kpss-tests



# Are some results different because of structural break?
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.zeileis.org/papers/OeSG-2001.pdf
# https://github.com/julzerinos/r-structural-breaks-with-ml

# Zivot and Andrews Unit Root Test
# H_0: unit root process with drift that excludes exogenous structural change
# H_1: depending on the model variant: trend stationary process that allows for a one time break in the level, the trend or both
za.gnp <- ur.za(data.xts$DA_Price_DE)
summary(za.gnp)
plot(data.xts$DA_Price_DE)
addEventLines(events = xts(x = 'Breakpoint', order.by = time(data.xts$DA_Price_DE)[za.gnp@bpoint]), lty = 2, col = 'red', lwd = 1)

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


######################
### Fitting models ###
######################

# Fitting 'Netherlands export' with auto.arima()
fit <- auto.arima(data.xts$Netherlands_export, trace=TRUE) # Choose ARIMA(2,1,2)
# Best model: ARIMA(2,1,2)
par(mar=c(1,1,1,1))
tsdiag(fit)
dev.off()
qqnorm(fit$residuals)
TSstudio::arima_diag(ts.obj = ts(data.xts$Netherlands_export, frequency = 365))

# Diagnostics from different package, therefore we need model in different type
fitv2 <- arima(data.xts$Netherlands_export,order = c(2,1,2))
par(mar=c(1,1,1,1))
aTSA::ts.diag(fitv2)
dev.off()
acf(data.xts$Netherlands_export)

fitv3 <- sarima(data.xts$Netherlands_export, 2,1,2) # sarima also plots nice diagnostic plots

# Plot differentiated time series
plot(diff(data.xts$Netherlands_export, differences = ndiffs(data.xts$Netherlands_export, alpha = 0.05, test = "kpss")))

# Histogram (distribution of data)
hist(data.xts$Netherlands_export, main = "TITLE", probability = TRUE) #not stationary
hist(diff(data.xts$Netherlands_export, differences = ndiffs(data.xts[,60])), main = "TITLE", probability = TRUE)

# (Maybe) good packages for causal discovery for TS-data:
#library(bsts)
library(CausalImpact)
library(NlinTS)


###############################################################
#### Causal modeling and inference for electricity markets ####
###############################################################

# Log-transform
# Depends heavily on the subset we choose (>0!)
subset.xts <- log(data.xts[, c("NG_TTF", "EUA_price", "COAL_API2", "biomass_proudction")])
names(subset.xts)
plot(subset.xts)
corrplot(cor(subset.xts), type = "upper", method = "color",
         insig = "pch", tl.cex = 0.8, tl.col = "black", tl.srt = 45)
# Seasonally adjusting by subtracting seasonal term
# is this okay in this way or do we need a function describing the seasonality?
for (i in 1:dim(subset.xts)[2]) {
  plot(stats::decompose(ts(subset.xts[,i], frequency = 365.25)))
}
for (i in 1:dim(subset.xts)[2]) {
  s <- stats::decompose(ts(subset.xts[,i], frequency = 365.25))
  subset.xts[,i] <- xts(seasadj(s), order.by = as.Date(data_raw[,1], "%d.%m.%y"))
}
plot(subset.xts)


# p-values for tests (+differences)
pvals_tests_subset <- data.frame(matrix(ncol = 7, nrow = length(colnames(subset.xts))))
colnames(pvals_tests_subset) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff")
rownames(pvals_tests_subset) <- colnames(subset.xts)
for (i in 1:dim(pvals_tests_subset)[1]) {
  adf <- adf.test(subset.xts[,i])
  pvals_tests_subset[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(subset.xts[,i], differences = 1)[-1])
  pvals_tests_subset[i,2] <- adf1$p.value
  
  pp <- pp.test(subset.xts[,i])
  pvals_tests_subset[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(subset.xts[,i], differences = 1)[-1])
  pvals_tests_subset[i,4] <- pp1$p.value
  
  kpss <- kpss.test(subset.xts[,i])
  pvals_tests_subset[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(subset.xts[,i], differences = 1)[-1])
  pvals_tests_subset[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(subset.xts[,i], differences = 2)[-c(1, 2)])
  pvals_tests_subset[i,7] <- kpss3$p.value
}


sign.lvl_subset <- 0.05
stationarity_subset <- data.frame(matrix(ncol = 7, nrow = length(colnames(subset.xts))))
colnames(stationarity_subset) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff")
rownames(stationarity_subset) <- colnames(subset.xts)
for (i in 1:dim(stationarity_subset)[1]) {
  if(pvals_tests_subset[i,1] < sign.lvl_subset){
    stationarity_subset[i,1] <- "stat"
  } else {
    stationarity_subset[i,1] <- "non-stat"
  }
  if(pvals_tests_subset[i,2] < sign.lvl_subset){
    stationarity_subset[i,2] <- "stat"
  } else {
    stationarity_subset[i,2] <- "non-stat"
  }
  if(pvals_tests_subset[i,3] < sign.lvl_subset){
    stationarity_subset[i,3] <- "stat"
  } else {
    stationarity_subset[i,3] <- "non-stat"
  }
  if(pvals_tests_subset[i,4] < sign.lvl_subset){
    stationarity_subset[i,4] <- "stat"
  } else {
    stationarity_subset[i,4] <- "non-stat"
  }
  if(pvals_tests_subset[i,5] < sign.lvl_subset){
    stationarity_subset[i,5] <- "non-stat"
  } else {
    stationarity_subset[i,5] <- "stat"
  }
  if(pvals_tests_subset[i,6] < sign.lvl_subset){
    stationarity_subset[i,6] <- "non-stat"
  } else {
    stationarity_subset[i,6] <- "stat"
  }
  if(pvals_tests_subset[i,7] < sign.lvl_subset){
    stationarity_subset[i,7] <- "non-stat"
  } else {
    stationarity_subset[i,7] <- "stat"
  }
}


# optimal lag order of the subset-VAR 
# VARselect heavily depends on lag.max 
# https://stats.stackexchange.com/questions/187289/var-lag-selection-heavily-depends-on-maximum-lag-investigated
# https://stats.stackexchange.com/questions/399772/aic-bic-values-keep-changing-with-lag-max-in-var-model
opt_lag_subset <- VARselect(subset.xts, lag.max = 25, type = "const")
opt_lag_subset$selection 
no_lags_subset <- as.numeric(opt_lag_subset$selection[1])
# optimal order of lags for each indivual variable instead of together
#lapply(subset.xts, VARselect)

# different function (don't know the exact difference but leads in general to different results...)
# lag length + rank
rk_sel <- lags.select(subset.xts, lag.max = 25)
summary(rk_sel)


# Johansen Procedure for VAR / Cointegration
# !! Critical values are only reported for systems with less than 11 variables and are taken from Osterwald-Lenum
# spec = "transitory" leads to the VECM meant in the paper (see ?ca.jo)
# ecdet = "const" for constant term in cointegration (intercept)

# https://www.r-econometrics.com/timeseries/vecintro/
coint_test <- ca.jo(subset.xts, type = "trace", ecdet = "const", spec = "transitory", K = no_lags_subset, dumvar = NULL)
coint_test_noconst <- ca.jo(subset.xts, type = "trace", ecdet = "none", spec = "transitory", K = no_lags_subset, dumvar = NULL)
coint_test_trend <- ca.jo(subset.xts, type = "trace", ecdet = "trend", spec = "transitory", K = no_lags_subset, dumvar = NULL)
#summary(coint_test)
# most important results
beta <- coint_test@V
alpha <- coint_test@W
gammas <- coint_test@GAMMA
# PI = Error-correction-term (matrix)
pi <- alpha%*%t(beta) # is the same as coint_test@PI
cbind(coint_test@teststat, coint_test@cval)
# Interpretation:
# r=0 tests for presence of cointegration
# test statistic for r=0 exceeds 1% sign.lvl. we have strong evidence to reject H_0 of no cointegration
# But: for sign.lvl. 0.01 we're not able to reject H_0: r<=1 
# => 1 cointegrated vectors at a 1% significance level
# A cointegrating vector is a stationary linear combination of possibly nonstationary vector time-series components
# https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/

# Estimating VECM with cajorls() and specified rank from teststatistic
VECM_ca.jo <- cajorls(coint_test, r = 1)
summary(VECM_ca.jo$rlm)
VECM_ca.jo$beta
VECM_ca.jo$rlm$coefficients

# Check if cointegrated time series we get is stationary
# normalized cointegration vectors
VECM_ca.jo$beta
# (1st) cointegration vector
coint.ts1 <- VECM_ca.jo$beta[1,1]*subset.xts$NG_TTF + VECM_ca.jo$beta[2,1]*subset.xts$EUA_price +
  VECM_ca.jo$beta[3,1]*subset.xts$COAL_API2 + VECM_ca.jo$beta[4,1]*subset.xts$biomass_proudction + VECM_ca.jo$beta[5,1]
adf.test(coint.ts1)
plot(coint.ts1)
# not stationary => WHY??


# Alternative: Estimating VECM with VECM()-function
VECM_VECM <- VECM(subset.xts, lag = no_lags_subset-1, r = 1, estim = "ML")
# NOTE: VECM(): lag = k-1
summary(VECM_VECM)
residuals <- VECM_VECM$residuals



# Jarque-Bera test for residuals
# tests for normality in both the univariate and multivariate case
# H_0: normality
#jarque.bera.test()
# tests only for univariate time series (in)
# -> different approach needed!

# normality.test() computes univariate and multivariate Jarque-Bera tests for residuals of VECM
# To use normality.test() we need to estimate vec2var
# (restricted VECM)
# the VAR representation of a VECM from ca.jo
vecm.level <- vec2var(coint_test, r=1)
norm_test <- normality.test(vecm.level, multivariate.only = FALSE)
pvals_res <- data.frame(matrix(ncol = 1, nrow = dim(subset.xts)[2]+1))
colnames(pvals_res) <- "p-value"
rownames(pvals_res) <- c(colnames(subset.xts),"Multivariate")
pvals_res[1,1] <- as.numeric(norm_test$jb.uni$`resids of NG_TTF`$p.value)
pvals_res[2,1] <- as.numeric(norm_test$jb.uni$`resids of EUA_price`$p.value)
pvals_res[3,1] <- as.numeric(norm_test$jb.uni$`resids of COAL_API2`$p.value)
pvals_res[4,1] <- as.numeric(norm_test$jb.uni$`resids of biomass_proudction`$p.value)
pvals_res[5,1] <- as.numeric(norm_test$jb.mul$JB$p.value)
# All values <.05 => residuals not normally distr. (also not multivariate normal)
# [Geht bestimmt schöner zu coden..]

# Autocorrelation between residuals?
for (i in 1:dim(norm_test$resid)[2]) {
  acf2(norm_test$resid[,i], main = paste("ACF and PACF of residuals of", colnames(subset.xts)[i]))
}
# showes no significant auto-correlation between the residuals
# => assumption of independent and non-Gaussian residuals is not unreasonable
# => LiNGAM can be used

# weak exogeneity test
# BUILD IT FROM HERE: https://stackoverflow.com/questions/64289992/vecm-in-r-testing-weak-exogeneity-and-imposing-restrictions
# restriction matrix:
DA <- matrix(c(1,0,0,0,0,1,0,0), c(4,2))
exogeneity_test <- alrtest(coint_test, A=DA, r=2)
summary(exogeneity_test)


# exclusion test
# for more info on test: Juselius (2006)


# Obtain Impulse-response-function
#ir <- irf(vecm.level, n.ahead = 20, runs = 500)
#plot(ir)


# standardize time series
subset.xts_std <- scale(subset.xts)
plot(subset.xts_std)







# Procedure on p. 7 of paper


# Translate the estimated VECM coeffs into a VAR representation
vecm.level
# coeffs: vecm.level$A$...
# residuals
res.vecm.level <- vecm.level$resid

# LiNGAM analysis
# Note: res$Bpruned is transpose of adjacency matrix
res <- lingam(res.vecm.level)
adjmat <- as(res, "amat")
B_null <- t(res$Bpruned)
colnames(B_null) <- names(subset.xts)

# B_0: The instantaneous causal effects
g <- graph_from_adjacency_matrix(
  B_null,
  mode = "directed",
  weighted = TRUE
)

plot.igraph(g, layout=layout.reingold.tilford, edge.arrow.size=0.01, vertex.size = 5, vertex.label.cex = 1)


# How to combine unit root and stationary tests
# https://stats.stackexchange.com/questions/30569/what-is-the-difference-between-a-stationary-test-and-a-unit-root-test/235916#235916

# When VAR when VECM
# https://www.researchgate.net/post/Is-it-necessary-for-variables-to-be-integrated-of-order-1-to-applying-VAR-model-or-I-can-use-it-if-variables-are-integrated-of-any-order


# for nice correlation plots:
#corrplot(corr_matrix_with_lags, method="number")
