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
library(readxl)
library(ggplot2)
library(visNetwork)
library(htmlwidgets)
# (Maybe) good packages for Causal Discovery in TS-data:
#library(bsts)
#library(CausalImpact)
#library(NlinTS)


#source("GrangerTests.R")
#source("ConditionalGrangerCausality.R")


######################
### Importing data ###
######################

#csvdata <- read.csv(file = 'working_db_Trafo4_Daily_Lagged.csv')
exceldata <- read_excel("working_db_Trafo5_Daily_Lagged.xlsx")
data_raw <- data.frame(exceldata)
data_raw$Date <- format(as.POSIXct(data_raw$Date, tz = "UTC"), 
                        format = "%Y-%m-%d")
dim(data_raw)
head(data_raw)

# Explore variable names and rename wrongly named 'Date'
names(data_raw)
#names(data_raw)[1] <- 'Date'
# NOT NECESSARY ANYMORE


##########################
### Converting to .xts ###
##########################

# Date formats: https://www.ibm.com/docs/en/cmofm/9.0.0?topic=SSEPCD_9.0.0/com.ibm.ondemand.mp.doc/arsa0257.html
# ?strptime, as.Date(data_raw[,1], "%Y-%m-%d")
data_raw.xts <- xts(data_raw[,-1], order.by = as.Date(data_raw[,1], "%Y-%m-%d"))


#############################################
### Removing globally redundant variables ###
#############################################

# Remove Time-variables ('DayofWeek' etc.)
data.xts <- data_raw.xts[,! names(data_raw.xts) %in% c("DayofWeek", "Is_Weekday", "Seasons", "Holiday", "Year")]
# Remove 'ShareOf's'
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("shareOf", names(data.xts))])]
# Remove 'renewables_forecast_error': 'renewables_forecast_error' = sum('biomass_production', ...)-sum('gen_forecast_solar', ...)
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("renewables_forecast_error", names(data.xts))])]
# Remove 'Total_production': 'Total_production' = sum('gas_production', ...)
data.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("Total_production", names(data.xts))])]


# Important variables: 'forecast_residual_load', 'COAL_API2', 'NG_TTF', 'EUA_price'
# # # ## # ## # ## # ## # # #


#########################
### Converting to .ts ###
#########################

# Converting Date from character representation to class 'Date'
#data_conv <- data_raw
#data_conv %<>%
#  mutate(Date = as.Date(Date, format= "%d.%m.%y"))
# Converting to .ts-data # yearly seasonality
#data.ts <- ts(data_conv, start = c(as.numeric(format(data_conv$Date[1], "%Y")), as.numeric(format(data_conv$Date[1], "%j"))), frequency = 365)
data.ts <- ts_ts(data.xts)


######################
### Standard Plots ###
######################

# A few plots..
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
plot(data.xts[,c("gen_forecast_Solar", "gen_forecast_wind_Offshore", "gen_forecast_wind_Onshore")], main = "Forecast Solar, Wind On- and Offshore Generation", col = c("black", "tomato", "blue"))
addLegend(legend.loc = "topleft",
          legend.names = c(names(data.xts)[4], names(data.xts)[5], names(data.xts)[6]),
          #col = c("black", "tomato", "blue"),
          lty=1, lwd=1)

# Netherlands export, import
plot(data.xts[,c("Netherlands_import", "Netherlands_export")], main = "Netherlands export, import")
addLegend(legend.loc = "topleft",
          legend.names = c(names(data.xts$Netherlands_export), names(data.xts$Netherlands_import)),
          lty=1, lwd=1)


########################
### Data Exploration ###
########################

# Box-Plot of all variables
stack(as.data.frame(data.xts)) %>% ggplot(aes(x = ind, y = values, fill = ind)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  xlab('Variables') +
  ylab('Values')


# -------------------------- 
# Variables 'starting later' 
# -------------------------- 

# Example: physical_net_export
plot(data.xts$physical_net_export)
time_index <- min(which((data.xts$physical_net_export != 0) == TRUE))
addEventLines(events = xts(x = 'first nonzero', order.by = time(data.xts$physical_net_export)[time_index]), lty = 2, col = 'tomato', lwd = 1.5)

# Determine first 'non-zero'-entry
first_nonzero <- data.frame(matrix(ncol = 1, nrow = dim(data.xts)[2]))
colnames(first_nonzero) <- "First nonzero index"
rownames(first_nonzero) <- colnames(data.xts)
for (i in 1:dim(first_nonzero)[1]) {
  boolean <- data.xts[,i] != 0
  first_nonzero[i,1] <- min(which(boolean == TRUE))
}
# Names of variables which "start later"
cat("--------------------------", "VARIABLES 'STARTING LATER'", "--------------------------", rownames(first_nonzero)[first_nonzero$`First nonzero index` >2], "--------------------------", paste("total:", sum(first_nonzero > 2), "/", dim(data.xts)[2]), sep='\n')


# -----------------------------
# Positive / Negative Variables
# -----------------------------

# Positive variables
strictly_positives.xts <- xts(order.by=index(data.xts))
for (i in 1:dim(data.xts)[2]){
  if (colSums(data.xts[first_nonzero[i,]:dim(data.xts)[1], i]<=0) == 0){
    strictly_positives.xts <- cbind(strictly_positives.xts, data.xts[,i])
  }
}
cat("---------------------------", "STRICTLY POSITIVE VARIABLES", "---------------------------", names(strictly_positives.xts), "---------------------------", paste("total:", dim(strictly_positives.xts)[2], "/", dim(data.xts)[2]), sep='\n')

# Non-negative variables (|R^+_0)
non_negatives.xts = data.xts[,colSums(data.xts<0)==0]
cat("----------------------", "NON-NEGATIVE VARIABLES", "----------------------", names(non_negatives.xts), "----------------------", paste("total:", dim(non_negatives.xts)[2], "/", dim(data.xts)[2]), sep='\n')

# Variables which attain negative AND positive values
negatives.xts <- data.xts[,! names(data.xts) %in% names(non_negatives.xts)]
cat("-----------------------------------------", "VARIABLES WITH NEGATIVE AND POSITIVE VALS", "-----------------------------------------", names(negatives.xts), "-----------------------------------------", paste("total:", dim(negatives.xts)[2], "/", dim(data.xts)[2]), sep='\n')


# ------------------
# Zero-inflated data
# ------------------

zeros_count <- data.frame(matrix(ncol = 1, nrow = dim(data.xts)[2]))
rownames(zeros_count) <- colnames(data.xts)
colnames(zeros_count) <- "# of Zeros"
for (i in 1:dim(zeros_count)[1]) {
  #zeros_count[1,i] <- sum(data.xts[,i]==0)
  zeros_count[i,1] <- sum(data.xts[first_nonzero[i,]:dim(data.xts)[1],i]==0)
}
# Histograms of variables which contain at least one zero (removed zeros before "start")
for (i in 1:dim(subset(zeros_count, `# of Zeros` != 0))[1]) {
  hist(data.xts[first_nonzero[rownames(subset(zeros_count, `# of Zeros` != 0))[i],]:dim(data.xts)[1], rownames(subset(zeros_count, `# of Zeros` != 0))[i]], xlab = rownames(subset(zeros_count, `# of Zeros` != 0))[i], main = paste("Histogram of ", rownames(subset(zeros_count, `# of Zeros` != 0))[i]), probability = TRUE)
}

# >30 zeros as threshold for zero-inflation:
# Zero-inflated variables
zero_infl_vars.xts <- data.xts[,rownames(subset(zeros_count, `# of Zeros` > 30))]
# Histograms zero-inflated variables (removed zeros before "start")
for (i in 1:dim(zero_infl_vars.xts)[2]) {
  hist(data.xts[first_nonzero[names(zero_infl_vars.xts)[i],]:dim(data.xts)[1], names(zero_infl_vars.xts)[i]], xlab = names(zero_infl_vars.xts)[i], main = paste("Histogram of ", names(zero_infl_vars.xts)[i]), probability = TRUE)
}
# Which ones seem not zero-inflated?
#         IS THERE ALSO A TEST FOR ZERO-INFLATION?
# No count-data / Varibales which also have negative values:
zero_infl_vars.remove <- c("DK_1_P_spread_to_DE", "DK_2_P_spread_to_DE", "France_P_spread_to_DE",
                           "Netherlands_P_spread_to_DE", "Slovenia_P_spread_to_DE", "Czech_P_spread_to_DE",
                           "Austria_P_spread_to_DE", "Belgium_P_spread_to_DE", "physical_net_export")
zero_infl_vars.xts <- zero_infl_vars.xts[,! names(zero_infl_vars.xts) %in% zero_infl_vars.remove]
cat("-----------------------", "ZERO-INFLATED VARIABLES", "-----------------------", names(zero_infl_vars.xts), "-----------------------", paste("total:", dim(zero_infl_vars.xts)[2], "/", dim(data.xts)[2]), sep='\n')

# Non-zero-inflated variables
non_zero_infl_vars.xts <- data.xts[,! names(data.xts) %in% names(zero_infl_vars.xts)]
cat("---------------------------", "NOT-ZERO-INFLATED VARIABLES", "---------------------------", names(non_zero_infl_vars.xts), "---------------------------", paste("total:", dim(non_zero_infl_vars.xts)[2], "/", dim(data.xts)[2]), sep='\n')
# Histograms "non-zero-inflated" variables (removed zeros before "start")
for (i in 1:dim(non_zero_infl_vars.xts)[2]) {
  hist(data.xts[first_nonzero[names(non_zero_infl_vars.xts)[i],]:dim(data.xts)[1], names(non_zero_infl_vars.xts)[i]], xlab = names(non_zero_infl_vars.xts)[i], main = paste("Histogram of ", names(non_zero_infl_vars.xts)[i]), probability = TRUE)
}


# -----------------------------------
# Covariance and Correlation matrices
# -----------------------------------

# Correlation matrix (w/o lags)
cor_threshold <- 0.85

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

# Covariance + correlation matrix (w/ lags)
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


# -----------------
# Gaussianity tests
# -----------------

# (I) Shapiro-Wilk-Test
# H_0: Variable is normally distributed
# p-value not >0.05 -> significantly different from normal distribution
# (II) Jarque-Bera test
# H_O: Normality
# Not used: (III) Kolmogorov-Smirnov-Test
# H_0: Normality
# ks.test(as.numeric(data.xts[,76]), "pnorm") 

pvals_normality <- data.frame(matrix(ncol = 2, nrow = dim(data.xts)[2]))
colnames(pvals_normality) <- c("pvals Shapiro-Wilk", "pvals Jarque-Bera")
rownames(pvals_normality) <- colnames(data.xts)
for (i in 1:dim(pvals_normality)[1]) {
  t <- shapiro.test(as.numeric(data.xts[,i]))
  pvals_normality[i,1] <- t$p.value
  s <- jarque.bera.test(as.numeric(data.xts[,i]))
  pvals_normality[i,2] <- s$p.value
}
# None of the variables is Gaussian (Shapiro-Wilk):
row.names(pvals_normality)[which(pvals_normality[,1]>0.05)]
# variable with highest p-value and its histogram (Shapiro-Wilk)
rownames(pvals_normality)[which.max(pvals_normality[,1])]
hist(data.xts$Poland_export, xlab = "Poland_export", main = paste("Histogram of Poland_export"), probability = TRUE)

# Are the strictly positives Gaussian after applying log-transform?
pvals_shapiro_log <- data.frame(matrix(ncol = 1, nrow = dim(strictly_positives.xts)[2]))
colnames(pvals_shapiro_log) <- "p-values of Shapiro-Wilk-Test"
rownames(pvals_shapiro_log) <- colnames(strictly_positives.xts)
for (i in 1:dim(pvals_shapiro_log)[1]) {
  t <- shapiro.test(log(as.numeric(strictly_positives.xts[,i])))
  pvals_shapiro_log[i,1] <- t$p.value
}
# Still none of the variables Gaussian:
row.names(pvals_shapiro_log)[which(pvals_shapiro_log[,1]>0.05)]


# ----------------------------
# Standardizing data (example)
# ----------------------------

# Histogram of non-standardized data
# Example: Poland_export (b/c highest p-value)
Poland_export <- data.xts$Poland_export
hist(data.xts[,rownames(pvals_normality)[which.max(pvals_normality[,1])]], xlab = rownames(pvals_normality)[which.max(pvals_normality[,1])], main = paste("Histogram of", rownames(pvals_normality)[which.max(pvals_normality[,1])]), probability = TRUE)
qqnorm(Poland_export, main=colnames(data.xts[,rownames(pvals_normality)[which.max(pvals_normality[,1])]]))
qqline(rnorm(dim(data.xts)[1]), col = "red")
# Standardized
Poland_export.std <- scale(data.xts[,rownames(pvals_normality)[which.max(pvals_normality[,1])]])
hist(Poland_export.std, xlab = rownames(pvals_normality)[which.max(pvals_normality[,1])], main = paste("Histogram of", rownames(pvals_normality)[which.max(pvals_normality[,1])]), probability = TRUE)
qqnorm(Poland_export.std, main=colnames(data.xts[,rownames(pvals_normality)[which.max(pvals_normality[,1])]]))
qqline(rnorm(dim(data.xts)[1]), col = "red")


#####################################
### Log and Logit-transformations ###
#####################################

# Applying it globally 
# But: Transformations to all or only specific ones?


###################
### Seasonality ###
###################

# --------------------------------
# Test for Seasonality [UNCERTAIN]
# --------------------------------
pvals_seasonality <- data.frame(matrix(ncol = 1, nrow = length(colnames(data.xts))))
colnames(pvals_seasonality) <- "Seasonal"
rownames(pvals_seasonality) <- colnames(data.xts)
for (i in 1:dim(pvals_seasonality)[1]) {
  pvals_seasonality[i,1] <- isSeasonal(data.xts[,i], test = "combined")
}

# ---------------------------------------------
# Decomposition with decompose() + seasonplot()
# ---------------------------------------------

# frequency = 365 means: 365 obs. until season repeats (yearly seasonality)
# Example: forecast_residual_load
plot(data.xts$lignite_production)
decomp_actual_load <- stats::decompose(ts(data.xts$lignite_production, frequency = 365.25))
plot(decomp_actual_load)
# Seasonally Adjusting
actual_load_SeasonAdj <- ts(data.xts$actual_load, frequency = 365.25) - decomp_actual_load$seasonal
plot(actual_load_SeasonAdj, main = paste("Deseasonalized", colnames(data.xts$actual_load)))

# Alternative: seasonplot:
seasonplot(ts(data_raw$actual_load, frequency = 365.25))


# -----------------------------------
# Estimating Seasonality with Fourier
# -----------------------------------

#t <- seq(from = 1, to = dim(data.xts)[1], by = 1)
# Estimating seasonality components 
data.lms <- lapply(1:dim(data.xts)[2], function(x) lm(data.xts[first_nonzero[names(data.xts)[x],1]:dim(data.xts)[1],x] ~ sin(2*pi*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[names(data.xts)[x],1]+1, by = 1)/365.25) 
                                                    + cos(2*pi*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[names(data.xts)[x],1]+1, by = 1)/365.25) 
                                                    + sin(2*pi*2*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[names(data.xts)[x],1]+1, by = 1)/365.25) 
                                                    + cos(2*pi*2*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[names(data.xts)[x],1]+1, by = 1)/365.25)))
data.fitted <- sapply(data.lms, fitted)
for(i in 1:dim(data.xts)[2]){
  if (length(data.fitted[[i]]) < dim(data.xts)[1]){
    data.fitted[[i]] <- c(rep(0, first_nonzero[names(data.xts)[i],1]-1), data.fitted[[i]])
  }
}
data.fitted <- data.frame(data.fitted)
colnames(data.fitted) <- colnames(data.xts)
data.fitted <- xts(data.fitted, order.by = as.Date(data_raw[,1], "%Y-%m-%d"))


for(i in 1:dim(data.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = data.xts, aes(x = time(data.xts), y = as.numeric(data.xts[,i])), color = "black") +
    geom_line(data = data.fitted, aes(x = time(data.xts), y = as.numeric(data.fitted[,i])), color = "red") +
    xlab('Date') +
    ylab(colnames(data.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
  #Sys.sleep(2)
}

# FROM WHICH ONES SHOULD WE REALLY SUBTRACT THE ESTIMATED SEASONAL COMPONENT?
seasonadj.xts <- data.xts - data.fitted


####################
### Stationarity ###
####################

# Example: "DA_Price_DE" ACF plot
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


# -------------------------------
# Ljung-Box test for independence
# -------------------------------

# H_0: Independence in a given time series 
# (a non-stationary signal will have a low p-value)
Box.test(data.xts$DA_Price_DE, lag=25, type="Ljung-Box")


# ------------
# ACF and PACF
# ------------

# Further specifications: max.lag	= xx
for (i in 1:dim(data.xts)[2]) {
  acf2(data.xts[,i], main = paste("ACF and PACF of ", colnames(data.xts)[i]))
}


# ------------------
# Stationarity Tests
# ------------------

# https://stats.stackexchange.com/questions/88407/adf-test-pp-test-kpss-test-which-test-to-prefer
# Unit root tests: ADF, PP
# H_0: Unit root (equivalently, x is a non-stationary time series)
# H_1: Process has root outside the unit circle, which is usually equivalent to stationarity or trend stationarity

# (Trend-) Stationarity test: KPSS (non-parametric)
# H_0: (Trend) Stationarity
# H_1: There is a unit root

# How unit-root test and stationarity-test complement each other:
# https://stats.stackexchange.com/questions/30569/what-is-the-difference-between-a-stationary-test-and-a-unit-root-test/235916#235916(I
# PP main disadvantage: performs worse than ADF if sample not big enough
# PP main advantage: non-parametric

# Log transforming a non-stationary variable can not make it stationary

# Are some results different because of structural break?
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.zeileis.org/papers/OeSG-2001.pdf
# https://github.com/julzerinos/r-structural-breaks-with-ml

# Zivot and Andrews Unit Root Test (allows for structural breaks)
# H_0: unit root process with drift that excludes exogenous structural change
# H_1: depending on the model variant: trend stationary process that allows for a one time break in the level, the trend or both
# Reject H_0 if teststat < critical value => (trend-) stationary

# p-values for the tests
pvals_tests <- data.frame(matrix(ncol = 8, nrow = length(colnames(data.xts))))
colnames(pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)", "KPSS (deseason)")
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
  kpss4 <- kpss.test(seasonadj.xts[,i])
  pvals_tests[i,8] <- kpss4$p.value
}

# Zivot and Andrews test
za.tests <- lapply(1:dim(data.xts)[2], function(x) ur.za(data.xts[,x]))

# Plot of all variables with their potential structural breaks
for(i in 1:dim(data.xts)[2]) {  
  pl <- ggplot() + 
    geom_line(data = data.xts, aes(x = time(data.xts), y = as.numeric(data.xts[,i])), color = "black") +
    geom_vline(xintercept = time(data.xts)[za.tests[[i]]@bpoint], color = "red", linetype="dashed") +
    xlab('Date') +
    ylab(colnames(data.xts)[i])
  print(pl)
}

# Matrix for stationarity: stat vs non-stat
sign.lvl.pp <- 0.05
sign.lvl.adf <- 0.05
sign.lvl.kpss <- 0.05
sign.lvl.za <- 0.05
ifelse(sign.lvl.za == 0.05, ind <- 2, ind <- 1)
stationarity <- data.frame(matrix(ncol = 9, nrow = length(colnames(data.xts))))
colnames(stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)", "KPSS (deseason)", "ZA")
rownames(stationarity) <- colnames(data.xts)
for (i in 1:dim(stationarity)[1]) {
  if(pvals_tests[i,1] < sign.lvl.adf){
    stationarity[i,1] <- "stat"
  } else {
    stationarity[i,1] <- "non-stat"
  }
  if(pvals_tests[i,2] < sign.lvl.adf){
    stationarity[i,2] <- "stat"
  } else {
    stationarity[i,2] <- "non-stat"
  }
  if(pvals_tests[i,3] < sign.lvl.pp){
    stationarity[i,3] <- "stat"
  } else {
    stationarity[i,3] <- "non-stat"
  }
  if(pvals_tests[i,4] < sign.lvl.pp){
    stationarity[i,4] <- "stat"
  } else {
    stationarity[i,4] <- "non-stat"
  }
  if(pvals_tests[i,5] < sign.lvl.kpss){
    stationarity[i,5] <- "non-stat"
  } else {
    stationarity[i,5] <- "stat"
  }
  if(pvals_tests[i,6] < sign.lvl.kpss){
    stationarity[i,6] <- "non-stat"
  } else {
    stationarity[i,6] <- "stat"
  }
  if(pvals_tests[i,7] < sign.lvl.kpss){
    stationarity[i,7] <- "non-stat"
  } else {
    stationarity[i,7] <- "stat"
  }
  if(pvals_tests[i,8] < sign.lvl.kpss){
    stationarity[i,8] <- "non-stat"
  } else {
    stationarity[i,8] <- "stat"
  }
  if(za.tests[[i]]@teststat < za.tests[[i]]@cval[ind]){
    stationarity[i,9] <- "stat"
  } else {
    stationarity[i,9] <- "non-stat"
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


#
# =/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=
# =/=/=/=/=/=/=/=/=/=/=/= BREAK =/=/=/=/=/=/=/=/=/=/=/=/=/=
# =/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=
#


# --------------------------------------
# Long version of structural-break check
# --------------------------------------

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


#
# =/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=
# =/=/=/=/=/=/=/=/=/=/=/= BREAK END =/=/=/=/=/=/=/=/=/=/=/=
# =/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=
#


###############################################################
#### Causal modeling and inference for electricity markets ####
###############################################################
# When VAR when VECM
# https://www.researchgate.net/post/Is-it-necessary-for-variables-to-be-integrated-of-order-1-to-applying-VAR-model-or-I-can-use-it-if-variables-are-integrated-of-any-order

# ----------------------
# 1st "complete" dataset
# ----------------------
# COAL AND NG: EUA_price AND NG_TTF/COAL_API2
# Remove 'Clean prices': combination of "raw prices" and EUA price (Coal and NG)
complete.xts <- data.xts[,! names(data.xts) %in% names(data.xts[,grep("CleanPrice", names(data.xts))])]

# Plot data
for(i in 1:dim(complete.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = complete.xts, aes(x = time(data.xts), y = as.numeric(complete.xts[,i])), color = "black") +
    xlab('Date') +
    ylab(colnames(complete.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}

# Detecting exponentially growing trend
# if no exponentially growing trend => No need for log (?)
# Which ones seem to have exponential trend?
# DA_Price_DE, COAL_API2, EUA_price, NG_TTF

# LOGIT-TRANSFORM variables with values in 0% - 100%: NG_storage
complete.xts$NG_storage <- logit(complete.xts$NG_storage/100)
# Is it reasonable? 

# LOG-TRANSFORM ALL STRICTLY POSITIVES
pos <- names(complete.xts)[names(complete.xts) %in% names(strictly_positives.xts)[!names(strictly_positives.xts) %in% c("NG_storage")]]
for (i in pos){
  complete.xts[first_nonzero[i,]:dim(complete.xts)[1], i] <- log(complete.xts[first_nonzero[i,]:dim(complete.xts)[1], i])
}
# LOG-TRANSFORM ALL variables WITH ZEROS AND NEGATIVE VAL'S
# (Value + Maximum Negative value + 1)
for (i in names(complete.xts)[!names(complete.xts) %in% c(pos,"NG_storage")]){
  complete.xts[first_nonzero[i,]:dim(complete.xts)[1], i] <- log(complete.xts[first_nonzero[i,]:dim(complete.xts)[1], i] +
                                                                   abs(min(data.xts[,i][data.xts[,i]<=0])) +1)
}

# LOG-TRANSFORM ONLY the variables which have exponential trend
#complete.xts$DA_Price_DE <- log(complete.xts[first_nonzero["DA_Price_DE",]:dim(complete.xts)[1], "DA_Price_DE"] + abs(min(data.xts[,"DA_Price_DE"][data.xts[,"DA_Price_DE"]<=0])) +1)
#complete.xts$COAL_API2 <- log(complete.xts[first_nonzero["COAL_API2",]:dim(complete.xts)[1], "COAL_API2"])
#complete.xts$EUA_price <- log(complete.xts[first_nonzero["EUA_price",]:dim(complete.xts)[1], "EUA_price"])
#complete.xts$NG_TTF <- log(complete.xts[first_nonzero["NG_TTF",]:dim(complete.xts)[1], "NG_TTF"])

# Estimating seasonality components 
complete.lms <- lapply(1:dim(complete.xts)[2], function(x) lm(complete.xts[first_nonzero[names(complete.xts)[x],1]:dim(complete.xts)[1], x] ~ sin(2*pi*seq(from = 1, to = dim(complete.xts)[1]-first_nonzero[names(complete.xts)[x], 1]+1, by = 1)/365.25) 
                                                              + cos(2*pi*seq(from = 1, to = dim(complete.xts)[1]-first_nonzero[names(complete.xts)[x],1]+1, by = 1)/365.25) 
                                                              + sin(2*pi*2*seq(from = 1, to = dim(complete.xts)[1]-first_nonzero[names(complete.xts)[x],1]+1, by = 1)/365.25) 
                                                              + cos(2*pi*2*seq(from = 1, to = dim(complete.xts)[1]-first_nonzero[names(complete.xts)[x],1]+1, by = 1)/365.25)))
complete.fitted <- sapply(complete.lms, fitted)
for(i in 1:dim(complete.xts)[2]){
  if (length(complete.fitted[[i]]) < dim(complete.xts)[1]){
    complete.fitted[[i]] <- c(rep(0, first_nonzero[names(complete.xts)[i],1]-1), complete.fitted[[i]])
  }
}
complete.fitted <- data.frame(complete.fitted)
colnames(complete.fitted) <- colnames(complete.xts)
complete.fitted <- xts(complete.fitted, order.by = as.Date(data_raw[,1], "%Y-%m-%d"))

# Checking seasonality
for(i in 1:dim(complete.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = complete.xts, aes(x = time(complete.xts), y = as.numeric(complete.xts[,i])), color = "black") +
    geom_line(data = complete.fitted, aes(x = time(complete.xts), y = as.numeric(complete.fitted[,i])), color = "red") +
    xlab('Date') +
    ylab(colnames(complete.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}

# Variables which seem NOT to have seasonality:
# Zero-inflated data should not get seasonality removed (?)
# Otherwise zero-inflation disappears (?)
complete.remove <- unique(c(names(zero_infl_vars.xts), "Belgium_import", "Belgium_export", "Norway_import", "Norway_export",
                            "Poland_import", "France_export", "Netherlands_import", "Netherlands_export", "Sweden_4_P_spread_to_DE", 
                            "COAL_API2", "EUA_price", "NG_TTF", "DA_Price_DE"))
for (i in names(complete.xts[, ! names(complete.xts) %in% complete.remove])){
  complete.xts[,i] <- complete.xts[,i] - complete.fitted[,i]
}

# Plots after deseasonalization
for(i in 1:dim(complete.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = complete.xts, aes(x = time(data.xts), y = as.numeric(complete.xts[,i])), color = "black") +
    xlab('Date') +
    ylab(colnames(complete.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}

# Kick out Luxembourg_import: Only 4 values
complete.xts <- subset(complete.xts, select=-Luxembourg_import)

# p-values for tests (+differences)
complete.pvals_tests <- data.frame(matrix(ncol = 7, nrow = length(colnames(complete.xts))))
colnames(complete.pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(complete.pvals_tests) <- colnames(complete.xts)
for (i in 1:dim(complete.pvals_tests)[1]) {
  adf <- adf.test(complete.xts[,i])
  complete.pvals_tests[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(complete.xts[,i], differences = 1)[-1])
  complete.pvals_tests[i,2] <- adf1$p.value
  
  pp <- pp.test(complete.xts[,i])
  complete.pvals_tests[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(complete.xts[,i], differences = 1)[-1])
  complete.pvals_tests[i,4] <- pp1$p.value
  
  kpss <- kpss.test(complete.xts[,i])
  complete.pvals_tests[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(complete.xts[,i], differences = 1)[-1])
  complete.pvals_tests[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(complete.xts[,i], differences = 2)[-c(1, 2)])
  complete.pvals_tests[i,7] <- kpss3$p.value
}

complete.sign.lvl_adf <- 0.05
complete.sign.lvl_pp <- 0.05
complete.sign.lvl_kpss <- 0.05
complete.stationarity <- data.frame(matrix(ncol = 7, nrow = length(colnames(complete.xts))))
colnames(complete.stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(complete.stationarity) <- colnames(complete.xts)
for (i in 1:dim(complete.stationarity)[1]) {
  if(complete.pvals_tests[i,1] < complete.sign.lvl_adf){
    complete.stationarity[i,1] <- "stat"
  } else {
    complete.stationarity[i,1] <- "non-stat"
  }
  if(complete.pvals_tests[i,2] < complete.sign.lvl_adf){
    complete.stationarity[i,2] <- "stat"
  } else {
    complete.stationarity[i,2] <- "non-stat"
  }
  if(complete.pvals_tests[i,3] < complete.sign.lvl_pp){
    complete.stationarity[i,3] <- "stat"
  } else {
    complete.stationarity[i,3] <- "non-stat"
  }
  if(complete.pvals_tests[i,4] < complete.sign.lvl_pp){
    complete.stationarity[i,4] <- "stat"
  } else {
    complete.stationarity[i,4] <- "non-stat"
  }
  if(complete.pvals_tests[i,5] < complete.sign.lvl_kpss){
    complete.stationarity[i,5] <- "non-stat"
  } else {
    complete.stationarity[i,5] <- "stat"
  }
  if(complete.pvals_tests[i,6] < complete.sign.lvl_kpss){
    complete.stationarity[i,6] <- "non-stat"
  } else {
    complete.stationarity[i,6] <- "stat"
  }
  if(complete.pvals_tests[i,7] < complete.sign.lvl_kpss){
    complete.stationarity[i,7] <- "non-stat"
  } else {
    complete.stationarity[i,7] <- "stat"
  }
}

# Differentiate the variables which are still non-stat after 1st diff (KPSS) beforehand
complete.diff <- c("NG_TTF", "COAL_API2")
for (i in complete.diff){
  complete.xts[,i] <- diff(complete.xts[,i], 1)
  colnames(complete.xts)[colnames(complete.xts) == i] <- paste0(i, ".diff")
}
# EM algorithm / Kalman filter for the NA's?
# But sample is not very small => Remove first row
complete.xts <- complete.xts[-1,]

# Estimate number of lags needed
complete.nlags <- VARselect(complete.xts, lag.max = 15, type = "const")
complete.nlags$selection 
complete.nlags <- as.numeric(complete.nlags$selection[1])
# alternative: lags.select()

# Estimate number of cointegrating vectors
complete.rank <- rank.test(VECM(complete.xts, include = "const", estim = "ML", lag = complete.nlags-1, LRinclude = "none"), cval = 0.01, type = "eigen")
complete.rank
complete.rank <- complete.rank$r
# alternative: rank.select()

# Estimating VECM with cajorls() and specified rank from
complete.coint <- ca.jo(complete.xts, type = "eigen", ecdet = "none", spec = "transitory", K = complete.nlags, dumvar = NULL)
complete.vecm <- cajorls(complete.coint, r = complete.rank)
#summary(complete.vecm$rlm)

# Normality test on residuals
complete.var <- vec2var(complete.coint, r = complete.rank)
complete.normtest <- normality.test(complete.var, multivariate.only = FALSE)

complete.pvals_res <- data.frame(matrix(ncol = 1, nrow = dim(complete.xts)[2]+1))
colnames(complete.pvals_res) <- "p-value"
rownames(complete.pvals_res) <- c(colnames(complete.xts),"Multivariate")
for (i in 1:dim(complete.xts)[2]){
  complete.pvals_res[i,1] <- as.numeric(complete.normtest$jb.uni[[paste0("resids of ", names(complete.xts)[i])]]$p.value)
}
complete.pvals_res[dim(complete.xts)[2]+1,1] <- as.numeric(complete.normtest$jb.mul$JB$p.value)

# Autocorrelation between residuals?
for (i in 1:dim(complete.normtest$resid)[2]) {
  acf2(complete.normtest$resid[,i], main = paste("ACF and PACF of residuals of", colnames(complete.xts)[i]))
}
# showes no significant auto-correlation between the residuals
# => assumption of independent and non-Gaussian residuals is not unreasonable
# => LiNGAM can be used

# TESTS [!]
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/

# Impulse responses
irf_all <- irf(complete.var)
par(ask=F)
#plot(irf_all)

# Standardize variables before DAG analysis
complete.xts.std <- scale(complete.xts)

# Procedure p. 7
# VECM 2 VAR:
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# 1: VECM
complete.std.coint <- ca.jo(complete.xts.std, type = "eigen", ecdet = "none", spec = "transitory", K = complete.nlags, dumvar = NULL)
complete.std.vecm <- cajorls(complete.std.coint, r = complete.rank)
complete.std.pi <- coefPI(complete.std.vecm)
complete.std.alpha <- coefA(complete.std.vecm)
complete.std.beta <- coefB(complete.std.vecm)
# How to get the gammas and mu? gammas has only 1 matrix (!)

# 2: VAR
complete.std.var <- vec2var(complete.std.coint, r=complete.rank)
complete.std.M_list <- complete.std.var$A # M_list has 2 matrices (since nlags=2)

# 3: Residuals of VAR
complete.std.var.res <- complete.std.var$resid

# 4: LiNGAM
complete.std.lingam <- lingam(complete.std.var.res)
#as(complete.std.lingam, "amat")
complete.std.B_0 <- t(complete.std.lingam$Bpruned) # Bpruned is transpose of adj.matr.
colnames(complete.std.B_0) <- names(complete.xts.std)
rownames(complete.std.B_0) <- names(complete.xts.std)

# 5: Matrices of lagged causal effects, B_tau
complete.std.B_list <- list()
for(i in 1:complete.nlags){
  complete.std.B_list[[i]] <- (diag(dim(complete.xts.std)[2])-complete.std.B_0)%*%as.matrix(complete.std.M_list[[i]])
}
# use a cutoff in effect size as our signifcance threshold
# Remove all effects from B_tau that are smaller in abs. value than 70% abs. value quantile of all elements in B_1
complete.std.B.threshold <- quantile(abs(complete.std.B_list[[1]]), probs = 0.87)
for(i in 1:length(complete.std.B_list)){
  complete.std.B_list[[i]][which(abs(complete.std.B_list[[i]]) < complete.std.B.threshold)] <- 0
}

# Graphical representation of B_0
complete.std.graph.B_0 <- graph_from_adjacency_matrix(
  complete.std.B_0,
  weighted = TRUE
)
visIgraph(complete.std.graph.B_0)
#saveWidget(visIgraph(complete.std.graph.B_0), file = "Plots/causaleffects_B_0.html", title = "B_0")

# Graphical representations of B_lagged
complete.std.graph.B_lagged <- lapply(1:complete.nlags, function(x) graph_from_adjacency_matrix(complete.std.B_list[[x]], weighted = TRUE))
lapply(1:complete.nlags, function(x) visIgraph(complete.std.graph.B_lagged[[x]]))
#lapply(1:complete.nlags, function(x) saveWidget(visIgraph(complete.std.graph.B_lagged[[x]]), file = paste0("Plots/causaleffects_B_",x,".html"), title = paste0("B_", x)))



# Alternative ways for graphical representation:
## convert to VisNetwork-list
test.visn <- toVisNetworkData(complete.std.graph.B_0)
## copy column "weight" to new column "value" in list "edges"
test.visn$edges$value <- test.visn$edges$weight

visNetwork(test.visn$nodes, test.visn$edges)
# tkplot()


# ----------------------
# 2nd "complete" dataset
# ----------------------

# COAL AND NG: CLEAN PRICES 
complete2.xts <- data.xts[,! names(data.xts) %in% c("EUA_price", "COAL_API2", "NG_TTF")]

# LOGIT-TRANSFORM variables with values in 0% - 100%: NG_storage
complete2.xts$NG_storage <- logit(complete2.xts$NG_storage/100)
# Is it reasonable? 

# LOG-TRANSFORM ALL STRICTLY POSITIVES
pos2 <- names(complete2.xts)[names(complete2.xts) %in% names(strictly_positives.xts)[!names(strictly_positives.xts) %in% c("NG_storage")]]
for (i in pos2){
  complete2.xts[first_nonzero[i,]:dim(complete2.xts)[1], i] <- log(complete2.xts[first_nonzero[i,]:dim(complete2.xts)[1], i])
}
# LOG-TRANSFORM ALL variables WITH ZEROS AND NEGATIVE VAL'S
# (Value + Maximum Negative value + 1)
for (i in names(complete2.xts)[!names(complete2.xts) %in% c(pos2,"NG_storage")]){
  complete2.xts[first_nonzero[i,]:dim(complete2.xts)[1], i] <- log(complete2.xts[first_nonzero[i,]:dim(complete2.xts)[1], i] +
                                                                   abs(min(data.xts[,i][data.xts[,i]<=0])) +1)
}

# Estimating seasonality components 
complete2.lms <- lapply(1:dim(complete2.xts)[2], function(x) lm(complete2.xts[first_nonzero[names(complete2.xts)[x],1]:dim(complete2.xts)[1], x] ~ sin(2*pi*seq(from = 1, to = dim(complete2.xts)[1]-first_nonzero[names(complete2.xts)[x], 1]+1, by = 1)/365.25) 
                                                              + cos(2*pi*seq(from = 1, to = dim(complete2.xts)[1]-first_nonzero[names(complete2.xts)[x],1]+1, by = 1)/365.25) 
                                                              + sin(2*pi*2*seq(from = 1, to = dim(complete2.xts)[1]-first_nonzero[names(complete2.xts)[x],1]+1, by = 1)/365.25) 
                                                              + cos(2*pi*2*seq(from = 1, to = dim(complete2.xts)[1]-first_nonzero[names(complete2.xts)[x],1]+1, by = 1)/365.25)))
complete2.fitted <- sapply(complete2.lms, fitted)
for(i in 1:dim(complete2.xts)[2]){
  if (length(complete2.fitted[[i]]) < dim(complete2.xts)[1]){
    complete2.fitted[[i]] <- c(rep(0, first_nonzero[names(complete2.xts)[i],,1]-1), complete2.fitted[[i]])
  }
}
complete2.fitted <- data.frame(complete2.fitted)
colnames(complete2.fitted) <- colnames(complete2.xts)
complete2.fitted <- xts(complete2.fitted, order.by = as.Date(data_raw[,1], "%Y-%m-%d"))

# Checking seasonality
for(i in 1:dim(complete2.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = complete2.xts, aes(x = time(complete2.xts), y = as.numeric(complete2.xts[,i])), color = "black") +
    geom_line(data = complete2.fitted, aes(x = time(complete2.xts), y = as.numeric(complete2.fitted[,i])), color = "red") +
    xlab('Date') +
    ylab(colnames(complete2.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}

# Variables which seem NOT to have seasonality:
complete2.remove <- unique(c(names(zero_infl_vars.xts), "Belgium_import", "Belgium_export", "Norway_import", "Norway_export",
                            "Poland_import", "France_export", "Netherlands_import", "Sweden_4_P_spread_to_DE", 
                            "COAL_CleanPrice", "NG_CleanPrice", "DA_Price_DE"))
for (i in names(complete2.xts[, ! names(complete2.xts) %in% complete2.remove])){
  complete2.xts[,i] <- complete2.xts[,i] - complete2.fitted[,i]
}

# Plots after deseasonalization
for(i in 1:dim(complete2.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = complete2.xts, aes(x = time(data.xts), y = as.numeric(complete2.xts[,i])), color = "black") +
    xlab('Date') +
    ylab(colnames(complete2.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}

# Kick out Luxembourg_import: Only 4 values
complete2.xts <- subset(complete2.xts, select=-Luxembourg_import)

# p-values for tests (+differences)
complete2.pvals_tests <- data.frame(matrix(ncol = 7, nrow = length(colnames(complete2.xts))))
colnames(complete2.pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(complete2.pvals_tests) <- colnames(complete2.xts)
for (i in 1:dim(complete2.pvals_tests)[1]) {
  adf <- adf.test(complete2.xts[,i])
  complete2.pvals_tests[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(complete2.xts[,i], differences = 1)[-1])
  complete2.pvals_tests[i,2] <- adf1$p.value
  
  pp <- pp.test(complete2.xts[,i])
  complete2.pvals_tests[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(complete2.xts[,i], differences = 1)[-1])
  complete2.pvals_tests[i,4] <- pp1$p.value
  
  kpss <- kpss.test(complete2.xts[,i])
  complete2.pvals_tests[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(complete2.xts[,i], differences = 1)[-1])
  complete2.pvals_tests[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(complete2.xts[,i], differences = 2)[-c(1, 2)])
  complete2.pvals_tests[i,7] <- kpss3$p.value
}

complete2.sign.lvl_adf <- 0.05
complete2.sign.lvl_pp <- 0.05
complete2.sign.lvl_kpss <- 0.05
complete2.stationarity <- data.frame(matrix(ncol = 7, nrow = length(colnames(complete2.xts))))
colnames(complete2.stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(complete2.stationarity) <- colnames(complete2.xts)
for (i in 1:dim(complete2.stationarity)[1]) {
  if(complete2.pvals_tests[i,1] < complete2.sign.lvl_adf){
    complete2.stationarity[i,1] <- "stat"
  } else {
    complete2.stationarity[i,1] <- "non-stat"
  }
  if(complete2.pvals_tests[i,2] < complete2.sign.lvl_adf){
    complete2.stationarity[i,2] <- "stat"
  } else {
    complete2.stationarity[i,2] <- "non-stat"
  }
  if(complete2.pvals_tests[i,3] < complete2.sign.lvl_pp){
    complete2.stationarity[i,3] <- "stat"
  } else {
    complete2.stationarity[i,3] <- "non-stat"
  }
  if(complete2.pvals_tests[i,4] < complete2.sign.lvl_pp){
    complete2.stationarity[i,4] <- "stat"
  } else {
    complete2.stationarity[i,4] <- "non-stat"
  }
  if(complete2.pvals_tests[i,5] < complete2.sign.lvl_kpss){
    complete2.stationarity[i,5] <- "non-stat"
  } else {
    complete2.stationarity[i,5] <- "stat"
  }
  if(complete2.pvals_tests[i,6] < complete2.sign.lvl_kpss){
    complete2.stationarity[i,6] <- "non-stat"
  } else {
    complete2.stationarity[i,6] <- "stat"
  }
  if(complete2.pvals_tests[i,7] < complete2.sign.lvl_kpss){
    complete2.stationarity[i,7] <- "non-stat"
  } else {
    complete2.stationarity[i,7] <- "stat"
  }
}

# Differentiate the variables which are still non-stat after 1st diff (KPSS) beforehand
complete2.diff <- c("Coal_CleanPrice", "NG_CleanPrice")
for (i in complete2.diff){
  complete2.xts[,i] <- diff(complete2.xts[,i], 1)
  colnames(complete2.xts)[colnames(complete2.xts) == i] <- paste0(i, ".diff")
}
# EM algorithm / Kalman filter for the NA's?
# But sample is not very small => Remove first row
complete2.xts <- complete2.xts[-1,]

# Estimate number of lags needed
complete2.nlags <- VARselect(complete2.xts, lag.max = 15, type = "const")
complete2.nlags$selection 
complete2.nlags <- as.numeric(complete2.nlags$selection[1])
# alternative: lags.select()

# Estimate number of cointegrating vectors
complete2.rank <- rank.test(VECM(complete2.xts, include = "const", estim = "ML", lag = complete2.nlags-1, LRinclude = "none"), cval = 0.01, type = "eigen")
complete2.rank
complete2.rank <- complete2.rank$r

# Estimating VECM with cajorls() and specified rank from
complete2.coint <- ca.jo(complete2.xts, type = "eigen", ecdet = "none", spec = "transitory", K = complete2.nlags, dumvar = NULL)
complete2.vecm <- cajorls(complete2.coint, r = complete2.rank)
#summary(complete2.vecm$rlm)

# Normality test on residuals
complete2.var <- vec2var(complete2.coint, r=complete2.rank)
complete2.normtest <- normality.test(complete2.var, multivariate.only = FALSE)

complete2.pvals_res <- data.frame(matrix(ncol = 1, nrow = dim(complete2.xts)[2]+1))
colnames(complete2.pvals_res) <- "p-value"
rownames(complete2.pvals_res) <- c(colnames(complete2.xts),"Multivariate")
for (i in 1:dim(complete2.xts)[2]){
  complete2.pvals_res[i,1] <- as.numeric(complete2.normtest$jb.uni[[paste0("resids of ", names(complete2.xts)[i])]]$p.value)
}
complete2.pvals_res[dim(complete2.xts)[2]+1,1] <- as.numeric(complete2.normtest$jb.mul$JB$p.value)

# Autocorrelation between residuals?
for (i in 1:dim(complete2.normtest$resid)[2]) {
  acf2(complete2.normtest$resid[,i], main = paste("ACF and PACF of residuals of", colnames(complete2.xts)[i]))
}
# showes no significant auto-correlation between the residuals
# => assumption of independent and non-Gaussian residuals is not unreasonable
# => LiNGAM can be used

# TESTS [!]
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/


# Standardize variables before DAG analysis
complete2.xts.std <- scale(complete2.xts)

# Procedure p. 7
# VECM 2 VAR:
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# 1: VECM
complete2.std.coint <- ca.jo(complete2.xts.std, type = "eigen", ecdet = "none", spec = "transitory", K = 4, dumvar = NULL)
# WHY IS K=2 NOT POSSIBLE FOR STANDARDIZED DATA?
complete2.std.vecm <- cajorls(complete2.std.coint, r = complete2.rank)
complete2.std.pi <- coefPI(complete2.std.vecm)
complete2.std.alpha <- coefA(complete2.std.vecm)
complete2.std.beta <- coefB(complete2.std.vecm)
# How to get the gammas and mu? gammes has only 1 matrix (!)

# 2: VAR
complete2.std.var <- vec2var(complete2.std.coint, r=complete2.rank)
complete2.std.M_list <- complete2.std.var$A # M_list hast 2 matrices (since nlags=2)

# 3: Residuals of VAR
complete2.std.var.res <- complete2.std.var$resid

# 4: LiNGAM
complete2.std.lingam <- lingam(complete2.std.var.res)
#as(complete.std.lingam, "amat")
complete2.std.B_0 <- t(complete2.std.lingam$Bpruned) # Bpruned is transpose of adj.matr.
colnames(complete2.std.B_0) <- names(complete2.xts.std)
rownames(complete2.std.B_0) <- names(complete2.xts.std)

# 5: Matrices of lagged causal effects, B_tau
complete2.std.B_list <- list()
for(i in 1:complete2.nlags){
  complete2.std.B_list[[i]] <- (diag(dim(complete2.xts.std)[2])-complete2.std.B_0)%*%as.matrix(complete2.std.M_list[[i]])
}
# use a cutoff in effect size as our signifcance threshold
# Remove all effects from B_tau that are smaller in abs. value than 70% abs. value quantile of all elements in B_1
complete2.std.B.threshold <- quantile(abs(complete2.std.B_list[[1]]), probs = 0.87)
for(i in 1:length(complete2.std.B_list)){
  complete2.std.B_list[[i]][which(abs(complete2.std.B_list[[i]]) < complete2.std.B.threshold)] <- 0
}


# Graphical representation of B_0
complete2.std.graph.B_0 <- graph_from_adjacency_matrix(
  complete2.std.B_0,
  weighted = TRUE
)
visIgraph(complete2.std.graph.B_0)
#saveWidget(visIgraph(complete2.std.graph.B_0), file = "Plots/causaleffects_B_0.html", title = "B_0")


# ----------------------
# 3rd "complete" dataset
# ----------------------

# COAL AND NG: CLEAN PRICES AND EUA_price AND COAL_API2 AND NG_TTF
complete3.xts <- data.xts

# LOGIT-TRANSFORM variables with values in 0% - 100%: NG_storage
complete3.xts$NG_storage <- logit(complete3.xts$NG_storage/100)
# Is it reasonable? 

# LOG-TRANSFORM ALL STRICTLY POSITIVES
pos3 <- names(complete3.xts)[names(complete3.xts) %in% names(strictly_positives.xts)[!names(strictly_positives.xts) %in% c("NG_storage")]]
for (i in pos3){
  complete3.xts[first_nonzero[i,]:dim(complete3.xts)[1], i] <- log(complete3.xts[first_nonzero[i,]:dim(complete3.xts)[1], i])
}
# LOG-TRANSFORM ALL variables WITH ZEROS AND NEGATIVE VAL'S
# (Value + Maximum Negative value + 1)
for (i in names(complete3.xts)[!names(complete3.xts) %in% c(pos3,"NG_storage")]){
  complete3.xts[first_nonzero[i,]:dim(complete3.xts)[1], i] <- log(complete3.xts[first_nonzero[i,]:dim(complete3.xts)[1], i] +
                                                                   abs(min(data.xts[,i][data.xts[,i]<=0])) +1)
}

# Estimating seasonality components 
complete3.lms <- lapply(1:dim(complete3.xts)[2], function(x) lm(complete3.xts[first_nonzero[names(complete3.xts)[x],1]:dim(complete3.xts)[1], x] ~ sin(2*pi*seq(from = 1, to = dim(complete3.xts)[1]-first_nonzero[names(complete3.xts)[x], 1]+1, by = 1)/365.25) 
                                                                + cos(2*pi*seq(from = 1, to = dim(complete3.xts)[1]-first_nonzero[names(complete3.xts)[x],1]+1, by = 1)/365.25) 
                                                                + sin(2*pi*2*seq(from = 1, to = dim(complete3.xts)[1]-first_nonzero[names(complete3.xts)[x],1]+1, by = 1)/365.25) 
                                                                + cos(2*pi*2*seq(from = 1, to = dim(complete3.xts)[1]-first_nonzero[names(complete3.xts)[x],1]+1, by = 1)/365.25)))
complete3.fitted <- sapply(complete3.lms, fitted)
for(i in 1:dim(complete3.xts)[2]){
  if (length(complete3.fitted[[i]]) < dim(complete3.xts)[1]){
    complete3.fitted[[i]] <- c(rep(0, first_nonzero[names(complete3.xts)[i],,1]-1), complete3.fitted[[i]])
  }
}
complete3.fitted <- data.frame(complete3.fitted)
colnames(complete3.fitted) <- colnames(complete3.xts)
complete3.fitted <- xts(complete3.fitted, order.by = as.Date(data_raw[,1], "%Y-%m-%d"))

# Checking seasonality
for(i in 1:dim(complete3.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = complete3.xts, aes(x = time(complete3.xts), y = as.numeric(complete3.xts[,i])), color = "black") +
    geom_line(data = complete3.fitted, aes(x = time(complete3.xts), y = as.numeric(complete3.fitted[,i])), color = "red") +
    xlab('Date') +
    ylab(colnames(complete3.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}


# Variables which seem NOT to have seasonality:
complete3.remove <- unique(c(names(zero_infl_vars.xts), "Belgium_import", "Belgium_export", "Norway_import", "Norway_export",
                             "Poland_import", "France_export", "Netherlands_import", "Sweden_4_P_spread_to_DE", 
                             "COAL_CleanPrice", "NG_CleanPrice", "DA_Price_DE", "COAL_API2", "EUA_price", "NG_TTF"))
for (i in names(complete3.xts[, ! names(complete3.xts) %in% complete3.remove])){
  complete3.xts[,i] <- complete3.xts[,i] - complete3.fitted[,i]
}

# Plots after deseasonalization
for(i in 1:dim(complete3.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = complete3.xts, aes(x = time(data.xts), y = as.numeric(complete3.xts[,i])), color = "black") +
    xlab('Date') +
    ylab(colnames(complete3.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}

# Kick out Luxembourg_import: Only 4 values
complete3.xts <- subset(complete3.xts, select=-Luxembourg_import)

# p-values for tests (+differences)
complete3.pvals_tests <- data.frame(matrix(ncol = 7, nrow = length(colnames(complete3.xts))))
colnames(complete3.pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(complete3.pvals_tests) <- colnames(complete3.xts)
for (i in 1:dim(complete3.pvals_tests)[1]) {
  adf <- adf.test(complete3.xts[,i])
  complete3.pvals_tests[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(complete3.xts[,i], differences = 1)[-1])
  complete3.pvals_tests[i,2] <- adf1$p.value
  
  pp <- pp.test(complete3.xts[,i])
  complete3.pvals_tests[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(complete3.xts[,i], differences = 1)[-1])
  complete3.pvals_tests[i,4] <- pp1$p.value
  
  kpss <- kpss.test(complete3.xts[,i])
  complete3.pvals_tests[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(complete3.xts[,i], differences = 1)[-1])
  complete3.pvals_tests[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(complete3.xts[,i], differences = 2)[-c(1, 2)])
  complete3.pvals_tests[i,7] <- kpss3$p.value
}

complete3.sign.lvl_adf <- 0.05
complete3.sign.lvl_pp <- 0.05
complete3.sign.lvl_kpss <- 0.05
complete3.stationarity <- data.frame(matrix(ncol = 7, nrow = length(colnames(complete3.xts))))
colnames(complete3.stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(complete3.stationarity) <- colnames(complete3.xts)
for (i in 1:dim(complete3.stationarity)[1]) {
  if(complete3.pvals_tests[i,1] < complete3.sign.lvl_adf){
    complete3.stationarity[i,1] <- "stat"
  } else {
    complete3.stationarity[i,1] <- "non-stat"
  }
  if(complete3.pvals_tests[i,2] < complete3.sign.lvl_adf){
    complete3.stationarity[i,2] <- "stat"
  } else {
    complete3.stationarity[i,2] <- "non-stat"
  }
  if(complete3.pvals_tests[i,3] < complete3.sign.lvl_pp){
    complete3.stationarity[i,3] <- "stat"
  } else {
    complete3.stationarity[i,3] <- "non-stat"
  }
  if(complete3.pvals_tests[i,4] < complete3.sign.lvl_pp){
    complete3.stationarity[i,4] <- "stat"
  } else {
    complete3.stationarity[i,4] <- "non-stat"
  }
  if(complete3.pvals_tests[i,5] < complete3.sign.lvl_kpss){
    complete3.stationarity[i,5] <- "non-stat"
  } else {
    complete3.stationarity[i,5] <- "stat"
  }
  if(complete3.pvals_tests[i,6] < complete3.sign.lvl_kpss){
    complete3.stationarity[i,6] <- "non-stat"
  } else {
    complete3.stationarity[i,6] <- "stat"
  }
  if(complete3.pvals_tests[i,7] < complete3.sign.lvl_kpss){
    complete3.stationarity[i,7] <- "non-stat"
  } else {
    complete3.stationarity[i,7] <- "stat"
  }
}

# Differentiate the variables which are still non-stat after 1st diff (KPSS) beforehand
complete3.diff <- c("Coal_CleanPrice", "NG_CleanPrice", "COAL_API2", "NG_TTF")
for (i in complete3.diff){
  complete3.xts[,i] <- diff(complete3.xts[,i], 1)
  colnames(complete3.xts)[colnames(complete3.xts) == i] <- paste0(i, ".diff")
}
# EM algorithm / Kalman filter for the NA's?
# But sample is not very small => Remove first row
complete3.xts <- complete3.xts[-1,]

# Estimate number of lags needed
complete3.nlags <- VARselect(complete3.xts, lag.max = 15, type = "const")
complete3.nlags$selection 
complete3.nlags <- as.numeric(complete3.nlags$selection[1])
# alternative: lags.select()

# Estimate number of cointegrating vectors
complete3.rank <- rank.test(VECM(complete3.xts, include = "const", estim = "ML", lag = complete3.nlags-1, LRinclude = "none"), cval = 0.01, type = "eigen")
complete3.rank
complete3.rank <- complete3.rank$r

# Estimating VECM with cajorls() and specified rank from
complete3.coint <- ca.jo(complete3.xts, type = "eigen", ecdet = "none", spec = "transitory", K = complete3.nlags, dumvar = NULL)
complete3.vecm <- cajorls(complete3.coint, r = complete3.rank)
#summary(complete3.vecm$rlm)

# Normality test on residuals
complete3.var <- vec2var(complete3.coint, r=complete3.rank)
complete3.normtest <- normality.test(complete3.var, multivariate.only = FALSE)

complete3.pvals_res <- data.frame(matrix(ncol = 1, nrow = dim(complete3.xts)[2]+1))
colnames(complete3.pvals_res) <- "p-value"
rownames(complete3.pvals_res) <- c(colnames(complete3.xts),"Multivariate")
for (i in 1:dim(complete3.xts)[2]){
  complete3.pvals_res[i,1] <- as.numeric(complete3.normtest$jb.uni[[paste0("resids of ", names(complete3.xts)[i])]]$p.value)
}
complete3.pvals_res[dim(complete3.xts)[2]+1,1] <- as.numeric(complete3.normtest$jb.mul$JB$p.value)

# Autocorrelation between residuals?
for (i in 1:dim(complete3.normtest$resid)[2]) {
  acf2(complete3.normtest$resid[,i], main = paste("ACF and PACF of residuals of", colnames(complete3.xts)[i]))
}
# showes no significant auto-correlation between the residuals
# => assumption of independent and non-Gaussian residuals is not unreasonable
# => LiNGAM can be used

# TESTS [!]
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/


# Standardize variables before DAG analysis
complete3.xts.std <- scale(complete3.xts)

# Procedure p. 7
# VECM 2 VAR:
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# 1: VECM
complete3.std.coint <- ca.jo(complete3.xts.std, type = "eigen", ecdet = "none", spec = "transitory", K = 4, dumvar = NULL)
# WHY IS K=2 NOT POSSIBLE FOR STANDARDIZED DATA?
complete3.std.vecm <- cajorls(complete3.std.coint, r = complete3.rank)
complete3.std.pi <- coefPI(complete3.std.vecm)
complete3.std.alpha <- coefA(complete3.std.vecm)
complete3.std.beta <- coefB(complete3.std.vecm)
# How to get the gammas and mu? gammes has only 1 matrix (!)

# 2: VAR
complete3.std.var <- vec2var(complete3.std.coint, r=complete3.rank)
complete3.std.M_list <- complete3.std.var$A # M_list hast 2 matrices (since nlags=2)

# 3: Residuals of VAR
complete3.std.var.res <- complete3.std.var$resid

# 4: LiNGAM
complete3.std.lingam <- lingam(complete3.std.var.res)
#as(complete.std.lingam, "amat")
complete3.std.B_0 <- t(complete3.std.lingam$Bpruned) # Bpruned is transpose of adj.matr.
colnames(complete3.std.B_0) <- names(complete3.xts.std)
rownames(complete3.std.B_0) <- names(complete3.xts.std)

# 5: Matrices of lagged causal effects, B_tau
complete3.std.B_list <- list()
for(i in 1:complete3.nlags){
  complete3.std.B_list[[i]] <- (diag(dim(complete3.xts.std)[2])-complete3.std.B_0)%*%as.matrix(complete3.std.M_list[[i]])
}
# use a cutoff in effect size as our signifcance threshold
# Remove all effects from B_tau that are smaller in abs. value than 70% abs. value quantile of all elements in B_1
complete3.std.B.threshold <- quantile(abs(complete3.std.B_list[[1]]), probs = 0.87)
for(i in 1:length(complete3.std.B_list)){
  complete3.std.B_list[[i]][which(abs(complete3.std.B_list[[i]]) < complete3.std.B.threshold)] <- 0
}


# Graphical representation of B_0
complete3.std.graph.B_0 <- graph_from_adjacency_matrix(
  complete3.std.B_0,
  weighted = TRUE
)
visIgraph(complete3.std.graph.B_0)
#saveWidget(visIgraph(complete3.std.graph.B_0), file = "Plots/causaleffects_B_0.html", title = "B_0")









# --------------------------------------
# Dataset w/o variables "starting later"
# --------------------------------------
same_start.xts <- complete.xts[, ! names(complete.xts) %in% rownames(first_nonzero)[first_nonzero$`First nonzero index` >2]]
same_start.xts <- scale(same_start.xts)

# Estimate number of lags needed
same_start.nlags <- VARselect(same_start.xts, lag.max = 15, type = "const")
same_start.nlags$selection 
same_start.nlags <- as.numeric(same_start.nlags$selection[1])
# alternative: lags.select()

# Estimate number of cointegrating vectors
same_start.rank <- rank.test(VECM(same_start.xts, include = "const", estim = "ML", lag = same_start.nlags-1, LRinclude = "none"), cval = 0.01, type = "eigen")
same_start.rank
same_start.rank <- same_start.rank$r

# Procedure p. 7
# VECM 2 VAR:
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# 1: VECM
same_start.coint <- ca.jo(same_start.xts, type = "eigen", ecdet = "none", spec = "transitory", K = same_start.nlags, dumvar = NULL)
same_start.vecm <- cajorls(same_start.coint, r = same_start.rank)
same_start.pi <- coefPI(same_start.vecm)
same_start.alpha <- coefA(same_start.vecm)
same_start.beta <- coefB(same_start.vecm)
# How to get the gammas and mu? gammes has only 1 matrix (!)

# 2: VAR
same_start.var <- vec2var(same_start.coint, r=same_start.rank)
same_start.M_list <- same_start.var$A # M_list hast 2 matrices (since nlags=2)

# 3: Residuals of VAR
same_start.var.res <- same_start.var$resid

# 4: LiNGAM
same_start.lingam <- lingam(same_start.var.res)
#as(same_start.lingam, "amat")
same_start.B_0 <- t(same_start.lingam$Bpruned) # Bpruned is transpose of adj.matr.
colnames(same_start.B_0) <- names(same_start.xts)
rownames(same_start.B_0) <- names(same_start.xts)

# 5: Matrices of lagged causal effects, B_tau
same_start.B_list <- list()
for(i in 1:complete.nlags){
  same_start.B_list[[i]] <- (diag(dim(same_start.xts)[2])-same_start.B_0)%*%as.matrix(same_start.M_list[[i]])
}
# use a cutoff in effect size as our signifcance threshold
# Remove all effects from B_tau that are smaller in abs. value than 70% abs. value quantile of all elements in B_1
same_start.B.threshold <- quantile(abs(same_start.B_list[[1]]), probs = 0.87)
for(i in 1:length(same_start.B_list)){
  same_start.B_list[[i]][which(abs(same_start.B_list[[i]]) < same_start.B.threshold)] <- 0
}


# Graphical representation of B_0
same_start.graph.B_0 <- graph_from_adjacency_matrix(
  same_start.B_0,
  weighted = TRUE
)
visIgraph(same_start.graph.B_0)
#saveWidget(visIgraph(same_start.graph.B_0), file = "Plots/causaleffects_B_0.html", title = "B_0")




# ---------------------------------------------------------
# Dataset w/o variables "starting later" and w/o zero-infl.
# ---------------------------------------------------------
same_start_no_zi.xts <- complete.xts[, ! names(complete.xts) %in% rownames(first_nonzero)[first_nonzero$`First nonzero index` >2]]
same_start_no_zi.xts <- complete.xts[, ! names(complete.xts) %in% names(zero_infl_vars.xts)]

same_start_no_zi.xts <- scale(same_start_no_zi.xts)

# Estimate number of lags needed
same_start_no_zi.nlags <- VARselect(same_start_no_zi.xts, lag.max = 15, type = "const")
same_start_no_zi.nlags$selection 
same_start_no_zi.nlags <- as.numeric(same_start_no_zi.nlags$selection[1])

# Estimate number of cointegrating vectors
same_start_no_zi.rank <- rank.test(VECM(same_start_no_zi.xts, include = "const", estim = "ML", lag = same_start_no_zi.nlags-1, LRinclude = "none"), cval = 0.01, type = "eigen")
same_start_no_zi.rank
same_start_no_zi.rank <- same_start_no_zi.rank$r

# Procedure p. 7
# VECM 2 VAR:
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# 1: VECM
same_start_no_zi.coint <- ca.jo(same_start_no_zi.xts, type = "eigen", ecdet = "none", spec = "transitory", K = same_start_no_zi.nlags, dumvar = NULL)
same_start_no_zi.vecm <- cajorls(same_start_no_zi.coint, r = same_start_no_zi.rank)
same_start_no_zi.pi <- coefPI(same_start_no_zi.vecm)
same_start_no_zi.alpha <- coefA(same_start_no_zi.vecm)
same_start_no_zi.beta <- coefB(same_start_no_zi.vecm)
# How to get the gammas and mu? gammes has only 1 matrix (!)

# 2: VAR
same_start_no_zi.var <- vec2var(same_start_no_zi.coint, r=same_start_no_zi.rank)
same_start_no_zi.M_list <- same_start_no_zi.var$A # M_list hast 2 matrices (since nlags=2)

# 3: Residuals of VAR
same_start_no_zi.res <- same_start_no_zi.var$resid

# 4: LiNGAM
same_start_no_zi.lingam <- lingam(same_start_no_zi.res)
#as(same_start.lingam, "amat")
same_start_no_zi.B_0 <- t(same_start_no_zi.lingam$Bpruned) # Bpruned is transpose of adj.matr.
colnames(same_start_no_zi.B_0) <- names(same_start_no_zi.xts)
rownames(same_start_no_zi.B_0) <- names(same_start_no_zi.xts)

# 5: Matrices of lagged causal effects, B_tau
same_start_no_zi.B_list <- list()
for(i in 1:same_start_no_zi.nlags){
  same_start_no_zi.B_list[[i]] <- (diag(dim(same_start_no_zi.xts)[2])-same_start_no_zi.B_0)%*%as.matrix(same_start_no_zi.M_list[[i]])
}
# use a cutoff in effect size as our signifcance threshold
# Remove all effects from B_tau that are smaller in abs. value than 70% abs. value quantile of all elements in B_1
same_start_no_zi.B.threshold <- quantile(abs(same_start_no_zi.B_list[[1]]), probs = 0.87)
for(i in 1:length(same_start_no_zi.B_list)){
  same_start_no_zi.B_list[[i]][which(abs(same_start_no_zi.B_list[[i]]) < same_start_no_zi.B.threshold)] <- 0
}


# Graphical representation of B_0
same_start_no_zi.graph.B_0 <- graph_from_adjacency_matrix(
  same_start_no_zi.B_0,
  weighted = TRUE
)
visIgraph(same_start_no_zi.graph.B_0)





# ----------------------
# Stationary-only subset
# ----------------------
# We can directly use VAR-Model
# Where do all tests agree in stationarity?
# ADF == PP == KPSS == stat
subset_1.xts <- data.xts[, rownames(stationarity)[which(stationarity$ADF=="stat" & stationarity$PP=="stat" & stationarity$KPSS=="stat")]]
names(subset_1.xts)
plot(subset_1.xts)
stack(as.data.frame(subset_1.xts)) %>% ggplot(aes(x = ind, y = values, fill = ind)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Variables') +
  ylab('Values') +
  labs(fill = "Variables")
# Log-transform not possible (<=0!)
# remove Luxembourg_import: only 4 datapoints
subset_1.xts <- subset(subset_1.xts, select = -Luxembourg_import)
corrplot(cor(subset_1.xts), type = "upper", method = "color",
         insig = "pch", tl.cex = 0.8, tl.col = "black", tl.srt = 45)
# Seasonally adjusting by subtracting seasonal term
subset_1.lms <- lapply(1:dim(subset_1.xts)[2], function(x) lm(subset_1.xts[first_nonzero[x,1]:dim(data.xts)[1],x] ~ sin(2*pi*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25) 
                                                                       + cos(2*pi*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25) 
                                                                       + sin(2*pi*2*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25) 
                                                                       + cos(2*pi*2*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25)))
subset_1.fitted <- sapply(subset_1.lms, fitted)
for(i in 1:dim(subset_1.xts)[2]){
  if (length(subset_1.fitted[[i]]) < dim(data.xts)[1]){
    subset_1.fitted[[i]] <- c(rep(0, first_nonzero[i,1]-1), subset_1.fitted[[i]])
  }
}
subset_1.fitted <- data.frame(subset_1.fitted)
colnames(subset_1.fitted) <- colnames(subset_1.xts)
subset_1.fitted <- xts(subset_1.fitted, order.by = index(data.xts))

for(i in 1:dim(subset_1.xts)[2]) {  
  pl <- ggplot() + 
    geom_line(data = subset_1.xts, aes(x = time(data.xts), y = as.numeric(subset_1.xts[,i])), color = "black") +
    geom_line(data = subset_1.fitted, aes(x = time(data.xts), y = as.numeric(subset_1.fitted[,i])), color = "red") +
    xlab('Date') +
    ylab(colnames(subset_1.xts)[i])
  print(pl)
}

# Is there seasonality?
# NOT: Sweden_import, Sweden_export, forecast_load
remove <- c("Sweden_import", "Sweden_export", "forecast_load")
for (i in names(subset_1.xts[, ! names(subset_1.xts) %in% remove])){
  subset_1.xts[,i] <- subset_1.xts[,i] - fitted.subset_1[,i]
}

# p-values for tests (+differences)
subset_1.pvals_tests <- data.frame(matrix(ncol = 7, nrow = length(colnames(subset_1.xts))))
colnames(subset_1.pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(subset_1.pvals_tests) <- colnames(subset_1.xts)
for (i in 1:dim(subset_1.pvals_tests)[1]) {
  adf <- adf.test(subset_1.xts[,i])
  subset_1.pvals_tests[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(subset_1.xts[,i], differences = 1)[-1])
  subset_1.pvals_tests[i,2] <- adf1$p.value
  
  pp <- pp.test(subset_1.xts[,i])
  subset_1.pvals_tests[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(subset_1.xts[,i], differences = 1)[-1])
  subset_1.pvals_tests[i,4] <- pp1$p.value
  
  kpss <- kpss.test(subset_1.xts[,i])
  subset_1.pvals_tests[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(subset_1.xts[,i], differences = 1)[-1])
  subset_1.pvals_tests[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(subset_1.xts[,i], differences = 2)[-c(1, 2)])
  subset_1.pvals_tests[i,7] <- kpss3$p.value
}


subset_1.sign.lvl_adf <- 0.05
subset_1.sign.lvl_pp <- 0.05
subset_1.sign.lvl_kpss <- 0.05
subset_1.stationarity <- data.frame(matrix(ncol = 7, nrow = length(colnames(subset_1.xts))))
colnames(subset_1.stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(subset_1.stationarity) <- colnames(subset_1.xts)
for (i in 1:dim(subset_1.stationarity)[1]) {
  if(subset_1.pvals_tests[i,1] < subset_1.sign.lvl_adf){
    subset_1.stationarity[i,1] <- "stat"
  } else {
    subset_1.stationarity[i,1] <- "non-stat"
  }
  if(subset_1.pvals_tests[i,2] < subset_1.sign.lvl_adf){
    subset_1.stationarity[i,2] <- "stat"
  } else {
    subset_1.stationarity[i,2] <- "non-stat"
  }
  if(subset_1.pvals_tests[i,3] < subset_1.sign.lvl_pp){
    subset_1.stationarity[i,3] <- "stat"
  } else {
    subset_1.stationarity[i,3] <- "non-stat"
  }
  if(subset_1.pvals_tests[i,4] < subset_1.sign.lvl_pp){
    subset_1.stationarity[i,4] <- "stat"
  } else {
    subset_1.stationarity[i,4] <- "non-stat"
  }
  if(subset_1.pvals_tests[i,5] < subset_1.sign.lvl_kpss){
    subset_1.stationarity[i,5] <- "non-stat"
  } else {
    subset_1.stationarity[i,5] <- "stat"
  }
  if(subset_1.pvals_tests[i,6] < subset_1.sign.lvl_kpss){
    subset_1.stationarity[i,6] <- "non-stat"
  } else {
    subset_1.stationarity[i,6] <- "stat"
  }
  if(subset_1.pvals_tests[i,7] < subset_1.sign.lvl_kpss){
    subset_1.stationarity[i,7] <- "non-stat"
  } else {
    subset_1.stationarity[i,7] <- "stat"
  }
}

# optimal lag order of the subset-VAR 
subset_1.lags <- VARselect(subset_1.xts, lag.max = 25, type = "const")
subset_1.lags$selection 
subset_1.lags <- as.numeric(subset_1.lags$selection[1])

# Estimating VAR
subset_1.VAR <- VAR(subset_1.xts, p = subset_1.lags, type = "const")

# Testing for Gaussianity in residuals of estimated VAR
subset_1.normtest <- normality.test(subset_1.VAR, multivariate.only = FALSE)
subset_1.pvals_res <- data.frame(matrix(ncol = 1, nrow = dim(subset_1.xts)[2]+1))
colnames(subset_1.pvals_res) <- "p-value"
rownames(subset_1.pvals_res) <- c(colnames(subset_1.xts), "Multivariate")
subset_1.pvals_res[1,1] <- as.numeric(subset_1.normtest$jb.uni$forecast_load$p.value)
subset_1.pvals_res[2,1] <- as.numeric(subset_1.normtest$jb.uni$Temperature$p.value)
subset_1.pvals_res[3,1] <- as.numeric(subset_1.normtest$jb.uni$DewPoint$p.value)
subset_1.pvals_res[4,1] <- as.numeric(subset_1.normtest$jb.uni$Wind_Speed$p.value)
subset_1.pvals_res[5,1] <- as.numeric(subset_1.normtest$jb.uni$Wind_Direction$p.value)
subset_1.pvals_res[6,1] <- as.numeric(subset_1.normtest$jb.uni$Slovenia_P_spread_to_DE$p.value)
subset_1.pvals_res[7,1] <- as.numeric(subset_1.normtest$jb.uni$Hungary_P_spread_to_DE$p.value)
subset_1.pvals_res[8,1] <- as.numeric(subset_1.normtest$jb.uni$Sweden_export$p.value)
subset_1.pvals_res[9,1] <- as.numeric(subset_1.normtest$jb.uni$Sweden_import$p.value)
subset_1.pvals_res[10,1] <- as.numeric(subset_1.normtest$jb.uni$biomass_production$p.value)
subset_1.pvals_res[11,1] <- as.numeric(subset_1.normtest$jb.mul$JB$p.value)
# All values <.05 => residuals not Gaussian

# Autocorrelation between residuals
for (i in 1:dim(subset_1.normtest$resid)[2]) {
  acf2(subset_1.normtest$resid[,i], main = paste("ACF and PACF of residuals of", colnames(subset_1.xts)[i]))
}
# showes no significant auto-correlation between the residuals
# => assumption of independent and non-Gaussian residuals is not unreasonable
# => LiNGAM can be used

# LiNGAM
# Note: res$Bpruned is transpose of adjacency matrix
subset_1.res <- residuals(subset_1.VAR)
subset_1.lingam <- lingam(subset_1.res)
as(subset_1.lingam, "amat")
subset_1.B_null <- t(subset_1.lingam$Bpruned)
colnames(subset_1.B_null) <- names(subset_1.xts)
rownames(subset_1.B_null) <- names(subset_1.xts)

# Plotting the results
subset_1.graph_inst <- graph_from_adjacency_matrix(
  subset_1.B_null,
  weighted = TRUE
)
plot.igraph(subset_1.graph_inst, layout=layout.reingold.tilford, edge.color = "grey53", 
            edge.arrow.size=0.01, vertex.size = 15, vertex.label.color="black", 
            vertex.label.dist=3.5, vertex.color="tomato", vertex.label.cex = 0.5, 
            edge.label = round(E(subset_1.graph_inst)$weight,3), edge.label.cex = 0.45, edge.label.color = "brown")

# Coefficient matrices VAR
subset_1.varcoeffs <- data.frame(Bcoef(subset_1.VAR))
subset_1.mu <- subset_1.varcoeffs[,"const"]
subset_1.varcoeffs <- subset(subset_1.varcoeffs, select = -const)
subset_1.M_list <- list()
for(i in 1:subset_1.lags){
  subset_1.M_list[[i]] <- subset_1.varcoeffs[, (i*dim(subset_1.xts)[2]-dim(subset_1.xts)[2]+1):(i*dim(subset_1.xts)[2])]
}
subset_1.B_list <- list()
for(i in 1:subset_1.lags){
  subset_1.B_list[[i]] <- (diag(dim(subset_1.xts)[2])-subset_1.B_null)%*%as.matrix(subset_1.M_list[[i]])
}

# Plotting lagged causal effects
# The smallest effects have been removed.
for(i in 1:length(subset_1.B_list)){
  subset_1.B_list[[i]][which(abs(subset_1.B_list[[i]]) < 0.2)] <- 0
}
subset_1.lagged_graphs <- lapply(1:subset_1.lags, function(x) graph_from_adjacency_matrix(subset_1.B_list[[x]], weighted = TRUE))
lapply(1:subset_1.lags, function(x) plot.igraph(subset_1.lagged_graphs[[x]], layout=layout.reingold.tilford, edge.color = "grey53", 
                                                edge.arrow.size=0.01, vertex.size = 15, vertex.label.color="black", vertex.label.dist=3.5, 
                                                vertex.color="tomato", vertex.label.cex = 0.5, edge.label = round(E(subset_1.lagged_graphs[[x]])$weight,3), 
                                                edge.label.cex = 0.45, edge.label.color = "brown"))
# Graph plots:
# https://bookdown.org/markhoff/social_network_analysis/network-visualization-and-aesthetics.html



# ---------------------------------------------
# Variables which are stationary after one diff
# ---------------------------------------------
subset_2.xts <- data.xts[, c("DA_Price_DE", "forecast_residual_load", "Temperature", "GHI", "solar_production")]
names(subset_2.xts)
plot(subset_2.xts)
stack(as.data.frame(subset_2.xts)) %>% ggplot(aes(x = ind, y = values, fill = ind)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Variables') +
  ylab('Values') +
  labs(fill = "Variables")
# Log-transform not possible (<=0!)
corrplot(cor(subset_2.xts), type = "upper", method = "color",
         insig = "pch", tl.cex = 0.8, tl.col = "black", tl.srt = 45)
# Seasonally adjusting by subtracting seasonal term
subset_2.lms <- lapply(1:dim(subset_2.xts)[2], function(x) lm(subset_2.xts[first_nonzero[x,1]:dim(data.xts)[1],x] ~ sin(2*pi*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25) 
                                                             + cos(2*pi*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25) 
                                                             + sin(2*pi*2*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25) 
                                                             + cos(2*pi*2*seq(from = 1, to = dim(data.xts)[1]-first_nonzero[x,1]+1, by = 1)/365.25)))
subset_2.fitted <- sapply(subset_2.lms, fitted)
for(i in 1:dim(subset_2.xts)[2]){
  if (length(subset_2.fitted[[i]]) < dim(data.xts)[1]){
    subset_2.fitted[[i]] <- c(rep(0, first_nonzero[i,1]-1), subset_2.fitted[[i]])
  }
}
subset_2.fitted <- data.frame(subset_2.fitted)
colnames(subset_2.fitted) <- colnames(subset_2.xts)
subset_2.fitted <- xts(subset_2.fitted, order.by = index(data.xts))

for(i in 1:dim(subset_2.xts)[2]) {  
  pl <- ggplot() + 
    geom_line(data = subset_2.xts, aes(x = time(data.xts), y = as.numeric(subset_2.xts[,i])), color = "black") +
    geom_line(data = subset_2.fitted, aes(x = time(data.xts), y = as.numeric(subset_2.fitted[,i])), color = "red") +
    xlab('Date') +
    ylab(colnames(subset_2.xts)[i])
  print(pl)
}

# Is there any seasonality?
# Yes: all
subset_2.xts <- subset_2.xts - subset_2.fitted

# p-values for tests (+differences)
subset_2.pvals_tests <- data.frame(matrix(ncol = 7, nrow = length(colnames(subset_2.xts))))
colnames(subset_2.pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(subset_2.pvals_tests) <- colnames(subset_2.xts)
for (i in 1:dim(subset_2.pvals_tests)[1]) {
  adf <- adf.test(subset_2.xts[,i])
  subset_2.pvals_tests[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(subset_2.xts[,i], differences = 1)[-1])
  subset_2.pvals_tests[i,2] <- adf1$p.value
  
  pp <- pp.test(subset_2.xts[,i])
  subset_2.pvals_tests[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(subset_2.xts[,i], differences = 1)[-1])
  subset_2.pvals_tests[i,4] <- pp1$p.value
  
  kpss <- kpss.test(subset_2.xts[,i])
  subset_2.pvals_tests[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(subset_2.xts[,i], differences = 1)[-1])
  subset_2.pvals_tests[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(subset_2.xts[,i], differences = 2)[-c(1, 2)])
  subset_2.pvals_tests[i,7] <- kpss3$p.value
}


subset_2.sign.lvl_adf <- 0.05
subset_2.sign.lvl_pp <- 0.05
subset_2.sign.lvl_kpss <- 0.05
subset_2.stationarity <- data.frame(matrix(ncol = 7, nrow = length(colnames(subset_2.xts))))
colnames(subset_2.stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(subset_2.stationarity) <- colnames(subset_2.xts)
for (i in 1:dim(subset_2.stationarity)[1]) {
  if(subset_2.pvals_tests[i,1] < subset_2.sign.lvl_adf){
    subset_2.stationarity[i,1] <- "stat"
  } else {
    subset_2.stationarity[i,1] <- "non-stat"
  }
  if(subset_2.pvals_tests[i,2] < subset_2.sign.lvl_adf){
    subset_2.stationarity[i,2] <- "stat"
  } else {
    subset_2.stationarity[i,2] <- "non-stat"
  }
  if(subset_2.pvals_tests[i,3] < subset_2.sign.lvl_pp){
    subset_2.stationarity[i,3] <- "stat"
  } else {
    subset_2.stationarity[i,3] <- "non-stat"
  }
  if(subset_2.pvals_tests[i,4] < subset_2.sign.lvl_pp){
    subset_2.stationarity[i,4] <- "stat"
  } else {
    subset_2.stationarity[i,4] <- "non-stat"
  }
  if(subset_2.pvals_tests[i,5] < subset_2.sign.lvl_kpss){
    subset_2.stationarity[i,5] <- "non-stat"
  } else {
    subset_2.stationarity[i,5] <- "stat"
  }
  if(subset_2.pvals_tests[i,6] < subset_2.sign.lvl_kpss){
    subset_2.stationarity[i,6] <- "non-stat"
  } else {
    subset_2.stationarity[i,6] <- "stat"
  }
  if(subset_2.pvals_tests[i,7] < subset_2.sign.lvl_kpss){
    subset_2.stationarity[i,7] <- "non-stat"
  } else {
    subset_2.stationarity[i,7] <- "stat"
  }
}


# optimal lag order of the subset-VAR 
# VARselect heavily depends on lag.max 
# https://stats.stackexchange.com/questions/187289/var-lag-selection-heavily-depends-on-maximum-lag-investigated
# https://stats.stackexchange.com/questions/399772/aic-bic-values-keep-changing-with-lag-max-in-var-model
subset_2.lags <- VARselect(subset_2.xts, lag.max = 12, type = "const")
subset_2.lags$selection 
subset_2.lags <- as.numeric(subset_2.lags$selection[1])
# optimal order of lags for each individual variable instead of together
#lapply(subset_2.xts, VARselect)

# different function (don't know the exact difference but leads in general to different results...)
# lag length + rank
rk_sel <- lags.select(subset_2.xts, lag.max = 12)
summary(rk_sel)


# Johansen Procedure for VAR / Cointegration
# !! Critical values are only reported for systems with less than 11 variables and are taken from Osterwald-Lenum
# spec = "transitory" leads to the VECM meant in the paper (see ?ca.jo)
# ecdet = "const" for constant term in cointegration (intercept)

# https://www.r-econometrics.com/timeseries/vecintro/
subset_2.coint <- ca.jo(subset_2.xts, type = "trace", ecdet = "none", spec = "transitory", K = subset_2.lags, dumvar = NULL)
#summary(subset_2.coint)
# most important results
subset_2.beta <- subset_2.coint@V
subset_2.alpha <- subset_2.coint@W
subset_2.gammas <- subset_2.coint@GAMMA
# PI = Error-correction-term (matrix)
subset_2.pi <- subset_2.alpha%*%t(subset_2.beta) # is the same as subset_2.coint@PI
cbind(subset_2.coint@teststat, subset_2.coint@cval)
# Interpretation:
# r=0 tests for presence of cointegration
# test statistic for r=0 exceeds 1% sign.lvl. we have strong evidence to reject H_0 of no cointegration
# But: for sign.lvl. 0.01 we're not able to reject H_0: r<=4 
# => 4 cointegrated vectors at a 1% significance level
# A cointegrating vector is a stationary linear combination of possibly nonstationary vector time-series components
# https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/

# Estimating VECM with cajorls() and specified rank from teststatistic
subset_2.vecm <- cajorls(subset_2.coint, r = 4)
summary(subset_2.vecm$rlm)
# Coeffs (should be normalized)
subset_2.vecm$rlm$coefficients

# Check if cointegrated time series we get are stationary
# normalized cointegration vectors
subset_2.vecm$beta
# (1st) cointegration vector
subset_2.coint_ts1 <- subset_2.vecm$beta[1,1]*subset_2.xts$DA_Price_DE + subset_2.vecm$beta[2,1]*subset_2.xts$forecast_residual_load +
  subset_2.vecm$beta[3,1]*subset_2.xts$Temperature + subset_2.vecm$beta[4,1]*subset_2.xts$GHI + subset_2.vecm$beta[5,1]*subset_2.xts$solar_production +
  subset_2.vecm$beta[6,1]
adf.test(subset_2.coint_ts1)
plot(subset_2.coint_ts1)
# (2nd) cointegration vector
subset_2.coint_ts2 <- subset_2.vecm$beta[1,2]*subset_2.xts$DA_Price_DE + subset_2.vecm$beta[2,2]*subset_2.xts$forecast_residual_load +
  subset_2.vecm$beta[3,2]*subset_2.xts$Temperature + subset_2.vecm$beta[4,2]*subset_2.xts$GHI + subset_2.vecm$beta[5,2]*subset_2.xts$solar_production +
  subset_2.vecm$beta[6,2]
adf.test(subset_2.coint_ts2)
plot(subset_2.coint_ts2)
# (3rd) cointegration vector
subset_2.coint_ts3 <- subset_2.vecm$beta[1,3]*subset_2.xts$DA_Price_DE + subset_2.vecm$beta[2,3]*subset_2.xts$forecast_residual_load +
  subset_2.vecm$beta[3,3]*subset_2.xts$Temperature + subset_2.vecm$beta[4,3]*subset_2.xts$GHI + subset_2.vecm$beta[5,3]*subset_2.xts$solar_production +
  subset_2.vecm$beta[6,3]
adf.test(subset_2.coint_ts3)
plot(subset_2.coint_ts3)
# (4th) cointegration vector
subset_2.coint_ts4 <- subset_2.vecm$beta[1,4]*subset_2.xts$DA_Price_DE + subset_2.vecm$beta[2,4]*subset_2.xts$forecast_residual_load +
  subset_2.vecm$beta[3,4]*subset_2.xts$Temperature + subset_2.vecm$beta[4,4]*subset_2.xts$GHI + subset_2.vecm$beta[5,4]*subset_2.xts$solar_production +
  subset_2.vecm$beta[6,4]
adf.test(subset_2.coint_ts4)
plot(subset_2.coint_ts4)
# All 4 are stationary
# But 4th seems to be the same as GHI. Why?

# Maybe in the other setting one of the variables itself was one of the cointegrating vectors
# -> test?!
# Or would one directly see it since then one of the vectors in beta must be a unit vector??
# MAYBE IT WAS B/C ONE OF THE VARS WERE NOT STATIONARY AFTER 1 DIFF

# Alternative: Estimating VECM with VECM()-function
#VECM_VECM <- VECM(subset_2.xts, lag = no_lags_subset-1, r = 1, estim = "ML")
# NOTE: VECM(): lag = k-1
#summary(VECM_VECM)
#residuals <- VECM_VECM$residuals

# normality.test() computes univariate and multivariate Jarque-Bera tests for residuals of VECM
# To use normality.test() we need to estimate vec2var
# (restricted VECM)
# the VAR representation of a VECM from ca.jo
subset_2.VAR <- vec2var(subset_2.coint, r=4)
subset_2.normtest <- normality.test(subset_2.VAR, multivariate.only = FALSE)
subset_2.pvals_res <- data.frame(matrix(ncol = 1, nrow = dim(subset_2.xts)[2]+1))
colnames(subset_2.pvals_res) <- "p-value"
rownames(subset_2.pvals_res) <- c(colnames(subset_2.xts),"Multivariate")
subset_2.pvals_res[1,1] <- as.numeric(subset_2.normtest$jb.uni$`resids of DA_Price_DE`$p.value)
subset_2.pvals_res[2,1] <- as.numeric(subset_2.normtest$jb.uni$`resids of forecast_residual_load`$p.value)
subset_2.pvals_res[3,1] <- as.numeric(subset_2.normtest$jb.uni$`resids of Temperature`$p.value)
subset_2.pvals_res[4,1] <- as.numeric(subset_2.normtest$jb.uni$`resids of GHI`$p.value)
subset_2.pvals_res[5,1] <- as.numeric(subset_2.normtest$jb.uni$`resids of solar_production`$p.value)
subset_2.pvals_res[6,1] <- as.numeric(subset_2.normtest$jb.mul$JB$p.value)
# All values <.05 => residuals not normally distr. (also not multivariate normal)
# [Geht bestimmt schöner zu coden..]

# Autocorrelation between residuals?
for (i in 1:dim(subset_2.normtest$resid)[2]) {
  acf2(subset_2.normtest$resid[,i], main = paste("ACF and PACF of residuals of", colnames(subset_2.xts)[i]))
}
# showes no significant auto-correlation between the residuals
# => assumption of independent and non-Gaussian residuals is not unreasonable
# => LiNGAM can be used

# weak exogeneity test
# BUILD IT FROM HERE: https://stackoverflow.com/questions/64289992/vecm-in-r-testing-weak-exogeneity-and-imposing-restrictions
# restriction matrix:
DA <- matrix(c(1,0,0,0,0,1,0,0), c(4,2))
exogeneity_test <- alrtest(subset_2.coint, A=DA, r=4)
summary(exogeneity_test)


# exclusion test
# for more info on test: Juselius (2006)


# Obtain Impulse-response-function
#ir <- irf(subset_2.VAR, n.ahead = 20, runs = 500)
#plot(ir)


# standardize time series
subset_2.xts_std <- scale(subset_2.xts)
plot(subset_2.xts_std)





# LiNGAM analysis
# Note: res$Bpruned is transpose of adjacency matrix
subset_2.res <- subset_2.VAR$resid
subset_2.lingam <- lingam(subset_2.res)
as(subset_2.lingam, "amat")
subset_2.B_null <- t(subset_2.lingam$Bpruned)
colnames(subset_2.B_null) <- names(subset_2.xts)
rownames(subset_2.B_null) <- names(subset_2.xts)

# B_0: The instantaneous causal effects
# Plotting the results
subset_2.graph_inst <- graph_from_adjacency_matrix(
  subset_2.B_null,
  weighted = TRUE
)
plot.igraph(subset_2.graph_inst, layout=layout.reingold.tilford, edge.color = "grey53", 
            edge.arrow.size=0.01, vertex.size = 15, vertex.label.color="black", 
            vertex.label.dist=3.5, vertex.color="tomato", vertex.label.cex = 0.5, 
            edge.label = round(E(subset_2.graph_inst)$weight,3), edge.label.cex = 0.45, edge.label.color = "brown")

# Coefficient matrices VAR
subset_2.mu <- subset_2.VAR$deterministic
subset_2.M_list <- subset_2.VAR$A
subset_2.B_list <- list()
for(i in 1:subset_2.lags){
  subset_2.B_list[[i]] <- (diag(dim(subset_2.xts)[2])-subset_2.B_null)%*%as.matrix(subset_2.M_list[[i]])
}

# Plotting lagged causal effects
# The smallest effects have been removed.
for(i in 1:length(subset_2.B_list)){
  subset_2.B_list[[i]][which(abs(subset_2.B_list[[i]]) < 0.2)] <- 0
}
subset_2.lagged_graphs <- lapply(1:subset_2.lags, function(x) graph_from_adjacency_matrix(subset_2.B_list[[x]], weighted = TRUE))
lapply(1:subset_2.lags, function(x) plot.igraph(subset_2.lagged_graphs[[x]], layout=layout.reingold.tilford, edge.color = "grey53", 
                                                 edge.arrow.size=0.01, vertex.size = 15, vertex.label.color="black", vertex.label.dist=3.5, 
                                                 vertex.color="tomato", vertex.label.cex = 0.5, edge.label = round(E(subset_2.lagged_graphs[[x]])$weight,3), 
                                                 edge.label.cex = 0.45, edge.label.color = "brown"))

# Plotting lagged causal effects
# The smallest effects have been removed.
for(i in 1:length(subset_1.B_list)){
  subset_1.B_list[[i]][which(abs(subset_1.B_list[[i]]) < 0.2)] <- 0
}
subset_1.lagged_graphs <- lapply(1:subset_1.lags, function(x) graph_from_adjacency_matrix(subset_1.B_list[[x]], weighted = TRUE))
lapply(1:subset_1.lags, function(x) plot.igraph(subset_1.lagged_graphs[[x]], layout=layout.reingold.tilford, edge.color = "grey53", 
                                                edge.arrow.size=0.01, vertex.size = 15, vertex.label.color="black", vertex.label.dist=3.5, 
                                                vertex.color="tomato", vertex.label.cex = 0.5, edge.label = round(E(subset_1.lagged_graphs[[x]])$weight,3), 
                                                edge.label.cex = 0.45, edge.label.color = "brown"))



# NEXT STEPS:
# Tests from paper (cointegration one of the ts, weak exogeneity, ...)










# ---------------------------------------
# Dataset without zero-inflated variables
# ---------------------------------------
no_zeroinfl.xts <- complete.xts[ , -which(names(complete.xts) %in% names(zero_infl_vars.xts))]
# w/o the ones starting later:
#no_zeroinfl.xts <- complete.xts[ , -which(rownames(first_nonzero)[first_nonzero$`First nonzero index` != 1] %in% names(zero_infl_vars.xts))]

# Estimate number of lags needed
no_zeroinfl.nlags <- VARselect(no_zeroinfl.xts, lag.max = 5, type = "const")
no_zeroinfl.nlags$selection 
no_zeroinfl.nlags <- as.numeric(no_zeroinfl.nlags$selection[1])
# alternative: lags.select()

# Estimate number of cointegrating vectors
no_zeroinfl.rank <- rank.test(VECM(no_zeroinfl.xts, include = "const", estim = "ML", lag = no_zeroinfl.nlags-1, LRinclude = "none"), cval = 0.01, type = "eigen")
no_zeroinfl.rank
no_zeroinfl.rank <- no_zeroinfl.rank$r

# Estimating VECM with cajorls() and specified rank from
no_zeroinfl.coint <- ca.jo(no_zeroinfl.xts, type = "eigen", ecdet = "none", spec = "transitory", K = no_zeroinfl.nlags, dumvar = NULL)
no_zeroinfl.vecm <- cajorls(no_zeroinfl.coint, r = no_zeroinfl.rank)
#summary(no_zeroinfl.vecm$rlm)

# Normality test on residuals
no_zeroinfl.var <- vec2var(no_zeroinfl.coint, r=no_zeroinfl.rank)
no_zeroinfl.normtest <- normality.test(no_zeroinfl.var, multivariate.only = FALSE)

no_zeroinfl.pvals_res <- data.frame(matrix(ncol = 1, nrow = dim(no_zeroinfl.xts)[2]+1))
colnames(no_zeroinfl.pvals_res) <- "p-value"
rownames(no_zeroinfl.pvals_res) <- c(colnames(no_zeroinfl.xts),"Multivariate")
for (i in 1:dim(no_zeroinfl.xts)[2]){
  no_zeroinfl.pvals_res[i,1] <- as.numeric(no_zeroinfl.normtest$jb.uni[[paste0("resids of ", names(no_zeroinfl.xts)[i])]]$p.value)
}
no_zeroinfl.pvals_res[dim(no_zeroinfl.xts)[2]+1,1] <- as.numeric(no_zeroinfl.normtest$jb.mul$JB$p.value)

# Autocorrelation between residuals?
for (i in 1:dim(no_zeroinfl.normtest$resid)[2]) {
  acf2(no_zeroinfl.normtest$resid[,i], main = paste("ACF and PACF of residuals of", colnames(no_zeroinfl.xts)[i]))
}
# showes no significant auto-correlation between the residuals
# => assumption of independent and non-Gaussian residuals is not unreasonable
# => LiNGAM can be used

# TESTS [!]
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/

# Impulse responses
no_zeroinfl.var.irf_all <- irf(no_zeroinfl.var)
par(ask=F)
#plot(irf_all)

# Standardize variables before DAG analysis
no_zeroinfl.xts.std <- scale(no_zeroinfl.xts)
#no_zeroinfl.xts.std <- no_zeroinfl.xts

# Procedure p. 7
# VECM 2 VAR:
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# 1: VECM
no_zeroinfl.std.coint <- ca.jo(no_zeroinfl.xts.std, type = "eigen", ecdet = "none", spec = "transitory", K = no_zeroinfl.nlags, dumvar = NULL)
no_zeroinfl.std.vecm <- cajorls(no_zeroinfl.std.coint, r = no_zeroinfl.rank)
no_zeroinfl.std.pi <- coefPI(no_zeroinfl.std.vecm)
no_zeroinfl.std.alpha <- coefA(no_zeroinfl.std.vecm)
no_zeroinfl.std.beta <- coefB(no_zeroinfl.std.vecm)
# How to get the gammas and mu? gammes has only 1 matrix (!)

# 2: VAR
no_zeroinfl.std.var <- vec2var(no_zeroinfl.std.coint, r=no_zeroinfl.rank)
no_zeroinfl.std.M_list <- no_zeroinfl.std.var$A # M_list hast 2 matrices (since nlags=2)

# 3: Residuals of VAR
no_zeroinfl.std.var.res <- no_zeroinfl.std.var$resid

# 4: LiNGAM
no_zeroinfl.std.lingam <- lingam(no_zeroinfl.std.var.res)
#as(no_zeroinfl.std.lingam, "amat")
no_zeroinfl.std.B_0 <- t(no_zeroinfl.std.lingam$Bpruned) # Bpruned is transpose of adj.matr.
colnames(no_zeroinfl.std.B_0) <- names(no_zeroinfl.xts.std)
rownames(no_zeroinfl.std.B_0) <- names(no_zeroinfl.xts.std)

# 5: Matrices of lagged causal effects, B_tau
no_zeroinfl.std.B_list <- list()
for(i in 1:no_zeroinfl.nlags){
  no_zeroinfl.std.B_list[[i]] <- (diag(dim(no_zeroinfl.xts.std)[2])-no_zeroinfl.std.B_0)%*%as.matrix(no_zeroinfl.std.M_list[[i]])
}
# use a cutoff in effect size as our signifcance threshold
# Remove all effects from B_tau that are smaller in abs. value than 70% abs. value quantile of all elements in B_1
no_zeroinfl.std.B.threshold <- quantile(abs(no_zeroinfl.std.B_list[[1]]), probs = 0.87)
for(i in 1:length(no_zeroinfl.std.B_list)){
  no_zeroinfl.std.B_list[[i]][which(abs(no_zeroinfl.std.B_list[[i]]) < no_zeroinfl.std.B.threshold)] <- 0
}


# Graphical representation of B_0
no_zeroinfl.std.graph.B_0 <- graph_from_adjacency_matrix(
  no_zeroinfl.std.B_0,
  weighted = TRUE
)
visIgraph(no_zeroinfl.std.graph.B_0)









###---------------------------------------------------------

# Plot to PDF
pdf(file = "Plots/My Plot.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches
# Or:
#png("my_plot.png", 600, 600)
plot.igraph(data.graph_inst, layout=layout.reingold.tilford, edge.color = "grey53", 
            edge.arrow.size=0.01, vertex.size = 15, vertex.label.color="black", 
            vertex.label.dist=3.5, vertex.color="tomato", vertex.label.cex = 0.5, 
            edge.label = round(E(data.graph_inst)$weight,3), edge.label.cex = 0.45, edge.label.color = "brown")

dev.off()



# ---------------------------
# Playing around with subsets
# ---------------------------

#testset.xts <- data.xts[,c("NG_TTF", "COAL_API2", "Temperature", "DewPoint", "Humidity",
#                           "Wind_Speed", "GHI", "wind_offshore_production", "wind_onshore_production",
#                           "solar_production", "gas_production")]
testset.xts <- strictly_positives.xts

# LOGIT-TRANSFORM 
testset.xts$NG_storage <- logit(testset.xts$NG_storage/100)

# LOG-TRANSFORM
for (i in names(testset.xts)[!names(testset.xts) %in% c("NG_storage")]){
  testset.xts[first_nonzero[i,]:dim(testset.xts)[1], i] <- log(testset.xts[first_nonzero[i,]:dim(testset.xts)[1], i])
}


# Estimating seasonality components 
testset.lms <- lapply(1:dim(testset.xts)[2], function(x) lm(testset.xts[first_nonzero[names(testset.xts)[x],1]:dim(testset.xts)[1], x] ~ sin(2*pi*seq(from = 1, to = dim(testset.xts)[1]-first_nonzero[names(testset.xts)[x], 1]+1, by = 1)/365.25) 
                                                              + cos(2*pi*seq(from = 1, to = dim(testset.xts)[1]-first_nonzero[names(testset.xts)[x],1]+1, by = 1)/365.25) 
                                                              + sin(2*pi*2*seq(from = 1, to = dim(testset.xts)[1]-first_nonzero[names(testset.xts)[x],1]+1, by = 1)/365.25) 
                                                              + cos(2*pi*2*seq(from = 1, to = dim(testset.xts)[1]-first_nonzero[names(testset.xts)[x],1]+1, by = 1)/365.25)))
testset.fitted <- sapply(testset.lms, fitted)
for(i in 1:dim(testset.xts)[2]){
  if (length(testset.fitted[[i]]) < dim(testset.xts)[1]){
    testset.fitted[[i]] <- c(rep(0, first_nonzero[names(testset.xts)[i],1]-1), testset.fitted[[i]])
  }
}
testset.fitted <- data.frame(testset.fitted)
colnames(testset.fitted) <- colnames(testset.xts)
testset.fitted <- xts(testset.fitted, order.by = as.Date(data_raw[,1], "%Y-%m-%d"))

# Checking seasonality
for(i in 1:dim(testset.xts)[2]) {  
  abc <- ggplot() + 
    geom_line(data = testset.xts, aes(x = time(testset.xts), y = as.numeric(testset.xts[,i])), color = "black") +
    geom_line(data = testset.fitted, aes(x = time(testset.xts), y = as.numeric(testset.fitted[,i])), color = "red") +
    xlab('Date') +
    ylab(colnames(testset.xts)[i]) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  print(abc)
}

# Kick out Luxembourg_export, starts later:
testset.xts <- subset(testset.xts, select=-Luxembourg_export)

# Variables which seem NOT to have seasonality:
testset.remove <- unique(c(names(zero_infl_vars.xts), "COAL_API2", "EUA_price", "NG_TTF"))
for (i in names(testset.xts[, ! names(testset.xts) %in% testset.remove])){
  testset.xts[,i] <- testset.xts[,i] - testset.fitted[,i]
}

# Estimate number of lags needed
testset.nlags <- VARselect(testset.xts, lag.max = 10, type = "const")
testset.nlags$selection 
testset.nlags <- as.numeric(testset.nlags$selection[1])

# p-values for tests (+differences)
testset.pvals_tests <- data.frame(matrix(ncol = 7, nrow = length(colnames(testset.xts))))
colnames(testset.pvals_tests) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(testset.pvals_tests) <- colnames(testset.xts)
for (i in 1:dim(testset.pvals_tests)[1]) {
  adf <- adf.test(testset.xts[,i])
  testset.pvals_tests[i,1] <- adf$p.value
  # removing first value to get no NA's
  adf1 <- adf.test(diff(testset.xts[,i], differences = 1)[-1])
  testset.pvals_tests[i,2] <- adf1$p.value
  
  pp <- pp.test(testset.xts[,i])
  testset.pvals_tests[i,3] <- pp$p.value
  # removing first value to get no NA's
  pp1 <- pp.test(diff(testset.xts[,i], differences = 1)[-1])
  testset.pvals_tests[i,4] <- pp1$p.value
  
  kpss <- kpss.test(testset.xts[,i])
  testset.pvals_tests[i,5] <- kpss$p.value
  # removing first value to get no NA's
  kpss2 <- kpss.test(diff(testset.xts[,i], differences = 1)[-1])
  testset.pvals_tests[i,6] <- kpss2$p.value
  # removing also second value to get no NA's
  kpss3 <- kpss.test(diff(testset.xts[,i], differences = 2)[-c(1, 2)])
  testset.pvals_tests[i,7] <- kpss3$p.value
}

testset.sign.lvl_adf <- 0.05
testset.sign.lvl_pp <- 0.05
testset.sign.lvl_kpss <- 0.05
testset.stationarity <- data.frame(matrix(ncol = 7, nrow = length(colnames(testset.xts))))
colnames(testset.stationarity) <- c("ADF", "ADF (1st diff)", "PP", "PP (1st diff)", "KPSS", "KPSS (1st diff)", "KPSS (2nd diff)")
rownames(testset.stationarity) <- colnames(testset.xts)
for (i in 1:dim(testset.stationarity)[1]) {
  if(testset.pvals_tests[i,1] < testset.sign.lvl_adf){
    testset.stationarity[i,1] <- "stat"
  } else {
    testset.stationarity[i,1] <- "non-stat"
  }
  if(testset.pvals_tests[i,2] < testset.sign.lvl_adf){
    testset.stationarity[i,2] <- "stat"
  } else {
    testset.stationarity[i,2] <- "non-stat"
  }
  if(testset.pvals_tests[i,3] < testset.sign.lvl_pp){
    testset.stationarity[i,3] <- "stat"
  } else {
    testset.stationarity[i,3] <- "non-stat"
  }
  if(testset.pvals_tests[i,4] < testset.sign.lvl_pp){
    testset.stationarity[i,4] <- "stat"
  } else {
    testset.stationarity[i,4] <- "non-stat"
  }
  if(testset.pvals_tests[i,5] < testset.sign.lvl_kpss){
    testset.stationarity[i,5] <- "non-stat"
  } else {
    testset.stationarity[i,5] <- "stat"
  }
  if(testset.pvals_tests[i,6] < testset.sign.lvl_kpss){
    testset.stationarity[i,6] <- "non-stat"
  } else {
    testset.stationarity[i,6] <- "stat"
  }
  if(testset.pvals_tests[i,7] < testset.sign.lvl_kpss){
    testset.stationarity[i,7] <- "non-stat"
  } else {
    testset.stationarity[i,7] <- "stat"
  }
}

# Differentiate the variables which are still non-stat after 1st diff (KPSS) beforehand
testset.diff <- c("NG_TTF", "COAL_API2")
for (i in testset.diff){
  testset.xts[,i] <- diff(testset.xts[,i], 1)
  colnames(testset.xts)[colnames(testset.xts) == i] <- paste0(i, ".diff")
}
# EM algorithm / Kalman filter for the NA's?
# But sample is not very small => Remove first row
testset.xts <- testset.xts[-1,]

# Estimate number of cointegrating vectors
testset.rank <- rank.test(VECM(testset.xts, include = "none", estim = "ML", lag = testset.nlags-1, LRinclude = "none"), cval = 0.01, type = "eigen")
testset.rank
testset.rank <- testset.rank$r


# Standardize data
testset.xts <- scale(testset.xts)


# Estimating VECM with cajorls() and specified rank from
testset.coint <- ca.jo(testset.xts, type = "eigen", ecdet = "const", spec = "transitory", K = testset.nlags, dumvar = NULL)
testset.vecm <- cajorls(testset.coint, r = testset.rank)

# VAR
testset.var <- vec2var(testset.coint, r=testset.rank)

fitsvar <- xts(fitted(testset.var), order.by=index(data.xts)[2:(dim(data.xts)[1]-testset.nlags+1)])
plot(fitsvar[,9])

resids <- xts(residuals(testset.var), order.by=index(data.xts)[2:(dim(data.xts)[1]-testset.nlags+1)])
lines(resids[,9], col = "red")


# 3: Residuals of VAR
testset.var.res <- testset.var$resid

# 4: LiNGAM
testset.lingam <- lingam(testset.var.res)
testset.B_0 <- t(testset.lingam$Bpruned) # Bpruned is transpose of adj.matr.
colnames(testset.B_0) <- names(testset.xts)
rownames(testset.B_0) <- names(testset.xts)

# Graphical representation of B_0
testset.graph.B_0 <- graph_from_adjacency_matrix(
  testset.B_0,
  weighted = TRUE
)
visIgraph(testset.graph.B_0)


# ----------------------------------  
# Plotting fitted vals and residuals
# ----------------------------------

x <- xts(fitted(complete.std.var), order.by = index(data.xts)[2:2479])
y <- xts(complete.std.var$resid, order.by = index(data.xts)[2:2479])
plot(complete.xts.std[,41])
lines(x[,41], col = "red")
lines(y[,41], col = "blue")
