
setwd("C:\\Users\\LuisEnrique\\Documents\\MIT FALL 2018\\Financial Data Science\\Project 2")
rm(list = ls())

# Packages
library(readr)
library(tidyr) # https://tidyr.tidyverse.org/ 
library(dplyr) # required to run r function spread
library(ggplot2)
library(fpp)
library(forecast)
library(reshape2)
library(caTools)
library(moments)
#################################################################################################################
# Question 1 #
# Read data and label columns 
data <- read.csv("Query1.csv", header = TRUE, sep = ",")
# name columns 
colnames(data) <- c("Date", "Stock ID", "Log Returns")
# Use function spread to organize data by ticker on columns and date on rows
spread_dat <- spread(data, "Stock ID", "Log Returns")
# check completeness of data
print(dim(spread_dat)[1]/252) 
# Change first column data type from factor to date and order chronologically   
spread_dat <- spread_dat[order(as.Date(spread_dat$Date, "%m/%d/%Y")), ]
# Make row names dates 
rownames(spread_dat) <- spread_dat$Date
dates <- spread_dat$Date 
# Remove date column, which is the first column 
spread_dat <- spread_dat[, -1]
# Compute daily average for every row (date). Use rowsums function: https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/rowSums
d_avg <- rowSums(spread_dat)/ncol(spread_dat)
rownames(d_avg) <- spread_dat$Date
#https://stackoverflow.com/questions/3444889/how-to-use-the-sweep-function Margin = 1 indicates that goes over rows
#sweep returns an array of the same shape as the input array #apply does not
excess_r <- sweep(spread_dat, 1, d_avg, "-")
# Weights: obtain a new array with weights for each trading day across securities
weights <- sweep(excess_r, 1, -2/rowSums(abs(excess_r)), "*")
# Daily portfolio returns - 1 lag in the weight relative to stock returns
portf_returns <- rowSums(spread_dat[2:nrow(spread_dat),] * weights[1:(nrow(spread_dat)-1),])


# (a)

# Plot daily portfolio returns
# First convert to data frame to be able to plot
portf_returns <- as.data.frame(portf_returns) # convert from value to data frame
portf_returns <- cbind(dates[-1], portf_returns) # start dates the second day available to match portolio returns
colnames(portf_returns) <- c("Date", "Portfolio_Returns") # date appears as a factor and not as date format
portf_returns$Date <- as.Date(portf_returns$Date, "%m/%d/%Y")
plot1 <- ggplot(portf_returns, aes(Date, Portfolio_Returns)) + geom_line(group = 1) + labs(y = "Daily Portfolio Log returns") + labs(x = "t") + theme_bw() + theme(axis.title=element_text(face="bold.italic", 
                                                                                                                                                                                           size="12", color="blue"), legend.position="top")
# Plot daily market returns
dates <- as.Date(dates, "%m/%d/%Y")
d_avg <- as.data.frame(cbind(dates, d_avg))
colnames(d_avg) <- c("Date", "Daily_Returns")
plot2 <- ggplot(d_avg, aes(as.Date(Date), Daily_Returns)) + geom_line(group = 1) + labs(y = "Daily Market Log Returns", x = "t") + theme_bw() + theme(axis.title=element_text(face="bold.italic", 
                                                                                                                                                                                  size="12", color="blue"), legend.position="top")

#(b) annualized mean return, volatility and sharpe ratio of contrarian strategy and market
#contrarian strategy
# Annualized mean
mean <- round(mean(portf_returns[,2]) * 252,4)
# Annualized volatility
volatility <- round(sd(portf_returns[,2]) * sqrt(252),4)
# Sharpe ratio
sharpe <- round(mean / volatility,4)
print(mean)
print(volatility)
print(sharpe)


# market
# Annualizing mean
mean_market <- round(mean(d_avg[,2]) * 252,4)
# Annualizing volatility
volatility_market <- round(sd(d_avg[,2]) * sqrt(252),4)
# Sharpe ratio
sharpe <- round(mean_market / volatility_market,4)
print(mean_market)
print(volatility_market)
print(sharpe)


# (c) Consistent over time. Stationary?
#Augmented Dickey-Fuller Test:  https://www.rdocumentation.org/packages/aTSA/versions/3.1.2/topics/adf.test
adf.test(portf_returns$Portfolio_Returns)
adf.test(d_avg$Daily_Returns)
# both are non stationary
Acf(portf_returns$Portfolio_Returns, main = "Daily portfolio returns (ACF)", ylab = "%")
Pacf(portf_returns$Portfolio_Returns, main = "Daily portfolio returns (PACF)", ylab = "%")

Acf(d_avg$Daily_Returns, main = "Daily market returns (ACF)", ylab = "%")
Pacf(d_avg$Daily_Returns, main = "Daily market returns (PACF)", ylab = "%")

#https://cran.r-project.org/web/packages/caTools/caTools.pdf
# plot moving mean and sd to see if there is evidence of stationarity
mov_mean <- runmean(portf_returns$Portfolio_Returns, 252) * 252
mov_mean <- as.data.frame(cbind(dates, mov_mean))
colnames(mov_mean) <- c("Date", "Movingmean")


plot3 <- ggplot(mov_mean, aes(as.Date(Date), Movingmean)) + geom_line(group = 1) + labs(y = "Rolling mean (252 days)", x = "t") + theme_bw() + theme(axis.title=element_text(face="bold.italic", size="12", color="blue"), legend.position="top")

mov_sd <-runsd(portf_returns$Portfolio_Returns, 252) * sqrt(252)
mov_sd <- as.data.frame(cbind(dates[-1], mov_sd))
colnames(mov_sd) <- c("Date", "Movingsd")

plot4 <- ggplot(mov_sd, aes(as.Date(Date), Movingsd)) + geom_line(group = 1) + labs(y = "Rolling sd (252 days)", x = "t") + theme_bw() + theme(axis.title=element_text(face="bold.italic", size="12", color="blue"), legend.position="top")

# (d) unusual events
# descriptive analysis using boxplots, skewness and kurtosis
boxplot(portf_returns$Portfolio_Returns, main="Daily Portfolio Returns", 
        xlab="", ylab="Portfolio returns")

boxplot(d_avg$Daily_Returns, main="Daily Market Returns", 
        xlab="", ylab="Market returns")

skewness(portf_returns$Portfolio_Returns)
kurtosis(portf_returns$Portfolio_Returns)

skewness(d_avg$Daily_Returns)
kurtosis(d_avg$Daily_Returns)

# Finding outliers through time in portfolio returns
# Threshold for outliers
sigma_threshold <- round(4*sd(portf_returns$Portfolio_Returns),4)
portf_mean <- round(mean(portf_returns$Portfolio_Returns),4)
# Boolean vector indicating outliers for portfolio returns
outliers <- (abs(portf_returns$Portfolio_Returns - portf_mean) >= sigma_threshold)
# Counting number of outliers (True)
count_outliers <- sum(outliers)
# Printing outliers
outlier_table_portf <- d_avg[which(outliers),]
cat("\n Outlier stocks: More than 4 standard deviations from mean \n")
print(outlier_table_portf)
################################################################################
#(e) correlation between contrarian strategy and market return
corr_cm <- cor(portf_returns$Portfolio_Returns, d_avg$Daily_Returns[-1])
print(corr_cm)

# Beta
beta <- cov(portf_returns$Portfolio_Returns, d_avg$Daily_Returns[-1])/var(d_avg$Daily_Returns[-1])
beta2 <- corr_cm*(sd(portf_returns$Portfolio_Returns)/sd(d_avg$Daily_Returns[-1]))
# Part (f) ##############################
# correlation between long and short portfolios
long_id <- (weights > 0)   # boolean to identify long positions
short_id <- (weights < 0)  # boolean to identify short positions
long_portf_ret <- rowSums(spread_dat[2:nrow(spread_dat),] * long_id[1:(nrow(spread_dat)-1),]* weights[1:(nrow(spread_dat)-1),])
short_portf_ret <- rowSums(spread_dat[2:nrow(spread_dat),] * short_id[1:(nrow(spread_dat)-1),]* weights[1:(nrow(spread_dat)-1),])
cor_long_short <- cor(long_portf_ret, short_portf_ret)
print(cor_long_short)
##############################################################################################################





