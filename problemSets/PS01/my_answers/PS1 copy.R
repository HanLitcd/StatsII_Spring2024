#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

set.seed(123)
# Generate 1000 Cauchy random variables
data <- rcauchy(1000, location = 0, scale = 1)
# create empirical distribution of observed data

ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))
# Function to approximate the KS p-value using the provided series formula
ks_p_value <- function(Dx) {
  # Ensure D is positive to avoid division by zero
  if (Dx <= 0) return(1)
  
  # Constants for the calculation
  pi_sq <- pi^2
  upper_limit <- 1000 
  
  # Calculate the series sum
  series_sum <- sum(sapply(1:upper_limit, function(k) {
    exp(-(2*k - 1)^2 * pi_sq / (8 * Dx^2))
  }))
  
  # Calculate the p-value using the series sum
  p_value <- sqrt(2*pi) / Dx * series_sum
  
  # Ensure the p-value is within [0,1]
  p_value <- min(max(p_value, 0), 1)
  
  return(p_value)
}
p_value <- ks_p_value(D)
print(p_value)
#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
# Estimate OLS using lm for comparison
lm_model <- lm(y ~ x, data = data)

# Define the log-likelihood function for the OLS model
logLikFun <- function(beta, data) {
  y <- data$y
  x <- data$x
  sigma <- 1.5 # Assuming known standard deviation
  -sum(dnorm(y, mean = beta[1] + beta[2] * x, sd = sigma, log = TRUE))
}

# Optimize the log-likelihood function using BFGS method
library(stats)
start_values <- c(0, 1) # Starting values for the coefficients
bfgs_model <- optim(start_values, logLikFun, data = data, method = "BFGS")

# Output the coefficients from both models
list(lm_model = coef(lm_model), bfgs_model = bfgs_model$par)