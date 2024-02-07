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
data1 <- rcauchy(1000, location = 0, scale = 1)
# create empirical distribution of observed data


# Function to approximate the KS p-value using the provided series formula
ks_p_value <- function(data) {
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # get D
  D <- max(abs(empiricalCDF - pnorm(data)))
  # Constants for the calculation
  pi_sq <- pi^2
  upper_limit <- length(data)
  # Calculate the series sum
  series_sum <- sum(sapply(1:upper_limit, function(k) {
    exp(-(2*k - 1)^2 * pi_sq / (8 * D^2))
  }))
  
  # Calculate the p-value using the series sum
  p_value <- sqrt(2*pi) / D * series_sum
  
  # Ensure the p-value is within [0,1]
  p_value <- min(max(p_value, 0), 1)
  cat('D is',D,' with p_value of ', p_value)
  return(p_value)
}
ks_p_value(data1)
#check
ks.test(data1,'pnorm')
#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
# Estimate OLS using lm for comparison
lm_model <- lm(y ~ x, data = data)

# Define the log-likelihood function for the OLS model
logLikFun <- function(theta,y,X) {
  n <-nrow(X)
  k <-ncol(X)
  beta <- theta[1:k]
  sigma <- theta[k+1]

  -sum(dnorm(y, mean =X %*% beta , sd = sigma, log = TRUE))
}

# Optimize the log-likelihood function using BFGS method

 # Starting values for the coefficients
start_values <- c(1,1,1)
bfgs_model <- optim(fn=logLikFun, par=start_values, X=cbind(1,data$x),y=data$y, method = "BFGS",hessian = T)

# Output the coefficients from both models
list(lm_model = coef(lm_model), bfgs_model = bfgs_model$par)
