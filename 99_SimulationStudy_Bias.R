# SETTINGS
nRegions <- 10 # number of regions
nYears <- 10 # number of years
sigma <- 1 # standard deviation of noise and predictor
nReps <- 1000 # number of repetitions of simulation run
set.seed(1)


# PREDICTOR
# draw a random predictor (uncorrelated)
predictor <- matrix(rnorm(nRegions*nYears, sd = sigma), nrow=nRegions)


# COEFFICIENTS AND TARGET
betaTrue <- c(0, 1)
yTrue <- betaTrue[1] + betaTrue[2] * predictor


# SIMULATION

regionId <- as.vector(row(yTrue))
yearId <- as.vector(col(yTrue))

# The following command takes a few seconds.
simulationResults <- replicate(nReps, {
  noise <- matrix(rnorm(nRegions*nYears, sd = sigma), nrow=nRegions)
  yObserved <- yTrue + noise
  fit <- lm(as.vector(yObserved) ~ as.vector(predictor))
  c(
    fit$coefficients, # estimated coefficients
    sandwich::vcovCL(fit, cluster = regionId), # covariance with clustering by region
    sandwich::vcovCL(fit, cluster = yearId), # covariance with clustering by year
    sandwich::vcovHAC(fit) # covariance without clustering
  )
})



# EVALUATION

# Estimate the true covariance matrix of the coefficient estimator using the different simulation runs
coefficients <- t(simulationResults[1:2,])
covTrue <- var(coefficients)

# collect the different covariance estimators
covClusterByRegion <- simulationResults[3:6,]
covClusterByYear <- simulationResults[7:10,]
covNoCluster <- simulationResults[11:14,]

# mean absolute bias of covariance estimates
mean(abs(matrix(rowMeans(covClusterByRegion), nrow=2) - covTrue)) # roughly same as the others
mean(abs(matrix(rowMeans(covClusterByYear), nrow=2) - covTrue)) # roughly same as the others
mean(abs(matrix(rowMeans(covNoCluster), nrow=2) - covTrue)) # roughly same as the others

# root mean squared error of covariance estimates
sqrt(mean((covClusterByRegion - as.vector(covTrue))^2))
sqrt(mean((covClusterByYear - as.vector(covTrue))^2))
sqrt(mean((covNoCluster - as.vector(covTrue))^2))  # by far the lowest


# CONCLUSION

# We uses clustered standard errors even though the errors are drawn independently.
# The clustered methods do not show a relevant bias. But there RMSE is larger.
# => Clustering when samples are independent should be avoided as it leads to
#    uncertainty estimates of poorer quality. But it does not have a bias in one
#    or the other direction, such as being overly conservative.
