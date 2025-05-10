# SETTINGS
nRegions <- 10 # number of regions
nYears <- 10 # number of years
sigmaSmall <- 0.1 # within cluster standard deviation
sigmaLarge <- 1 # standard deviation of cluster means
nReps <- 1000 # number of repetitions of simulation run
set.seed(0)


# PREDICTOR
# draw a random predictor vector, one entry for each region, and copy these values for each year and add some slight variation
basePredictor <- rnorm(nRegions, sd = sigmaLarge)
predictor <- matrix(rep(basePredictor, times = nYears), nrow=nRegions) + rnorm(nRegions*nYears, sd = sigmaSmall)
predictorCorrelationMatrixOfYears <- cor(predictor)
predictorCorrelationMatrixOfRegions <- cor(t(predictor))
# predictor's mean correlation coefficient between different years
mean(predictorCorrelationMatrixOfYears[upper.tri(predictorCorrelationMatrixOfYears)])
# predictor's  mean correlation coefficient between different regions
mean(predictorCorrelationMatrixOfRegions[upper.tri(predictorCorrelationMatrixOfRegions)])
# -> predictor is highly correlated between different years, but there is almost no correlation between different regions
# -> high temporal correlation, almost no spatial correlation in predictor

# COEFFICIENTS AND TARGET
betaTrue <- c(0, 1)
yTrue <- betaTrue[1] + betaTrue[2] * predictor


# SIMULATION

regionId <- as.vector(row(yTrue))
yearId <- as.vector(col(yTrue))

# The following command takes a few seconds.
simulationResults <- replicate(nReps, {
  noise <- rep(rnorm(nYears, sd = sigmaLarge), each = nRegions) + rnorm(nRegions*nYears, sd = sigmaSmall)
  yObserved <- yTrue + noise
  fit <- lm(as.vector(yObserved) ~ as.vector(predictor))
  residuals <- matrix(fit$residuals, nrow = nrow(yObserved))
  residualCorrelationMatrixOfYears <- cor(residuals)
  residualCorrelationMatrixOfRegions <- cor(t(residuals))
  c(
    mean(residualCorrelationMatrixOfYears[upper.tri(residualCorrelationMatrixOfYears)]), # mean correlation coefficient between different years
    mean(residualCorrelationMatrixOfRegions[upper.tri(residualCorrelationMatrixOfRegions)]), # mean correlation coefficient between different regions
    fit$coefficients, # estimated coefficients
    sandwich::vcovCL(fit, cluster = regionId), # covariance with clustering by region
    sandwich::vcovCL(fit, cluster = yearId), # covariance with clustering by year
    sandwich::vcovHAC(fit) # covariance without clustering
  )
})



# EVALUATION

# residual's mean correlation coefficient between different years averaged over simulation repetitions
mean(simulationResults[1,])
# residual's mean correlation coefficient between different regions averaged over simulation repetitions
mean(simulationResults[2,])
# -> residuals are highly correlated between different regions, but there is almost no correlation between different years
# -> high spatial correlation, almost no temporal correlation in residuals


# Estimate the true covariance matrix of the coefficient estimator using the different simulation runs
coefficients <- t(simulationResults[3:4,])
covTrue <- var(coefficients)

# collect the different covariance estimators
covClusterByRegion <- simulationResults[5:8,]
covClusterByYear <- simulationResults[9:12,]
covNoCluster <- simulationResults[13:16,]

# mean absolute bias of covariance estimates
mean(abs(matrix(rowMeans(covClusterByRegion), nrow=2) - covTrue))
mean(abs(matrix(rowMeans(covClusterByYear), nrow=2) - covTrue)) # by far the lowest
mean(abs(matrix(rowMeans(covNoCluster), nrow=2) - covTrue))

# mean squared error of covariance estimates
mean((covClusterByRegion - as.vector(covTrue))^2)
mean((covClusterByYear - as.vector(covTrue))^2)  # by far the lowest
mean((covNoCluster - as.vector(covTrue))^2)


# CONCLUSION

# Clustering by year, as suggested by the residual correlation, is by far the best option.
# The temporal correlation of the predictor are no problem, even though we do not account for temporal correlation when clustering by year.
