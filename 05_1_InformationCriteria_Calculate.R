source("common.R")



# Define Functions --------------------------------------------------------


logDetTerm <- function(a, b, n) {
  log(1 + b*n/a) + log(a)*n
}
muSigmaTerm <- function(normSqr, sumSqr, a, b, n) {
  r <- 1/a
  s <- b*r*r/(1+b*r*n)
  r*normSqr - s*sumSqr
}
# negative log likelihood in constant covariance model
nll <- function(normSqr, sumSqr, simgaSqr, covariance, n) {
  a <- simgaSqr - covariance
  b <- covariance
  blockNll <- (logDetTerm(a,b,n) + muSigmaTerm(normSqr,sumSqr,a,b,n))/2
  sum(blockNll)
}

infoCrit <- \(resid, cluster, nCoef) {
  nObs <- length(resid)
  sigmaSqr <- mean(resid^2)
  normSqr <- aggregate(resid, list(cluster=cluster), \(x) sum(x^2))
  sumSqr <- aggregate(resid, list(cluster=cluster), \(x) sum(x)^2)
  n <- table(cluster)
  res <- optimize(\(x) nll(normSqr$x, sumSqr$x, sigmaSqr, x, n), c(-sigmaSqr/max(n), sigmaSqr)) 
  nllValue <- nll(normSqr$x, sumSqr$x, sigmaSqr, res$minimum, n)
  nNoiseParam <- if (all(n == nObs)) 1 else 2
  c(COV = res$minimum,
    NLL = nllValue,
    AIC = 2*(nCoef+nNoiseParam) + 2*nllValue,
    BIC = log(nObs)*(nCoef+nNoiseParam) + 2*nllValue)
}

getInfoCrit <- function(dataNoFe, target, preds, nFixedEffects, cluster) {
  frml <- formula(paste(target, "~", paste(preds, collapse = " + "), "- 1"))
  nPredictors <- length(preds)
  fit <- lm(frml, data = dataNoFe)
  resid <- fit$residuals
  nCoef <- nPredictors + nFixedEffects
  infoCrit(resid, cluster, nCoef)
}



# Main --------------------------------------------------------------------


dataNoFe <- loadRegressionData("NoFe")

nFixedEffects <- getNumFixedEffects(dataNoFe)

icData <- 
  lapply(c("Gid0Year", "Gid1Year"), \(clusterName) {
    clust <- getClusters(dataNoFe, clusterName)
    models |> 
      rowwise() |> 
      mutate(ic = list(getInfoCrit(dataNoFe, target, preds, nFixedEffects, clust))) |> 
      ungroup() |> 
      select(-c(preds, target)) |> 
      mutate(ic = as_tibble(do.call(rbind, ic))) |> 
      unnest(ic) |> 
      pivot_longer(-c(kind, termName, lag), names_to = "criterion") |> 
      mutate(clusterName = clusterName, .before = 1)
  }) |> 
  bind_rows()
  
write_csv(icData, "results/informationCriteria.csv")
