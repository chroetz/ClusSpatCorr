source("common.R")



dataNoFe <- loadRegressionData("NoFe")
dataLagged <- loadRegressionData("Lagged")



model <- models |> filter(kind == "full")
frml <- formula(paste(model$target, "~", paste(model$preds[[1]], collapse=" + "), "- 1"))
fit <- lm(frml, data = dataNoFe)


dataOut <- 
  dataNoFe |> 
  select(GID_0, GID_1, year, !!target) |> 
  mutate(
    fitted = fit$fitted.values,
    residual = fit$residuals)
write_csv(dataOut, "regressionData/regressionData_NoFe_FitFull.csv")


coeffs <- coef(fit)
coefData <- tibble(
  variable = names(coeffs),
  coefficient = coeffs)
write_csv(coefData, "results/coefficients_FitFull.csv")



rSquared <- function(target, preds, dataNoFe, data) {
  frml <- formula(paste(target, "~", paste(preds, collapse=" + "), "- 1"))
  fit <- lm(frml, data = dataNoFe)
  ssResidual <- sum(fit$residuals^2)
  ssTotal <- sum((dataLagged[[model$target]] - mean(dataLagged[[model$target]]))^2)
  1 - ssResidual/ssTotal
}

rSquaredData <- 
  models |> 
  rowwise() |> 
  mutate(rSquared = rSquared(target, preds, dataNoFe, dataLagged)) |> 
  select(kind, termName, lag, rSquared)
write_csv(rSquaredData, "results/rSquared.csv")

