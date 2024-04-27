source("common.R")



# Define Functions --------------------------------------------------------


getStdErrTerm <- function(cov, baseName, interactionName, moderatorValue) {
  err <- sqrt(
    c(diag(cov)[baseName],
      diag(cov)[interactionName],
      diag(cov)[baseName] +
        2*cov[cbind(baseName, interactionName)]*moderatorValue + 
        moderatorValue^2 * diag(cov)[interactionName]))
  seData <- tibble(
    name = c(baseName, interactionName, paste0("TERM_", baseName)),
    standardError = err)
  return(seData)
}


calcPlotEffects <- function(dataRaw, fit, covList, variableName, moderatorName, sigLevel, scale, lags=0:10) {
  
  coefs <- coef(fit)
  nms <- names(coefs)
  variableNameAllLags <- str_subset(nms, paste0("\\b", variableName))
  interactionNameAllLags <- str_subset(nms, paste0(moderatorName, "_i_", variableName))
  moderatorValue <- dataRaw[[moderatorName]] |> median(na.rm=TRUE)
  seData <- 
    lapply(
      names(covList), 
      \(nm) {
        getStdErrTerm(covList[[nm]], variableNameAllLags, interactionNameAllLags, moderatorValue) |> 
        mutate(method = nm)
      }
    ) |> 
    bind_rows()
  
  effectData <- bind_rows(
    tibble(
      name = variableNameAllLags,
      role = "base",
      lag = lags,
      termName = variableName[1],
      effect = scale * coefs[variableNameAllLags]),
    tibble(
      name = interactionNameAllLags,
      role = "interaction",
      lag = lags,
      termName = variableName[1],
      effect = scale * coefs[interactionNameAllLags]),
    tibble(
      name = paste0("TERM_", variableNameAllLags),
      role = "term",
      lag = lags,
      termName = variableName[1],
      effect = scale * (coefs[variableNameAllLags] + coefs[interactionNameAllLags]*moderatorValue)))
  
  q <- qnorm(1 - sigLevel/2)
  
  confidenceData <- bind_rows(
    seData |> 
      left_join(effectData, join_by(name)) |> 
      mutate(
        standardError = scale * standardError,
        confiRadius = q * standardError,
        lower = effect - confiRadius,
        upper = effect + confiRadius,
        pValue = pnorm(effect, sd = standardError)
      ) |> 
      select(-effect, -confiRadius) |> 
      pivot_longer(c(lower, upper, pValue, standardError), names_to="kind", values_to="value"),
    effectData |> 
      rename(value = effect) |>
      mutate(method = "None", kind = "effect")
  )
  
  return(confidenceData)
}



# Main --------------------------------------------------------------------


dataRaw <- loadRegressionData("Raw")
dataNoFe <- loadRegressionData("NoFe")

model <- models |> filter(kind == "full")
frml <- formula(paste(model$target, "~", paste(model$preds[[1]], collapse=" + "), "- 1"))
fit <- lm(frml, data = dataNoFe)

n <- nrow(dataNoFe)
p1 <- length(model$preds[[1]])
nFe <- length(unique(dataNoFe$GID_1)) + length(unique(dataNoFe$year))
p2 <- nFe + length(model$preds[[1]])
correctionFactor <- (n-p1)/(n-p2)

clusterCountryEu28 <- dataNoFe$GID_0
clusterCountryEu28[clusterCountryEu28 %in% gid0Eu28] <- "EU"
clusterCountryEuCore <- dataNoFe$GID_0
clusterCountryEuCore[clusterCountryEuCore %in% gid0EuCore] <- "EU"

clusterList <- list(
  clusterRegion = dataNoFe$GID_1,
  clusterRegionYear = paste(dataNoFe$GID_1, dataNoFe$theYear),
  clusterCountry = dataNoFe$GID_0,
  clusterCountryEu28 = clusterCountryEu28,
  clusterCountryEuCore = clusterCountryEuCore,
  clusterYear = dataNoFe$theYear,
  clusterCountryYear = paste(dataNoFe$GID_0, dataNoFe$theYear),
  clusterCountryEu28Year = paste(clusterCountryEu28, dataNoFe$theYear),
  clusterCountryEuCoreYear = paste(clusterCountryEuCore, dataNoFe$theYear))
vcovRawList <- c(
  list(
    homoscedastic = sandwich::vcovHC(fit, type = "const"),
    heteroscedastic = sandwich::vcovHC(fit, type = "HC1"), 
    DriscollKraay0 = sandwich::vcovPL(fit, order.by = dataNoFe$theYear, lag = 0, aggregate = TRUE),
    DriscollKraay1 = sandwich::vcovPL(fit, order.by = dataNoFe$theYear, lag = 1, aggregate = TRUE),
    DriscollKraay2 = sandwich::vcovPL(fit, order.by = dataNoFe$theYear, lag = 2, aggregate = TRUE)),
  lapply(clusterList, \(cl) sandwich::vcovCL(fit, cluster = cl)))
vcovList <- lapply(vcovRawList, \(x) x*correctionFactor)

stds <- 
  dataRaw |> 
  summarise(across(
    c(Tmean_d,Tstd_d,Pt_d,wet_days_1_d,vwet_days_am_99p9_d), 
    \(x) sd(x, na.rm=TRUE)))
scales <- c(
  Tmean_d = 100*1,
  Tstd_d = 100*1,
  Pt_d = 100*stds$Pt_d,
  wet_days_1_d = 100*stds$wet_days_1_d,
  vwet_days_am_99p9_d = 100*stds$vwet_days_am_99p9_d)

confiData <- lapply(
  seq_along(varns), 
  \(i) calcPlotEffects(dataRaw, fit, vcovList, varns[i], modns[i], 0.05, scales[varns[i]])
  ) |> 
  bind_rows()

write_csv(confiData, "results/confidence_FitFull.csv")
