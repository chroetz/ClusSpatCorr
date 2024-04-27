if (!dir.exists("datacode")) {
  stop("Cannot find data from Kotz et al. 2024. Run download script or set correct working directory!")
}

source("common.R")



# Load and Prepare the Data -----------------------------------------------


dataRaw <- 
  read_csv("datacode/DOSE_climate_data/DOSEV2_W5E5D_full.csv") |>
  select(
    GID_0,
    GID_1,
    year,
    all_of(target),
    all_of(varns),
    all_of(modns)
  ) |>
  mutate(theYear = as.character(year))

for (i in 1:length(interactions)) {
  dataRaw[interactions[i]] <- dataRaw[modns[i]] * dataRaw[varns[i]]
}

write_csv(dataRaw, "regressionData/regressionData_Raw.csv")

addLagged <- function(x, y, lag, variable, regionName, timeName) {
  out <-
    x |>
    mutate(laggedTime = !!sym(timeName) - lag) |>
    left_join(
      y |>
        select(!!sym(regionName), !!sym(timeName), all_of(variable)) |>
        rename_with(\(x) paste0(x, sprintf("_lag%02d", lag)), all_of(variable)),
      join_by(!!sym(regionName), laggedTime == !!sym(timeName))) |>
    select(-laggedTime)
  return(out)
}

dataLagged <- dataRaw
lagsNew <- setdiff(lags, 0)
for (variable in c(varns, interactions)) {
  for (lag in lagsNew) {
    dataLagged <- addLagged(dataLagged, dataRaw, lag, variable, "GID_1", "year")
  }
}
data <- 
  dataLagged |> 
  select(GID_0, GID_1, theYear, year, all_of(target), matches(paste(c(varns, modns, interactions), collapse="|"))) |> 
  drop_na() |> 
  arrange(GID_0, GID_1, year)

write_csv(data, "regressionData/regressionData_Lagged.csv")



# Remove Fixed Effects --------------------------------------------------------

matrixOfVariables <- 
  data |> 
  select(all_of(c(target, predictors))) |>
  as.matrix()

# The following will take something like 3-10min.
pt <- proc.time()
frmlFe <- formula(paste("matrixOfVariables ~ ", paste(fixedEffects, collapse = " + "), "- 1"))
feFit <- lm(frmlFe, data = data)
cat("Remove Fixed Effects duration:", (proc.time()-pt)[3], "s\n")

dataNoFe <- bind_cols(
  data |> select(GID_0, GID_1, year),
  as_tibble(feFit$residuals))

write_csv(dataNoFe, "regressionData/regressionData_NoFE.csv")
