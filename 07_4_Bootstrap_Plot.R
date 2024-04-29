source("common.R")



# Define Functions --------------------------------------------------------


runPlotFig1 <- function(clusterName) {
  suffix <- paste0("cluster", clusterName)
  out <- system(
    sprintf("python plot_Fig1_MOD.py ssp2 lagdiff_lintren_fix_spec 8_9_10 30 %s", suffix),
    intern = TRUE)
  patternSigYear <- "^Distinction at levels \\[0.05, 0.01\\]: (\\d*), (\\d*)$"
  sigYears <- 
    out |> 
    str_subset(patternSigYear) |> 
    str_extract(patternSigYear, group=1:2) |>
    as.numeric()
  patternMedian <- "^Median loss: ([+-]?\\d*\\.\\d*)$"
  committedLossMedian <- 
    out |> 
    str_subset(patternMedian) |> 
    str_extract(patternMedian, group=1) |>
    as.numeric()
  patternRange <- "^Likely range loss: \\[([+-]?\\d*\\.\\d*) ([+-]?\\d*\\.\\d*)\\]"
  committedLossRange <- 
    out |> 
    str_subset(patternRange) |> 
    str_extract(patternRange, group=1:2) |>
    as.numeric()
  file.copy(sprintf("Fig1_%s.pdf", suffix), "../plots/", overwrite=TRUE)
  return(tibble(
    clusterName = clusterName,
    median = committedLossMedian[1], 
    lower = committedLossRange[1],
    upper = committedLossRange[2],
    sigYear01 = sigYears[2],
    sigYear05 = sigYears[1]
  ))
}
 


# Main --------------------------------------------------------------------


wd <- getwd()
setwd("datacode")

committedLossData <- lapply(clusterNames, runPlotFig1) |> bind_rows()

setwd(wd)

write_csv(committedLossData, "results/committedLoss.csv")
