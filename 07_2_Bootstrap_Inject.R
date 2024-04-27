source("common.R")



# Define Functions --------------------------------------------------------


writeBootstrapDataFiles <- function(bootData, clusterName, nVirtualSeeds) {
  
  bootData <- bootData |> filter(clusterName == .env$clusterName)
    
  lags <- bootData$lag |> unique()
  
  bootData <- 
    bootData |> 
    mutate(virtualSeed = rep(1:nVirtualSeeds, each=n()/nVirtualSeeds), .by = lag, .before=1) |> 
    rename_with(\(nm) str_replace(nm, "^(.*)_lag0?(\\d+)$", "l(\\1, \\2)"), matches("_lag\\d{2}$"))
  refData <- 
    read_csv(
      "datacode/reg_results/lagdiff_lintren_fix_spec_NL_10_bootN_50_seed_1_coefs.csv",
      name_repair="minimal",
      col_types=cols()
    )
  refColNames <- names(refData)
  
  for (l in lags) for (vseed in 1:nVirtualSeeds) {
    bd <- bootData |> filter(virtualSeed == vseed, lag == l) 
    bd <- bd |> select(all_of(setdiff(refColNames, ""))) |> select(where(\(x) !all(is.na(x))))
    bd <- bind_cols(refData[, 1], bd, .name_repair="minimal")
    write_csv(bd, sprintf("datacode/reg_results/lagdiff_lintren_fix_spec_NL_%d_bootN_50_seed_%d_coefs_cluster%s.csv", l, vseed, clusterName))
  }
}
 


# Main --------------------------------------------------------------------


bootData <- read_csv("results/bootstrap.csv")

nVirtualSeeds <- 20
nReps <- 50

for (clusterName in clusterNames) { 
  cat(clusterName, "... ")
  writeBootstrapDataFiles(bootData, clusterName, nVirtualSeeds)
  cat("Done.\n")
}
