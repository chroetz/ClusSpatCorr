source("common.R")



# Define Functions --------------------------------------------------------


runProjectDamages <- function(virtualSeed, climSsp, suffix) {
  cat("virtualSeed:", virtualSeed, "\n")
  pt <- proc.time()
  system(sprintf("python project_damages_MOD.py ssp2 %s lagdiff_lintren_fix_spec 8_9_10 50 %d 30 %s", climSsp, virtualSeed, suffix))
  cat("Duration:", (proc.time()-pt)[3], "s\n")
}



# Main --------------------------------------------------------------------


nCores <- detectCores()
virtualSeeds <- 1:20
climSsps <- c("ssp126", "ssp585")
suffixes <- paste0("cluster", clusterNames)

wd <- getwd()
setwd("datacode")

registerDoParallel(cores = nCores)

foreach(suffix = suffixes) %:%
  foreach(virtualSeed = virtualSeeds) %:%
    foreach(climSsp = climSsps) %dopar% {
      runProjectDamages(virtualSeed, climSsp, suffix)
    }

stopImplicitCluster()

setwd(wd)
