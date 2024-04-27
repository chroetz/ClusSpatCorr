source("common.R")



# Define Functions --------------------------------------------------------


bootOne <- function(xCluster, yCluster, seed) {
  stopifnot(length(xCluster) == length(yCluster))
  set.seed(seed)
  bootClusters <- sample(seq_along(xCluster), replace = TRUE)
  x <- do.call(rbind, xCluster[bootClusters])
  y <- unlist(yCluster[bootClusters])
  fit <- .lm.fit(x, y)
  c(seed, fit$coefficients)
}

bootstrap <- function(data, nReps, target, preds, cluster, nCores = 1) {
  x <- data |> select(all_of(preds)) |> as.matrix()
  y <- data |> pull(target)
  allClusters <- unique(cluster)
  xCluster <- lapply(allClusters, \(clus) x[cluster == clus, ])
  yCluster <- lapply(allClusters, \(clus) y[cluster == clus])
  seeds <- sample.int(.Machine$integer.max, nReps)
  if (length(nCores) == 1 && nCores > 1) {
    registerDoParallel(cores = nCores)
    bootResultList <- foreach(seed = seeds, .export = c("bootOne")) %dopar% {
      bootOne(xCluster, yCluster, seed)
    }
    stopImplicitCluster()
    bootResult <- simplify2array(bootResultList)
  } else {
    bootResult <- sapply(seeds, \(seed) bootOne(xCluster, yCluster, seed))
  }
  bootResult <- t(bootResult)
  colnames(bootResult) <- c("seed", preds)
  as_tibble(bootResult)
}

runBootstrap <- function(data, models, nReps, clusterName, nCores = 1) {
  models |> 
    rowwise() |> 
    mutate(bootstrap = list(bootstrap(
      data, nReps, target, preds, getClusters(data, clusterName), nCores
    ))) |> 
    select(lag, bootstrap) |> 
    unnest(bootstrap) |> 
    mutate(clusterName = clusterName, .before = 1)
}


writeBootstrapDataFiles <- function(bootData, clusterName, nVirtualSeeds) {
  
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


nVirtualSeeds <- 20
nReps <- 50
nCores <- detectCores()

dataNoFe <- loadRegressionData("NoFe")

bootData <- 
  lapply(clusterNames, \(clusterName) {
    runBootstrap(
      data = dataNoFe,
      models = models |> filter(kind == "klw24"),
      nReps = nVirtualSeeds * nReps,
      clusterName = clusterName,
      nCores = nCores)
  }) |> 
  bind_rows()

write_csv(bootData, "results/bootstrap.csv")
