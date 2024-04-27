source("common.R")



# Define Functions --------------------------------------------------------


fitAndPredictLm <- function(xTrain, yTrain, xValidation) {
  fit <- .lm.fit(xTrain, yTrain)
  as.vector(xValidation %*% fit$coefficients)
}

kFoldCvOne <- function(x, y, cluster, nFolds, seed) {
  set.seed(seed)
  allClusters <- unique(cluster)
  clustersFoldId <- sample(rep(1:nFolds, length.out = length(allClusters)))
  cvResult <- sapply(seq_len(nFolds), \(i) {
    validationClusters <- allClusters[clustersFoldId == i]
    isValidation <- cluster %in% validationClusters
    yValidation <- y[isValidation]
    yTrain <- y[!isValidation]
    xValidation <- x[isValidation, , drop=FALSE]
    xTrain <- x[!isValidation, , drop=FALSE]
    esti <- fitAndPredictLm(xTrain, yTrain, xValidation)
    loss <- (yValidation - esti)^2
    referenceLoss <- yValidation^2
    mean(loss - referenceLoss)
  })
  return(cvResult)
}

kFoldCv <- function(
    data,
    predictors,
    target,
    nReps,
    cluster,
    nFolds,
    nCores = NULL
) {
  cat(nFolds, "-fold CV with ", nReps, " reps for model with ", length(predictors), " predictors... ", sep="")
  st <- Sys.time()
  x <- data |> select(all_of(predictors)) |> as.matrix()
  y <- data |> pull(target)
  seeds <- sample.int(.Machine$integer.max, nReps)
  if (length(nCores) == 1 && nCores > 1) {
    registerDoParallel(cores = nCores)
    cvResultList <- foreach(seed = seeds, .export = c("kFoldCvOne", "fitAndPredictLm")) %dopar% {
      kFoldCvOne(x, y, cluster, nFolds, seed)
    }
    stopImplicitCluster()
    cvResult <- simplify2array(cvResultList)
  } else {
    cvResult <- sapply(seeds, \(seed) kFoldCvOne(x, y, cluster, nFolds, seed))
  }
  cat("Done after", format(Sys.time()-st, digits=2), "\n")
  tibble(mean = mean(cvResult), sd = sd(cvResult), n = length(cvResult))
}


runCrossValidation <- function(data, models, clusterName, nReps, nFolds, level, nCores = NULL) {
  
  q <- qnorm(1 - level/2)
  
  models |> 
    rowwise() |> 
    mutate(cv = kFoldCv(
      data = data,
      predictors = preds,
      target = target,
      nReps = nReps,
      cluster = getClusters(data, clusterName),
      nFolds = nFolds,
      nCores = nCores)
    ) |> 
    unnest(cv, names_sep = "_") |> 
    mutate(
      cv_lower = cv_mean - q * cv_sd / sqrt(cv_n),
      cv_upper = cv_mean + q * cv_sd / sqrt(cv_n)
    ) |>
    mutate(clusterName = clusterName, .before = 1)
}



# Main --------------------------------------------------------------------


dataNoFe <- loadRegressionData("NoFe")

nCores <- detectCores()

cvData <- 
  lapply(clusterNames, \(clusterName) {
    runCrossValidation(
      data = dataNoFe,
      models = models, 
      clusterName = clusterName,
      nReps = 100,
      nFolds = 10,
      level = 0.05,
      nCores = nCores)
  }) |> 
  bind_rows()
  
write_csv(cvData, "results/crossValidation.csv")
