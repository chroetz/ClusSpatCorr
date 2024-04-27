source("common.R")


dataNoFe <- loadRegressionData("NoFe")

model <- models |> filter(kind == "full")
frml <- formula(paste(model$target, "~", paste(model$preds[[1]], collapse=" + "), "- 1"))
fit <- lm(frml, data = dataNoFe)

n <- nrow(dataNoFe)
p1 <- length(model$preds[[1]])
nFe <- length(unique(dataNoFe$GID_1)) + length(unique(dataNoFe$year))
p2 <- nFe + length(model$preds[[1]])
correctionFactor <- (n-p1)/(n-p2)

covFile <- "datacode/reg_results/lagdiff_lintren_fix_lagN10_cov.csv"
covRefRaw <- read_csv(covFile)
covRef <- covRefRaw[-1] |> as.matrix()
rownames(covRef) <- convertReferenceNames(covRefRaw[[1]])
colnames(covRef) <- convertReferenceNames(colnames(covRef))
covThis <- correctionFactor*sandwich::vcovCL(fit, cluster = getClusters(dataNoFe, "Gid1"))
nms <- colnames(covThis)
cat("Checking covariance values:\n")
compareNumeric(covThis[nms, nms], covRef[nms, nms])
