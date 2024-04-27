source("common.R")

coeffs <- read_csv("results/coefficients_FitFull.csv")
coef1 <- coeffs$coefficient
names(coef1) <- coeffs$variable

coefFile <- "datacode/reg_results/lagdiff_lintren_fix_lagN10_coef.csv"
coef2tbl <- read_csv(coefFile)
coef2 <- coef2tbl[[2]]
names(coef2) <- convertReferenceNames(coef2tbl[[1]])

cat("Checking coefficient values:\n")
compareNumeric(coef2, coef1[names(coef2)])
