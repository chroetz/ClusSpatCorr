wd <- getwd()
setwd("./datacode")

cat("Running main regression script of Kotz et al. 2024 ...\n")
system("Rscript feols_main_regressions_MOD.R")
cat("Done.\n")

cat("Running bootstrap script of Kotz et al. 2024  for seed 1...\n")
system("Rscript feols_bootstrap_regressions_MOD.R 1 50 lagdiff_lintren_fix")
cat("Done.\n")

setwd(wd)
