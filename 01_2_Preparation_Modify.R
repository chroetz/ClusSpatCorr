source("common.R")


# feols_main_regressions.R
linesIn <- readLines("datacode/feols_main_regressions.R")
linesOut <- 
  linesIn |> 
  str_remove_all(",'Wn_d'") |> 
  str_remove_all(",'Wn_m'")
writeLines(linesOut, "datacode/feols_main_regressions_MOD.R")


# feols_main_regressions.R
linesIn <- readLines("datacode/feols_bootstrap_regressions.R")
linesOut <- 
  linesIn[-1] |> 
  str_remove_all(",'Wn_d'") |> 
  str_remove_all(",'Wn_m'")
writeLines(linesOut, "datacode/feols_bootstrap_regressions_MOD.R")


# project_damages.py
linesIn <- readLines("datacode/project_damages.py")
newArgLine <- "suffix=sys.argv[8]"
linesOut <- 
  c(linesIn[1:97], newArgLine, linesIn[98:length(linesIn)]) |> 
  str_replace_all(fixed("str(seed) + '.npy'"), "str(seed) + '_' + str(suffix) + '.npy'") |> 
  str_replace_all(fixed("str(sd) + '_coefs.csv'"), "str(sd) + '_coefs_' + str(suffix) + '.csv'") |> 
  str_replace_all(fixed("coefs=coefs.append(cfs)"), "coefs=coefs._append(cfs)")
writeLines(linesOut, "datacode/project_damages_MOD.py")


# plot_Fig1.py
linesIn <- readLines("datacode/plot_Fig1.py")
linesOut <- linesIn
newArgLine <- "suffix=sys.argv[5]"
printLines <- r"(
if not np.isnan(sig_years[0,0]):
  print("Distinction at levels " + str(cutoffs) + ": " + str(int(sig_years[0,0])) + ", "+ str(int(sig_years[1,0])))
  x = dam_curves[:,:,0,v,int(sig_years[0,0])]
  print("Median loss: " + str(np.median(x)))
  print("Likely range loss: " + str(np.percentile(x,[18,83])))
else:
  print("No level 5% difference.")
)"
linesOut <- 
  c(linesIn[1:15], newArgLine, linesIn[15:49], printLines, linesIn[52:length(linesIn)]) |> 
  str_replace_all(fixed("str(seed) + '.npy'"), "str(seed) + '_' + str(suffix) + '.npy'") |> 
  str_replace_all(fixed("str(Nboot*len(seeds)) + '.npy'"), "str(Nboot*len(seeds)) + '_' + str(suffix) + '.npy'") |> 
  str_replace_all(fixed("'Fig1.pdf'"), "'Fig1_' + str(suffix) + '.pdf'") |>
  str_replace(fixed("],90,axis=0"), "],95,axis=0") |> 
  str_replace(fixed("],10,axis=0"), "],5,axis=0")
writeLines(linesOut, "datacode/plot_Fig1_MOD.py")
