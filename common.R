library(tidyverse)
library(foreach)
library(doParallel)



set.seed(0)

dirNames <- c("results", "regressionData", "plots", "latex")
for (dirName in dirNames) if (!dir.exists(dirName)) dir.create(dirName)



convertReferenceNames <- function(nms) {
  nms |> 
    str_replace("l\\((.*), (\\d+)\\)", "\\1_lag\\2") |> 
    str_replace("_lag(\\d)$", "_lag0\\1")
}


compareNumeric <- function(
    x, y, 
    absTol = .Machine$double.eps^(1/2), 
    relTol = .Machine$double.eps^(1/4)
  ) {
  
  absDiff <- abs(x - y)
  relDiff <- absDiff/((abs(x) + abs(y))/2)
  mad <- max(absDiff)
  mrd <- max(relDiff)
  
  cat("maximum absolute difference:", mad)
  if (mad <= absTol) cat(" -> ok\n") else stop("Values are too different (absolute)!")
  cat("maximum relative difference:", mrd)
  if (mrd <= relTol) cat(" -> ok\n") else stop("Values are too different (relative)!")
  
  return(invisible())
}


varns <- c(
  "Tmean_d",
  "Tstd_d",
  "Pt_d",
  "wet_days_1_d",
  "vwet_days_am_99p9_d")
modns <- c(
  "Tmean_m",
  "Tseasdiff_m",
  "Pt_m",
  "wet_days_1_m",
  "Tmean_m")
interactions <- paste0(modns, "_i_", varns)
target <- "dlgrp_pc_usd_2015"
fixedEffects <- c("GID_1", "theYear", "GID_1:year")
getNumFixedEffects <- function(data) length(unique(data$theYear)) + 2*(length(unique(data$GID_1))-1)
predictors <- outer(
  c(varns, interactions),
  c("", sprintf("_lag%02d", 1:10)),
  paste0) |> as.vector()

termLabels <- c(
  Tmean_d = 'paste("Mean temp.: ", Delta*bar(T))',
  Pt_d = 'paste("Total precip.: ", Delta*P)',
  Tstd_d = 'paste("Temp. var.: ", Delta*tilde(T))',
  vwet_days_am_99p9_d = 'paste("Extreme precip.: ", Delta*P[ext])',
  wet_days_1_d = 'paste("Wet days: ", Delta*P[wd])')


lags <- 0:10
backwardModels <- lapply(1:5, \(vn) {
  tibble(
    termName = varns[vn],
    lag = lags,
    preds = lapply(rev(lags), \(i) {
      setdiff(predictors, outer(
        c(varns[vn], interactions[vn]),
        sprintf("_lag%02d", (10:0)[seq_len(i)]),
        paste0) |> as.vector())
    }))
}) |> 
  bind_rows()
forwardModels <- lapply(1:5, \(vn) {
  tibble(
    termName = varns[vn],
    lag = lags,
    preds = lapply(lags, \(i) {
      outer(
        c(varns[vn], interactions[vn]),
        c("", sprintf("_lag%02d", seq_len(i))),
        paste0) |> as.vector()
    }))
}) |> 
  bind_rows()

refModel <- \(k) c(
  outer(
    c(varns[1:2], interactions[1:2]),
    c("", sprintf("_lag%02d", 1:k)),
    paste0
  ) |> 
    as.vector(),
  outer(
    c(varns[3:5], interactions[3:5]),
    c("", sprintf("_lag%02d", 1:k)),
    paste0
  ) |> 
    as.vector())
models <- 
  bind_rows(
    tibble(kind = "trivial", termName = "None", lag = 0, preds = list(character(0))),
    tibble(kind = "full", termName = "None", lag = 10, preds = list(predictors)),
    tibble(kind = "klw24", termName = "None", lag = 8, preds = list(refModel(8))),
    tibble(kind = "klw24", termName = "None", lag = 9, preds = list(refModel(9))),
    tibble(kind = "klw24", termName = "None", lag = 10, preds = list(refModel(10))),
    forwardModels |> mutate(kind = "forward"),
    backwardModels |> mutate(kind = "backward")
  ) |> 
  mutate(target = .env$target)



gid0Eu28 <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR")
gid0EuCore <- c("AUT","BEL","DNK","FIN","FRA","DEU","GRC","IRL","ITA","LUX","NLD","PRT","ESP","SWE","GBR") # before 2004, but after 1995
gid0Large <- c("RUS", "CAN", "CHN", "USA", "BRA")


clusterNames <- c("Gid1", "Gid0", "Year", "Gid0Year", "Gid1Year")

getClusters <- function(data, clusterName) {
  switch(
    clusterName,
    Gid0 = data$GID_0,
    Gid1 = data$GID_1,
    Year = data$theYear,
    Gid0Year = paste(data$GID_0, data$theYear),
    Gid1Year = paste(data$GID_1, data$theYear))
}

clusterLabels <- c(
  Gid0 = "Country",
  Gid1 = "Region",
  Year = "Year",
  Gid0Year = "CountryYear",
  Gid1Year = "RegionYear")



loadRegressionData <- function(dataName) {
  path <- sprintf("regressionData/regressionData_%s.csv", dataName)
  if (!file.exists(path)) stop("Cannot find ", path, ". Run regression script and set correct working directory!")
  read_csv(path, col_types=cols()) |> 
    mutate(theYear = as.character(year))
}



# from 
# https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
shift_legend <- function(p) {

  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }

  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }

  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")

  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable::gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")

  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- cowplot::gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- cowplot::gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- cowplot::gtable_remove_grobs(gp, "guide-box")

  return(gp)
}





# Clean environment and make objects available for other scripts ---------------

if (search()[2] == "common") detach("common")

attach(
  lst(
    varns,
    modns,
    interactions, 
    target, 
    fixedEffects,
    getNumFixedEffects,
    predictors,
    termLabels,
    lags,
    models,
    gid0Eu28,
    gid0EuCore,
    gid0Large,
    clusterNames,
    clusterLabels,
    convertReferenceNames,
    compareNumeric,
    getClusters,
    loadRegressionData,
    shift_legend),
  name = "common")

remove(list = ls())
