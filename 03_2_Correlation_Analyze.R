source("common.R")



# Define Functions --------------------------------------------------------


analyzeCorr <- function(kind, corrVec, selection, name) {
  selection[is.na(selection)] <- FALSE 
  x <- corrVec[selection & !is.na(corrVec)]
  probs <- seq(0, 1, 0.05)
  bind_cols(
    tibble(
      kind = kind,
      name = name,
      mean = mean(x),
      median = median(x),
      sd = sd(x)),
    setNames(
      as.list(quantile(x, probs)), 
      sprintf("q%02d", as.integer(probs * 100))
    ) |> 
      as_tibble()
  )
}



# Main --------------------------------------------------------------------


corrMatYear <- 
  read_csv("results/correlationMatrix_FitFull_Year.csv") |> 
  column_to_rownames("year") |> 
  as.matrix()

corrMatGid1 <- 
  read_csv("results/correlationMatrix_FitFull_Gid1.csv") |> 
  column_to_rownames("GID_1") |> 
  as.matrix()

distMatGid1Raw <- 
  read_csv("results/distanceMatrix_Gid1.csv") |> 
  column_to_rownames("GID_1") |> 
  as.matrix()
distMatGid1 <- corrMatGid1
distMatGid1[] <- NA_real_
distMatGid1[rownames(distMatGid1Raw), colnames(distMatGid1Raw)] <- distMatGid1Raw

sel <- upper.tri(corrMatYear, diag=FALSE)
corrVecYear <- corrMatYear[sel]
timeDiff <- (col(corrMatYear)-row(corrMatYear))[sel]

sel <- upper.tri(corrMatGid1, diag=FALSE)
corrVecGid1 <- corrMatGid1[sel]
distVecGid1 <- distMatGid1[sel]
colGid1 <- colnames(corrMatGid1)[col(corrMatGid1)[sel]]
rowGid1 <- rownames(corrMatGid1)[row(corrMatGid1)[sel]]
colGid0 <- str_sub(colGid1, end=3)
rowGid0 <- str_sub(rowGid1, end=3)
colEu28 <- colGid0 %in% gid0Eu28
rowEu28 <- rowGid0 %in% gid0Eu28
colEuCore <- colGid0 %in% gid0EuCore
rowEuCore <- rowGid0 %in% gid0EuCore

corrData <- bind_rows(
  analyzeCorr("temporal", corrVecYear, TRUE, "all"),
  analyzeCorr("temporal", corrVecYear, timeDiff == 1, "consecutive"),
  analyzeCorr("spatial", corrVecGid1, TRUE, "all"),
  analyzeCorr("spatial", corrVecGid1, colGid0 == rowGid0, "same country"),
  analyzeCorr("spatial", corrVecGid1, colGid0 != rowGid0, "different country"),
  analyzeCorr("spatial", corrVecGid1, colGid0 != rowGid0 & colEu28 & rowEu28, "different EU28 countries"),
  analyzeCorr("spatial", corrVecGid1, colGid0 != rowGid0 & colEuCore & rowEuCore, "different EU 1995 countries"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e5, "distance < 100km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e5, "distance < 200km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 5e5, "distance < 500km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e6, "distance < 1000km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e6, "distance < 2000km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e5 & colGid0 != rowGid0, "distance < 100km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e5 & colGid0 != rowGid0, "distance < 200km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 5e5 & colGid0 != rowGid0, "distance < 500km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e6 & colGid0 != rowGid0, "distance < 1000km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e6 & colGid0 != rowGid0, "distance < 2000km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e5 & colGid0 == rowGid0, "distance < 100km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e5 & colGid0 == rowGid0, "distance < 200km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 5e5 & colGid0 == rowGid0, "distance < 500km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e6 & colGid0 == rowGid0, "distance < 1000km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e6 & colGid0 == rowGid0, "distance < 2000km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 1e5, "distance > 100km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 2e5, "distance > 200km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 5e5, "distance > 500km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 1e6, "distance > 1000km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 2e6, "distance > 2000km"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 1e5 & colGid0 != rowGid0, "distance > 100km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 2e5 & colGid0 != rowGid0, "distance > 200km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 5e5 & colGid0 != rowGid0, "distance > 500km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 1e6 & colGid0 != rowGid0, "distance > 1000km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 2e6 & colGid0 != rowGid0, "distance > 2000km and different country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 1e5 & colGid0 == rowGid0, "distance > 100km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 2e5 & colGid0 == rowGid0, "distance > 200km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 5e5 & colGid0 == rowGid0, "distance > 500km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 1e6 & colGid0 == rowGid0, "distance > 1000km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 > 2e6 & colGid0 == rowGid0, "distance > 2000km and same country"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e5 & colGid0 != rowGid0 & !colEu28 & !rowEu28, "distance < 100km and different country not EU"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e5 & colGid0 != rowGid0 & !colEu28 & !rowEu28, "distance < 200km and different country not EU"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 5e5 & colGid0 != rowGid0 & !colEu28 & !rowEu28, "distance < 500km and different country not EU"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 1e6 & colGid0 != rowGid0 & !colEu28 & !rowEu28, "distance < 1000km and different country not EU"),
  analyzeCorr("spatial", corrVecGid1, distVecGid1 < 2e6 & colGid0 != rowGid0 & !colEu28 & !rowEu28, "distance < 2000km and different country not EU"),
  lapply(
    gid0Large, 
    \(gid0) analyzeCorr("spatial", corrVecGid1, colGid0 == gid0 & rowGid0 == gid0, gid0)) |> bind_rows()
)

write_csv(corrData, "results/correlation_FitFull.csv")
