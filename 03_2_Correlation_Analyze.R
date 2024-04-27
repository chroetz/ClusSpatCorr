source("common.R")



# Define Functions --------------------------------------------------------


analyzeCorr <- function(kind, corrVec, selection, name) {
  xYes <- corrVec[selection]
  xNo <- corrVec[!selection]
  tibble(
    kind = kind,
    name = name,
    meanYes = mean(xYes, na.rm=TRUE),
    medianYes = median(xYes, na.rm=TRUE),
    sdYes = sd(xYes, na.rm=TRUE),
    meanNo = mean(xNo, na.rm=TRUE),
    medianNo = median(xNo, na.rm=TRUE),
    sdNo = sd(xNo, na.rm=TRUE))
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

corrVecYear <- corrMatYear[upper.tri(corrMatYear, diag=FALSE)]
timeDiff <- (col(corrMatYear)-row(corrMatYear))[upper.tri(corrMatYear, diag=FALSE)]

corrVecGid1 <- corrMatGid1[upper.tri(corrMatGid1, diag=FALSE)]
colGid1 <- colnames(corrMatGid1)[col(corrMatGid1)[upper.tri(corrMatGid1, diag=FALSE)]]
rowGid1 <- rownames(corrMatGid1)[row(corrMatGid1)[upper.tri(corrMatGid1, diag=FALSE)]]
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
  analyzeCorr("spatial", corrVecGid1, colGid0 != rowGid0 & colEu28 & rowEu28, "different EU28 countries"),
  analyzeCorr("spatial", corrVecGid1, colGid0 != rowGid0 & colEuCore & rowEuCore, "different EU 1995 countries"),
  lapply(
    gid0Large, 
    \(gid0) analyzeCorr("spatial", corrVecGid1, colGid0 == gid0 & rowGid0 == gid0, gid0)) |> bind_rows()
)

write_csv(corrData, "results/correlation_FitFull.csv")