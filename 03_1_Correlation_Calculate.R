source("common.R")


data <- loadRegressionData("NoFe_FitFull")


dataResidualYearWide <- 
  data |> 
  select(GID_1, year, residual) |> 
  arrange(year) |> 
  pivot_wider(names_from=year, values_from=residual) |> 
  arrange(GID_1)

corrMatYear <- cor(
  dataResidualYearWide |> select(-GID_1), 
  use = "pairwise.complete.obs")

corrMatYear |> 
  as_tibble(rownames = "year") |> 
  write_csv("results/correlationMatrix_FitFull_Year.csv")


dataResidualGid1Wide <- 
  data |> 
  select(GID_1, year, residual) |> 
  pivot_wider(names_from=GID_1, values_from=residual) |> 
  arrange(year)

corrMatGid1 <- cor(
  dataResidualGid1Wide |> select(-year), 
  use = "pairwise.complete.obs")

corrMatGid1 |> 
  as_tibble(rownames = "GID_1") |> 
  write_csv("results/correlationMatrix_FitFull_Gid1.csv")



shapesAll <- sf::read_sf("./datacode/masks/DOSE_shapes.gpkg")
dataNoFe <- loadRegressionData("NoFe")
shapes <- shapesAll |> semi_join(dataNoFe, join_by(GID_0, GID_1))
shapeRegions <- shapes |> as_tibble() |> select(GID_0, GID_1)
dataRegions <- dataNoFe |> select(GID_0, GID_1) |> distinct()
missingRegions <- anti_join(dataRegions, shapeRegions) 
cat("There are", nrow(missingRegions), "regions without shape information.\n")

cat("Calculating centers of", nrow(shapes), "regions ... ")
st <- Sys.time()
centers <- sf::st_centroid(shapes)
cat("Done after", format(Sys.time()-st), "\n")

cat("Calculating distance matrix of", nrow(centers), "centers ... ")
st <- Sys.time()
distanceMatrix <- s2::s2_distance_matrix(centers, centers)
dimnames(distanceMatrix) <- list(centers$GID_1, centers$GID_1)
cat("Done after", format(Sys.time()-st), "\n")

distanceMatrix |> 
  as_tibble(rownames = "GID_1") |> 
  write_csv("results/distanceMatrix_Gid1.csv")


