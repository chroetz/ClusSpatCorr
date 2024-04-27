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

