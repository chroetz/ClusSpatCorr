source("common.R")
library(gt)



confiData <- read_csv("results/confidence_FitFull.csv")

roles <- confiData$role |> unique()
termNames <- confiData$termName |> unique()

for (role in roles) for (termName in termNames) {
  
  data <- 
    confiData |> 
    filter(kind == "pValue") |> 
    filter(role == .env$role, termName == .env$termName) |> 
    mutate(name = sprintf("%d", lag)) |> 
    select(name, method, value) |>
    pivot_wider()

  tex <- 
    gt(data) |> 
    tab_header(
      title = termName,
      subtitle = role
    ) |> 
    fmt_number(decimals = 3) |> 
    as_latex()
  
  write_lines(tex, sprintf("latex/pValues_%s_%s.tex", termName, role))
  
}
