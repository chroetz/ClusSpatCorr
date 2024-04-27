source("common.R")



# Define Functions --------------------------------------------------------


plotConfidence <- function(data, alpha) {
  data <- data |> mutate(termLabel = termLabels[termName])
  colorGuide <- guide_legend(title  = "Clustering")
  plt <- ggplot(
    data |> filter(kind == "effect"),
    aes(x = lag, y = value)
    ) +
    geom_line() + geom_point() + 
    geom_ribbon(
      data = 
        data |> 
        filter(kind %in% c("lower", "upper")) |> 
        pivot_wider(names_from = kind, values_from=value),
      aes(x = lag, ymin = lower, ymax = upper, fill = cluster, color = cluster, y = NULL),
      alpha = alpha) +
    geom_hline(yintercept=0) +
    facet_wrap(vars(termLabel), scales = "free_y", ncol=2, labeller=label_parsed) +
    scale_x_continuous(breaks=c(0:10), minor_breaks=NULL) +
    xlab("Lag (year)") +
    ylab("Growth rate change (%)") +
    guides(fill = colorGuide, color = colorGuide)
  shift_legend(plt)
}



# Main --------------------------------------------------------------------


confiData <- read_csv("results/confidence_FitFull.csv")

plt <- 
  confiData |> 
  filter(
    method %in% c(
      "clusterRegion", "clusterCountryYear", "None",
      "clusterCountry", "clusterYear", "clusterRegionYear"),
    str_starts(name, "TERM_")
  ) |>
  mutate(cluster = str_sub(method, 8)) |> 
  plotConfidence(alpha = 0.1)
ggsave(plot=plt, "plots/coefConfidence.pdf", width=7, height=8)
