source("common.R")



# Define Functions --------------------------------------------------------


plotIc <- function(data, modelName) {
  plt <- 
    data |> 
    mutate(termLabel = termLabels[termName]) |> 
    ggplot(aes(x = lag, y = value, color = Adjusted, linetype = criterion, shape = criterion)) +
    geom_line() + geom_point() +
    facet_wrap(vars(termLabel), scales = "free_y", ncol=2, labeller=label_parsed) +
    geom_hline(yintercept=0) +
    scale_x_continuous(breaks=c(0:10), minor_breaks=NULL) +
    xlab("Number of lag years") +
    ylab(sprintf("Information criterion difference to %s model", modelName)) +
    guides(
      shape = guide_legend(title = "Criterion"),
      linetype = guide_legend(title = "Criterion"))
  shift_legend(plt)
}



# Main --------------------------------------------------------------------


icData <- read_csv("results/informationCriteria.csv")

ic0 <- 
  icData |> 
  filter(kind == "trivial") |> 
  select(clusterName, criterion, value) |> 
  rename(v0 = value)
plt <- 
  icData |>
  filter(kind == "forward", criterion %in% c("AIC", "BIC")) |>
  left_join(ic0, join_by(clusterName, criterion)) |> 
  mutate(
    value = value - v0,
    Adjusted = ifelse(clusterName == "Gid0Year", "Yes", "No")
  ) |> 
  plotIc("trivial")
ggsave(plot=plt, "plots/icForward.pdf", width=7, height=8)


icFull <- 
  icData |> 
  filter(kind == "full") |> 
  select(clusterName, criterion, value) |> 
  rename(v0 = value)
plt <-
  icData |>
  filter(kind == "backward", criterion %in% c("AIC", "BIC")) |>
  left_join(icFull, join_by(clusterName, criterion)) |> 
  mutate(
    value = value - v0,
    Adjusted = ifelse(clusterName == "Gid0Year", "Yes", "No")
  ) |> 
  plotIc("full")
ggsave(plot=plt, "plots/icBackward.pdf", width=7, height=8)
