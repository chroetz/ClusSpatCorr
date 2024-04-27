source("common.R")



# Define Functions --------------------------------------------------------


plotIc <- function(data) {
  plt <- 
    data |> 
    mutate(termLabel = termLabels[termName]) |> 
    mutate(clusterLabel = clusterLabels[clusterName]) |> 
    ggplot(aes(x = lag, y = value, color = clusterLabel, linetype = criterion, shape = criterion)) +
    geom_line() + geom_point() +
    facet_wrap(vars(termLabel), scales = "free_y", ncol=2, labeller=label_parsed) +
    geom_hline(yintercept=0) +
    scale_x_continuous(breaks=c(0:10), minor_breaks=NULL) +
    xlab("Lag (year)") +
    ylab("Information criterium difference to trivial model") +
    guides(
      color = guide_legend(title = "Clustering"),
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
  mutate(value = value - v0) |> 
  plotIc()
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
  mutate(value = value - v0) |> 
  plotIc()
ggsave(plot=plt, "plots/icBackward.pdf", width=7, height=8)






# bicsReference <- read_csv(paste0('datacode/lag_selection/lag_selectionBICs_10.csv'))[[2]][-1]
# aicsReference <- read_csv(paste0('datacode/lag_selection/lag_selectionAICs_10.csv'))[[2]][-1]
# icRef <- 
#   tibble(
#     termName = rep(varns, each=10),
#     lag = rep(10:1, times = 5),
#     Aic = aicsReference,
#     Bic = bicsReference
#   ) |> 
#   pivot_longer(c(Aic, Bic), names_to="ic")
# ic0Ref <- icRef |> filter(lag == 10)
# 
# icRefNormed <- 
#   icRef |> 
#   left_join(ic0Ref |> select(termName, ic, value) |> rename(v0 = value), by = join_by(termName, ic)) |> 
#   mutate(value = value - v0) |> 
#   select(-v0)
# d <- bind_rows(
#   icDataGid0Year |> mutate(lag = 10-lag),
#   icRefNormed |> mutate(cluster = "ref"))
#   
# d |> 
#   filter(cluster %in% c("None", "ref")) |> 
#   ggplot(aes(x = lag, y = value, color = cluster, linetype = ic, shape = ic)) +
#   geom_line() + geom_point() +
#   facet_wrap(vars(termName), scales="free_y") +
#   scale_x_reverse()




