source("common.R")



# Define Functions --------------------------------------------------------


plotCv <- function(cvData, modelName) {
  plt <- 
    cvData |> 
    mutate(termLabel = termLabels[termName]) |> 
    mutate(clusterLabel = clusterLabels[clusterName]) |> 
    ggplot(aes(x = lag, y = cv_mean, color = clusterLabel)) +
    geom_line() + geom_point() +
    geom_hline(yintercept=0) +
    facet_wrap(vars(termLabel), scales = "free_y", ncol=2, labeller=label_parsed) +
    scale_x_continuous(breaks=c(0:10), minor_breaks=NULL) +
    xlab("Number of lag years") +
    ylab(rlang::expr(paste(L^2, " loss difference to ", !!modelName ," model"))) +
    guides(color = guide_legend(title = "Clustering"))
  shift_legend(plt)
}



# Main --------------------------------------------------------------------


cvData <- read_csv("results/crossValidation.csv")

plt <- 
  cvData |> 
  filter(kind == "forward") |> 
  plotCv("trivial")
ggsave(plot=plt, "plots/cvForward.pdf", width=7, height=8)
  
cvFull <- 
  cvData |> 
  filter(kind == "backward", lag == 10) |> 
  select(clusterName, termName, cv_mean) |> 
  rename(v0 = cv_mean)
plt <- 
  cvData |> 
  filter(kind == "backward") |>
  left_join(cvFull, join_by(clusterName, termName)) |>
  mutate(cv_mean = cv_mean - v0) |>
  plotCv("trivial")
ggsave(plot=plt, "plots/cvBackward.pdf", width=7, height=8)

