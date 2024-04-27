source("common.R")



# Define Functions --------------------------------------------------------


plotCv <- function(cvData) {
  plt <- 
    cvData |> 
    mutate(termLabel = termLabels[termName]) |> 
    mutate(clusterLabel = clusterLabels[clusterName]) |> 
    ggplot(aes(x = lag, y = cv_mean, color = clusterLabel)) +
    geom_line() + geom_point() +
    geom_hline(yintercept=0) +
    facet_wrap(vars(termLabel), scales = "free_y", ncol=2, labeller=label_parsed) +
    scale_x_continuous(breaks=c(0:10), minor_breaks=NULL) +
    xlab("Lag (year)") +
    ylab(expression(paste(L^2, " loss differnce to trivial model"))) +
    guides(color = guide_legend(title = "Clustering"))
  shift_legend(plt)
}



# Main --------------------------------------------------------------------


cvData <- read_csv("results/crossValidation.csv")

plt <- 
  cvData |> 
  filter(kind == "forward") |> 
  plotCv()
ggsave(plot=plt, "plots/cvForward.pdf", width=7, height=8)
  
plt <- 
  cvData |> 
  filter(kind == "backward") |> 
  plotCv()
ggsave(plot=plt, "plots/cvBackward.pdf", width=7, height=8)

