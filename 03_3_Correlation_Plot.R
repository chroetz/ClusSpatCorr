source("common.R")



# Define Functions --------------------------------------------------------


autoCor <- function(k, mat) {
  n <- nrow(mat)
  v <- mat[cbind(1:(n-k), (1+k):n)]
  tibble(k = k, cor = v)
}



# Main --------------------------------------------------------------------


corrMatYear <- 
  read_csv("results/correlationMatrix_FitFull_Year.csv") |> 
  column_to_rownames("year") |> 
  as.matrix()

n <- nrow(corrMatYear)
autoCorData <- lapply(1:(n-1), autoCor, corrMatYear) |> bind_rows()

plt <- 
  autoCorData |>
  ggplot(aes(x=k, y=cor)) +
  geom_point() + geom_smooth() +
  geom_point(
    data = autoCorData |> summarize(cor = mean(cor), .by = k),
    color="red", stroke = 2, size = 2, shape = 4
  ) +
  geom_hline(yintercept = 0, color = "green") +
  labs(title = "Autocorrelation of Residuals by Number of Lag Years", x = "Lag", y = "Correlation")
ggsave(plot=plt, "plots/temporalCorrelation.pdf", width=4, height=3)
