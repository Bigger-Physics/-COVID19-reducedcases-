library(EpiEstim)
library(ggplot2)
library(readxl)

source("COVID19.R")

country <- c("Korea", "Singapore")
start <- c(26, 24)
base_date <- c("2020-05-23", "2020-05-23")
plot_options <- NULL

predict_typical_len <- 30
fit_length <- 10

for (i in seq(1, length(country))) {
  message(country[i])
  estm_result <- estimate_R_simple_model(country[i], 
                                  window = 6, 
                                  start = start[i], 
                                  base_date = base_date[i],
                                  plot_options = list(
                                    r_ylim = NULL,
                                    vline_date = "2020-05-07" 
                                  ))
  
  estm_result <- estimate_R_simple_model(country[i], 
                                         window = 0, 
                                         start = start[i], 
                                         base_date = base_date[i],
                                         plot_options = plot_options)
}


# file.copy("results/Korea/6window/Korea_R.png", 
#           "results/_Paper_How/Korea_R.png", 
#           overwrite = TRUE)
# 
# file.copy("results/Singapore/6window/Singapore_R.png", 
#           "results/_Paper_How/Singapore_R.png", 
#           overwrite = TRUE)

