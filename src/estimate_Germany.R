library(EpiEstim)
library(ggplot2)
library(readxl)

source("COVID19.R")

country <- "Germany"
window <- 6
start <- 26
base_date =  "2020-05-23"
plot_options <- NULL


estm_result <- estimate_R_simple_model(
  country,
  window = window,
  start = start,
  base_date = base_date,
  plot_options = plot_options
)

plot_start_index <- estm_result$R$t_start[1] - 1

t_plus <- 60
t_minus <- 14
fit_length <- 25
fit_model <- 1
fit_params <- list(a = 0.1,
                   b = 5,
                   c = 0.5)

pred_result_null <- predict_country_null(
  country,
  estm_result,
  t_minus,
  t_plus,
  method = "fittail",
  fit_config = list(
    fit_length = fit_length,
    model = fit_model,
    params = fit_params
  )
)

R_cn <- get_typical_R("China")
pred_result_rcn <-
  predict_country_scenario(country, estm_result, t_minus, t_plus, "rcn", R_cn$R, R_cn$R_d)

# plot_multi_simulations(
#   country,
#   "de-cn",
#   c("de", "de-cn"),
#   list(pred_result_null, pred_result_rcn),
#   plot_start_index
# )

R_kr <- get_typical_R("Korea")
pred_result_rkr <-
  predict_country_scenario(country, estm_result, t_minus, t_plus, "rkr", R_kr$R, R_kr$R_d)

# plot_multi_simulations(
#   country,
#   "de-kr",
#   c("de", "de-kr"),
#   list(pred_result_null, pred_result_rkr),
#   plot_start_index
# )

plot_multi_simulations(
  country,
  "de-cn-kr",
  c("de", "de-cn", "de-kr"),
  list(pred_result_null, pred_result_rcn, pred_result_rkr),
  plot_start_index
)

# R_sg <- get_typical_R("Singapore")
# pred_result_rsg <-
#   predict_country_scenario(country, estm_result, t_minus, t_plus, "rsg", R_sg$R, R_sg$R_d)
# 
# plot_multi_simulations(
#   country,
#   "de-sg",
#   c("de", "de-sg"),
#   list(pred_result_null, pred_result_rsg),
#   plot_start_index
# )
