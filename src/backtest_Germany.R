library(EpiEstim)
library(ggplot2)
library(readxl)

source("COVID19.R")

country <- "Germany"
window <- 6
start <- 26
base_date =  "2020-05-23"
plot_options <- NULL

output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


estm_result <- estimate_R_simple_model(
  country,
  window = window,
  start = start,
  base_date = base_date,
  plot_options = plot_options
)

T_index <- length(estm_result$dates)
I_t <- cumsum(estm_result$I)[T_index]
date_t <- estm_result$dates[T_index]

t_minus_n <- 70
predict_len <- 60


fit_model <- 1
fit_params <- list(a=0.1,
                   b=5,
                   c=0.5)
fit_length <- seq(30, 50)
I_s <- rep(NA, length(fit_length))
for (i in seq_len(length(fit_length))) {
  pred_result_null <- predict_country_null(
    country,
    estm_result,
    0,
    predict_len + fit_length[i],
    method = "fittail",
    fit_config = list(
      fit_length = fit_length[i],
      model = fit_model,
      params = fit_params
    )
  )
  I_s[i] <- pred_result_null$I[nrow(pred_result_null$I), "cum_I_mean"]
}

I_s_mean <- mean(I_s)
I_s_std <- sd(I_s)
I_s_lb <- I_s_mean - I_s_std
I_s_ub <- I_s_mean + I_s_std


scenario_null = "de"

# scenario <- "de-cn"
# R_data <- get_typical_R("China")

scenario <- "de-kr"
R_data <- get_typical_R("Korea")

# scenario <- "de-sg"
# R_data <- get_typical_R("Singapore")

incid_t <- NULL
incid_s <- NULL

for (t_minus_i in seq(t_minus_n, 1)) {
  pred_result <- predict_country_scenario(country,
                                          estm_result,
                                          t_minus_i,
                                          predict_len,
                                          scenario,
                                          R_data$R,
                                          R_data$R_d)
  
  data <- pred_result$I[T_index,
                        c("cum_I_mean", "cum_I_quantile_025", "cum_I_quantile_975")]
  if (is.null(incid_t)) {
    incid_t <- data
  } else {
    incid_t <- rbind(incid_t, data)
  }
  
  data <- pred_result$I[nrow(pred_result$I),
                        c("cum_I_mean", "cum_I_quantile_025", "cum_I_quantile_975")]
  if (is.null(incid_s)) {
    incid_s <- data
  } else {
    incid_s <- rbind(incid_s, data)
  }
}

incid_t$dates <-
  seq.Date(estm_result$dates[T_index - t_minus_n + 1], by = 1,
           length.out = t_minus_n)

incid_s$dates <-
  seq.Date(estm_result$dates[T_index - t_minus_n + 1], by = 1,
           length.out = t_minus_n)

result_data <- list(
  incid_t = incid_t,
  incid_s = incid_s,
  I_t = I_t,
  I_s = I_s,
  date_t = date_t,
  I_s_mean = I_s_mean,
  I_s_std = I_s_std,
  I_s_lb = I_s_lb,
  I_s_ub = I_s_ub
)
save(result_data,
     file = paste0(output_dir, country, "_", scenario, "_backtest.rda"))

#-------------------------------------------------------------------------------
# scenario_null = "de"
# scenario <- "de-kr"
# 
# load(file = paste0(output_dir, country, "_", scenario, "_backtest.rda"))
# incid_t <- result_data$incid_t
# incid_s <- result_data$incid_s
# I_t <- result_data$I_t
# date_t <- result_data$date_t
# I_s_mean <- result_data$I_s_mean
# I_s_std <- result_data$I_s_std
# I_s_lb <- result_data$I_s_lb
# I_s_ub <- result_data$I_s_ub
#-------------------------------------------------------------------------------


colors <- rep(few_pal("Dark")(2), each = 2)
fill_colors <- alpha(colors, c(0.4, 0.3, 0.4, 0.3))
linetypes <- c(1, 2, 1, 2)

subcripts = c(scenario, scenario_null)
labels <- c(bquote(italic(C)[paste("", .(subcripts[1]))](paste(t == T, ";", tau))),
            bquote(italic(C)[paste("", .(subcripts[2]))](t == T)),
            bquote(italic(C)[paste("", .(subcripts[1]))](paste(
              t == infinity, ";", tau
            ))),
            bquote(italic(C)[paste("", .(subcripts[2]))](t == infinity)))

ggplot(incid_s, aes(dates, cum_I_mean)) +
  geom_line(aes(
    y = cum_I_mean,
    colour = "3",
    linetype = "3"
  )) +
  geom_hline(aes(
    yintercept = I_s_mean,
    colour = "4",
    linetype = "4"
  )) +
  geom_line(data = incid_t,
            aes(
              x = dates,
              y = cum_I_mean,
              colour = "1",
              linetype = "1"
            )) +
  geom_hline(aes(
    yintercept = I_t,
    colour = "2",
    linetype = "2"
  )) +
  geom_vline(xintercept = date_t, linetype = "dotted") +
  annotate(
    "text",
    x = date_t,
    y = 0,
    label = "italic(tau==T)",
    parse = TRUE
  ) +
  
  geom_ribbon(aes(ymin = cum_I_quantile_025,
                  ymax = cum_I_quantile_975),
              fill = fill_colors[3]) +
  geom_ribbon(
    data = incid_t,
    aes(x = dates,
        ymin = cum_I_quantile_025,
        ymax = cum_I_quantile_975),
    fill = fill_colors[1]
  ) +
  geom_ribbon(aes(ymin = cum_I_mean,
                  ymax = I_s_mean),
              fill = fill_colors[4]) +
  geom_ribbon(aes(ymin = I_s_lb,
                  ymax = I_s_ub),
              fill = alpha("gray", 0.4)) +
  geom_ribbon(data = incid_t,
              aes(ymin = cum_I_mean,
                  ymax = I_t),
              fill = fill_colors[2]) +
  
  ylim(c(0, 250000)) +
  scale_colour_manual(name = NULL,
                      values = colors,
                      labels = labels) +
  # scale_fill_manual(name = NULL, values = fill_colors) +
  scale_linetype_manual(name = NULL,
                        values = linetypes,
                        labels = labels) +
  xlab(expression(italic(tau))) +
  ylab("Cumulative Incidence") +
  scale_x_date(date_labels = "%m-%d") +
  theme_few() +
  # theme(legend.position = "none") +
  theme(
    legend.text = element_text(family = "serif"),
    legend.text.align = 0,
    legend.position = c(0.01, 1),
    legend.justification = c(0, 1),
    # legend.spacing.x = unit(0.02, "npc"),
    legend.background = element_blank()
  ) +
  guides(col = guide_legend(nrow = 2), fill = FALSE)


ggsave(
  paste0(output_dir, country, "_", scenario, "_backtest.png"),
  device = "png",
  dpi = 600,
  width = 6,
  height = 4.5
)


population <- 83783942
reduced_t <- I_t - incid_t$cum_I_mean
reduced_t_pct <- reduced_t / I_t * 100
reduced_t_pct_population <- reduced_t / population * 100

reduced_s <- I_s_mean - incid_s$cum_I_mean
reduced_s_pct <- reduced_s / I_s_mean * 100
reduced_s_pct_population <- reduced_s / population * 100

reduced_s_lb <- I_s_lb - incid_s$cum_I_mean
reduced_s_pct_lb <- reduced_s_lb / I_s_lb * 100
reduced_s_pct_population_lb <- reduced_s_lb / population * 100

reduced_s_ub <- I_s_ub - incid_s$cum_I_mean
reduced_s_pct_ub <- reduced_s_ub / I_s_ub * 100
reduced_s_pct_population_ub <- reduced_s_ub / population * 100

df_reduced <- data.frame(
  tau = incid_t$dates,
  reduced_t = round(reduced_t, 0),
  reduced_t_pct = round(reduced_t_pct, 2),
  reduced_t_pct_population = round(reduced_t_pct_population, 2),
  reduced_s = round(reduced_s, 0),
  reduced_s_pct = round(reduced_s_pct, 2) ,
  reduced_s_pct_population = round(reduced_s_pct_population, 2),
  reduced_s_lb = round(reduced_s_lb, 0),
  reduced_s_pct_lb = round(reduced_s_pct_lb, 2) ,
  reduced_s_pct_population_lb = round(reduced_s_pct_population_lb, 2),
  reduced_s_ub = round(reduced_s_ub, 0),
  reduced_s_pct_ub = round(reduced_s_pct_ub, 2) ,
  reduced_s_pct_population_ub = round(reduced_s_pct_population_ub, 2)
)

write.table(
  df_reduced,
  paste0(output_dir, country, "_", scenario, "_reducedcases.csv"),
  sep = ",",
  row.names = FALSE
)



names(incid_t) <- c(paste0("C_", scenario, "_T", c("_mean", "_q.025", "_q.975")),
                    "date_T")
names(incid_s) <- c(paste0("C_", scenario, "_E", c("_mean", "_q.025", "_q.975")),
                    "date_E")
data <- cbind(incid_t, incid_s)
data$C_T <- I_t
data$C_E_mean <- I_s_mean
data$C_E_std<- I_s_std
data$C_E_lb <- I_s_lb
data$C_E_ub <- I_s_ub
names(data)[9] <- paste0("C_", scenario_null, "_T")
names(data)[seq(10, 13)] <- paste0("C_", scenario_null, "_E",
                                   c("_mean", "_std", "_lb", "_ub"))
write.table(
  data,
  paste0(output_dir, country, "_", scenario, "_backtest_data.csv"),
  sep = ",",
  row.names = FALSE
)
