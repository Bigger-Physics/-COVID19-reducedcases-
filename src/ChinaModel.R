library(readxl)
source("COVID19.R")


# 用于中国除湖北外R的估计
estimate_R_China_model <- function (filename,
                                    window = 6,
                                    start = 2) {
  
  output_dir = paste0(RESULT_ROOT_DIR, "/ChinaModel/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  output_dir = paste0(output_dir, window, "window/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  data <- read_excel(paste0(DATA_DIR, "/China/", filename))
  names(data) <- c("dates", "D_im", "I_im", "I_lc")
  data$dates <- as.Date(data$dates, tryFormats=c("%m/%d"))
  date_range = c("2020-01-20", "2020-02-10")
  selected <- (data$dates >= date_range[1]) & (data$dates <= date_range[2]) 
  data <- data[selected, ]
  # head(data)
  
  data_len <- nrow(data)
  t_start <- seq(start, data_len - window)
  t_end <- t_start + window
  
  result <- estimate_Rs(data,
                        method = "parametric_si",
                        config = make_config(
                          list(
                            t_start = t_start,
                            t_end = t_end,
                            mean_si = COVID19_SI_MEAN,
                            std_si = COVID19_SI_STD
                          )
                        ))
  result$R_im$R$date_start <- result$dates[result$R_im$R$t_start]
  result$R_im$R$date_end <- result$dates[result$R_im$R$t_end]
  result$R_lc$R$date_start <- result$dates[result$R_lc$R$t_start]
  result$R_lc$R$date_end <- result$dates[result$R_lc$R$t_end]
  
  write.table(data,
              paste0(output_dir, "China_incid.csv"),
              sep = ",",
              row.names = FALSE)
  write.table(result$R_im$R,
              paste0(output_dir, "China_Rim.csv"),
              sep = ",",
              row.names = FALSE)
  
  write.table(result$R_lc$R,
              paste0(output_dir, "China_Rlc.csv"),
              sep = ",",
              row.names = FALSE)
  
  write.table(result$R_lc$R,
              paste0(output_dir, "China_R.csv"),
              sep = ",",
              row.names = FALSE)
  
  grid <- plot_estimate_R_lc(result)
  ggsave(
    paste0(output_dir, "China_R.png"),
    plot = grid,
    device = "png",
    dpi = 600,
    width = 6,
    height = 6
  )
  
  return(result)
}

result <- estimate_R_China_model("DataForChinaR.xlsx", 
                                 window=6, 
                                 start=2)

result <- estimate_R_China_model("DataForChinaR.xlsx", 
                                 window=0, 
                                 start=2)



# file.copy("results/ChinaModel/6window/China_R.png", 
#           "results/_Paper_How/China_R.png", 
#           overwrite = TRUE)

# R_im <- data.frame(date = result$dates[result$R_im$R$t_end],
#                 r = result$R_im$R$`Mean(R)`)
# R_lc <- data.frame(date = result$dates[result$R_lc$R$t_end],
#                    r = result$R_lc$R$`Mean(R)`)
# 
# p_im <- ggplot(R_im, aes(date, r)) +
#   geom_line(aes(y = r, colour = "China"))
# p_lc <- ggplot(R_lc, aes(date, r)) +
#   geom_line(aes(y = r, colour = "China"))
# 
# regions <- c("四川", "天津", "安徽", "山东-青岛", "广东-深圳", "河北", "河南", "海南", "甘肃省", "陕西省")
# 
# # regions <- c("四川", "天津", "安徽", "广东-深圳", "河北", "河南", "甘肃省", "陕西省")
# 
# for (i in seq_len(length(regions))) {
#   result <- estimate_R_China_model(paste0(regions[i], ".xlsx"), 
#                                    window=6, 
#                                    start=2)
#   Rr_im <- data.frame(date = result$dates[result$R_im$R$t_end],
#                      r = result$R_im$R$`Mean(R)`)
#   Rr_lc <- data.frame(date = result$dates[result$R_lc$R$t_end],
#                      r = result$R_lc$R$`Mean(R)`)
#   p_im <- p_im +
#     geom_line(data = Rr_im, aes_string(x = "date", y = "r", colour = shQuote(regions[i])))
#   p_lc <- p_lc +
#     geom_line(data = Rr_lc, aes_string(x = "date", y = "r", colour = shQuote(regions[i])))
# }
# 
# colors <- brewer_pal(palette = "Set3")(11)
# 
# p_im <- p_im +
#   ylim(c(0,3)) +
#   scale_colour_manual("", values = colors) +
#   ggtitle("Estimated R_im") +
#   theme_few()
# 
# p_im
# 
# ggsave(
#   paste0(output_dir, "China_Rim.png"),
#   plot = p_im,
#   device = "png",
#   dpi = 600,
#   width = 6,
#   height = 4.5
# )
# 
# p_lc <- p_lc +
#   ylim(c(0,6)) +
#   scale_colour_manual("", values = colors) +
#   ggtitle("Estimated R_lc") +
#   theme_few()
# 
# p_lc
# 
# ggsave(
#   paste0(output_dir, "China_Rlc.png"),
#   plot = p_lc,
#   device = "png",
#   dpi = 600,
#   width = 6,
#   height = 4.5
# )
