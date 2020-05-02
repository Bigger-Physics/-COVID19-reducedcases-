source("COVID19.R")


save_China_R_data <- function () {
  output_dir = paste0(RESULT_ROOT_DIR, "/ChinaModel/")
  output_dir = paste0(output_dir, "6window/")
  R_file <- paste0(output_dir, "China_R.csv")
  incid_file <- paste0(output_dir, "China_incid.csv")
  
  
  R_data <- read.table(R_file, header = TRUE, sep = ",")
  incid_data <- read.table(incid_file, header = TRUE, sep = ",")
  
  incid_data <- incid_data[ , c("dates", "I_im", "I_lc")]
  names(incid_data) <- c("date", "I_1", "I_2+")
  
  
  R_data <- R_data[ , c("date_end", "Mean.R.", "Quantile.0.025.R.", "Quantile.0.975.R.")]
  names(R_data) <- c("date", "R_mean", "R_q.025", "R_q.975")
  
  data <- merge(incid_data, R_data, all.x = TRUE)
  write.table(data,
              paste0(output_dir, "China_R_data.csv"),
              sep = ",",
              row.names = FALSE)
}

save_China_R_data()


save_R_data <- function (country) {
  output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
  output_dir = paste0(output_dir, "6window/")
  R_file <- paste0(output_dir, country, "_R.csv")
  incid_file <- paste0(output_dir, country, "_incid.csv")
  
  
  R_data <- read.table(R_file, header = TRUE, sep = ",")
  incid_data <- read.table(incid_file, header = TRUE, sep = ",")
  
  incid_data <- incid_data[ , c("dates", "imported", "local")]
  names(incid_data) <- c("date", "I_0", "I_1+")
  
  
  R_data <- R_data[ , c("date_end", "Mean.R.", "Quantile.0.025.R.", "Quantile.0.975.R.")]
  names(R_data) <- c("date", "R_mean", "R_q.025", "R_q.975")
  
  data <- merge(incid_data, R_data, all.x = TRUE)
  write.table(data,
              paste0(output_dir, country, "_R_data.csv"),
              sep = ",",
              row.names = FALSE)
}


countries <- c("Korea", "Singapore", "USA", "UK", "Italy", "Germany")

for (country in countries) {
  save_R_data(country)
}


save_fig2_data <- function (country, label) {
  output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
  
  null_file <- paste0(output_dir, country, "_null_fittail_7t60_result.csv")
  cn_file <- paste0(output_dir, country, "_rcn_7t60_result.csv")
  kr_file <- paste0(output_dir, country, "_rkr_7t60_result.csv")
  
  null_data <- read.table(null_file, header = TRUE, sep = ",")
  cn_data <- read.table(cn_file, header = TRUE, sep = ",")
  kr_data <- read.table(kr_file, header = TRUE, sep = ",")
  
  null_data <- null_data[ , c("dates", "cum_I_mean", "cum_I_quantile_025",
                              "cum_I_quantile_975", "cum_I_real")]
  null_name_labels <- paste0("C_", label, c("_mean", "_q.025", "_q.975", "_real"))
  names(null_data) <- c("date", null_name_labels)
  
  cn_data <- cn_data[ , c( "cum_I_mean", "cum_I_quantile_025", "cum_I_quantile_975")]
  cn_name_labels <- paste0("C_", label, "-cn", c("_mean", "_q.025", "_q.975"))
  names(cn_data) <- cn_name_labels
  
  kr_data <- kr_data[ , c( "cum_I_mean", "cum_I_quantile_025", "cum_I_quantile_975")]
  kr_name_labels <- paste0("C_", label, "-kr", c("_mean", "_q.025", "_q.975"))
  names(kr_data) <- kr_name_labels
  
  data <- cbind(null_data, cn_data, kr_data)
  write.table(data,
              paste0(output_dir, country, "_", label, "-cn-kr_cum_incidence_data.csv"),
              sep = ",",
              row.names = FALSE)
}


countries <- c("USA", "UK", "Italy", "Germany")
labels <- c("us", "uk", "it", "de")

for (i in seq_len(length(countries))) {
  save_fig2_data(countries[i], labels[i])
}




