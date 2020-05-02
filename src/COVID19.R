library(EpiEstim)
library(ggplot2)
library(gridExtra)
library(scales)
library(ggthemes)
library(readxl)


# SI分布参数
COVID19_SI_MEAN = 4.4
COVID19_SI_STD = 3.0

# COVID19_SI_MEAN = 5.80
# COVID19_SI_STD = 3.95

# 数据所在目录
DATA_DIR = "../COVID19Data"

# 结果保存目录
RESULT_ROOT_DIR = "results"
if (!dir.exists(RESULT_ROOT_DIR)) {
  dir.create(RESULT_ROOT_DIR)
}


# 与estimate_R的plot类似，但只画出R和incidence
plot_estimate_R<- function (result, 
                            options = list(
                              r_ylim = NULL
                            )) {
  # colors <- brewer_pal(palette = "Set1")(3)
  colors <- few_pal("Dark")(2)
  
  x_limits = c(min(result$dates[result$R$t_start]) - 1, 
               max(result$dates[result$R$t_end]) + 1)
  
  incid = data.frame(dates = result$dates,
                     imported = result$I_imported,
                     local = result$I_local)
  data <- reshape2::melt(incid, id = c("dates"), variable.name = "group", 
                         value.name = "incidence")
  
  labels <-
    c(expression(italic(I)^{paste("0")}), expression(italic(I)^{paste("1+")}))
  
  p_incid <- ggplot(data, aes(x = dates, y = incidence, fill = group)) + 
    geom_col(position = "stack") + 
    scale_fill_manual(name = NULL, 
                      values = alpha(colors, 1), 
                      labels = labels) + 
    xlab("") +
    ylab("Incidence") +
    xlim(c(min(result$dates) - 1, max(result$dates) + 1)) + 
    # ggtitle("Epidemic curve") +
    theme_few() + 
    theme(legend.text = element_text(family = "serif"),
          legend.text.align = 0,
          legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1),
          legend.background = element_blank()) 
  if (class(result$dates) == "Date") {
    p_incid <- p_incid + scale_x_date(date_labels = "%m-%d", limits = x_limits)
  }
  
  df <- data.frame(
    start = result$dates[result$R$t_start],
    end = result$dates[result$R$t_end],
    R_mean = result$R[, "Mean(R)"],
    R_lower = result$R[, "Quantile.0.025(R)"],
    R_upper = result$R[, "Quantile.0.975(R)"]
  )
  if (is.null(options$r_ylim)) {
    options$r_ylim <- c(0, max(df[, grep("upper", names(df))],na.rm = TRUE))
  }
  p_R <- ggplot(df, aes(end, R_mean)) +
    geom_line(aes(y = R_mean, colour = "Mean")) +
    geom_hline(yintercept = 1, linetype = "dotted") +
    geom_ribbon(aes(ymin = R_lower, ymax = R_upper, fill = "95%CI")) +
    scale_colour_manual(name = NULL, values = colors[1]) +
    scale_fill_manual(name = NULL, values = alpha(colors[1], 0.2)) +
    xlab("Time") +
    ylab("R") +
    xlim(c(min(result$dates) - 1, max(result$dates) + 1)) +
    ylim(options$r_ylim) +
    # ggtitle("Estimated R") +
    theme_few() + 
    theme(legend.position = "none")
  if (class(result$dates) == "Date") {
    p_R <- p_R + scale_x_date(date_labels = "%m-%d", limits = x_limits)
  }
  
  gA <- ggplotGrob(p_incid)
  gB <- ggplotGrob(p_R)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  
  out <- list(incid = gA, R = gB)
  out.grid <- arrangeGrob(grobs = out,
                          nrow = 2,
                          ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  return(out.grid)
}


# estimate_Rs画图
plot_estimate_Rs <- function (result,
                              options = list(r_ylim = NULL), ...) {
  if (result$model == "2-region") {
    # colors <- brewer_pal(palette = "Set1")(3)
    colors <- few_pal("Dark")(3)
    
    x_limits = c(min(result$R_lc$dates[result$R_lc$R$t_start]) - 1, 
               max(result$R_lc$dates[result$R_lc$R$t_end]) + 1)

    incid = data.frame(
      dates = result$dates,
      D_im = result$D_im,
      I_im = result$I_im,
      I_lc = result$I_lc
    )
    data <-
      reshape2::melt(
        incid,
        id = c("dates"),
        variable.name = "group",
        value.name = "incidence"
      )
    labels <-
      c(expression(italic(I)^{paste("0")}),
        expression(italic(I)^{paste("1")}),
        expression(italic(I)^{paste("2+")}))
    
    p_incid <-
      ggplot(data, aes(x = dates, y = incidence, fill = group)) +
      geom_col(position = "stack") +
      scale_fill_manual(name = NULL,
                        values = alpha(colors, 1),
                        labels = labels) +
      xlab("") +
      ylab("Incidence") +
      xlim(c(min(result$dates) - 1, max(result$dates) + 1)) +
      theme_few() + 
      theme(legend.text = element_text(family = "serif"),
            legend.text.align = 0,
            legend.position = c(0.01, 0.99),
            legend.justification = c(0, 1),
            legend.background = element_blank())
    if (class(result$dates) == "Date") {
      p_incid <- p_incid + scale_x_date(date_labels = "%m-%d", 
                                        limits = x_limits)
    }
    
    df <- data.frame(
      start_im = result$R_im$dates[result$R_im$R$t_start],
      end_im = result$R_im$dates[result$R_im$R$t_end],
      R_im_mean = result$R_im$R[, "Mean(R)"],
      R_im_lower = result$R_im$R[, "Quantile.0.025(R)"],
      R_im_upper = result$R_im$R[, "Quantile.0.975(R)"],
      start_lc = result$R_lc$dates[result$R_lc$R$t_start],
      end_lc = result$R_lc$dates[result$R_lc$R$t_end],
      R_lc_mean = result$R_lc$R[, "Mean(R)"],
      R_lc_lower = result$R_lc$R[, "Quantile.0.025(R)"],
      R_lc_upper = result$R_lc$R[, "Quantile.0.975(R)"]
    )
    labels_R <-
      c(expression(italic(R)[0]^1), 
        expression(italic(R)[paste("1+")]^{paste("2+")}))
    if (is.null(options$r_ylim)) {
      options$r_ylim <-
        c(0, max(df[, grep("upper", names(df))], na.rm = TRUE))
    }
    p_R <- ggplot(df, aes(end_im, R_im_mean)) +
      geom_line(aes(y = R_im_mean, colour = "Rimmean")) +
      geom_line(aes(y = R_lc_mean, colour = "Rlcmean")) +
      geom_hline(yintercept = 1, linetype = "dotted") +
      geom_ribbon(aes(
        ymin = R_im_lower,
        ymax = R_im_upper,
        fill = "Rim95%CI"
      )) +
      geom_ribbon(aes(
        ymin = R_lc_lower,
        ymax = R_lc_upper,
        fill = "Rlc95%CI"
      )) +
      scale_colour_manual(name = NULL,
                          values = colors,
                          labels = labels_R) +
      scale_fill_manual(name = NULL,
                        values = alpha(colors, 0.2),
                        labels = labels_R) +
      xlab("Time") +
      ylab("R") +
      xlim(c(min(result$dates) - 1, max(result$dates) + 1)) +
      ylim(options$r_ylim) +
      theme_few() + 
      theme(legend.text = element_text(family = "serif"),
            legend.text.align = 0,
            legend.position = c(0.01, 0.99),
            legend.justification = c(0, 1),
            legend.background = element_blank())
    if (class(result$dates) == "Date") {
      p_R <- p_R + scale_x_date(date_labels = "%m-%d", 
                                        limits = x_limits)
    }
    
    gA <- ggplotGrob(p_incid)
    gB <- ggplotGrob(p_R)
    maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
    gA$widths[2:5] <- as.list(maxWidth)
    gB$widths[2:5] <- as.list(maxWidth)
    
    out <- list(incid = gA, R = gB)
    out.grid <- arrangeGrob(grobs = out,
                            nrow = 2,
                            ncol = 1)
    grid.arrange(out.grid, newpage = FALSE)
    return(out.grid)
    
  } else {
    stop("Plot method is not yet supported for 3-region model.")
  }
}


plot_estimate_R_lc <- function (result,
                                options = list(r_ylim = NULL), ...) {
  # colors <- brewer_pal(palette = "Set1")(2)
  colors <- few_pal("Dark")(2)
  
  x_limits = c(min(result$R_lc$dates[result$R_lc$R$t_start]) - 1, 
               max(result$R_lc$dates[result$R_lc$R$t_end]) + 1)
  
  incid = data.frame(
    dates = result$dates,
    I_im = result$I_im,
    I_lc = result$I_lc
  )
  data <-
    reshape2::melt(
      incid,
      id = c("dates"),
      variable.name = "group",
      value.name = "incidence"
    )
  labels <-
    c(expression(italic(I)^{paste("1")}), expression(italic(I)^{paste("2+")}))
  p_incid <-
    ggplot(data, aes(x = dates, y = incidence, fill = group)) +
    geom_col(position = "stack") +
    scale_fill_manual(name = NULL,
                      values = alpha(colors, 1),
                      labels = labels) +
    xlab("") +
    ylab("Incidence") +
    xlim(c(min(result$dates) - 1, max(result$dates) + 1)) +
    # ggtitle("Epidemic curve") +
    theme_few() +
    theme(
      legend.text = element_text(family = "serif"),
      legend.text.align = 0,
      legend.position = c(0.01, 0.99),
      legend.justification = c(0, 1),
      legend.background = element_blank()
    )
  if (class(result$dates) == "Date") {
    p_incid <- p_incid + scale_x_date(date_labels = "%m-%d", limits = x_limits)
  }
  
  df <- data.frame(
    start_lc = result$R_lc$dates[result$R_lc$R$t_start],
    end_lc = result$R_lc$dates[result$R_lc$R$t_end],
    R_lc_mean = result$R_lc$R[, "Mean(R)"],
    R_lc_lower = result$R_lc$R[, "Quantile.0.025(R)"],
    R_lc_upper = result$R_lc$R[, "Quantile.0.975(R)"]
  )
  if (is.null(options$r_ylim)) {
    options$r_ylim <-
      c(0, max(df[, grep("upper", names(df))], na.rm = TRUE))
  }
  p_R <- ggplot(df, aes(end_lc, R_lc_mean)) +
    geom_line(aes(y = R_lc_mean, colour = "Rlcmean")) +
    geom_hline(yintercept = 1, linetype = "dotted") +
    geom_ribbon(aes(
      ymin = R_lc_lower,
      ymax = R_lc_upper,
      fill = "Rlc95%CI"
    )) +
    scale_colour_manual(name = NULL,
                        values = colors[1]) +
    scale_fill_manual(name = NULL,
                      values = alpha(colors[1], 0.2)) +
    xlab("Time") +
    ylab("R") +
    xlim(c(min(result$dates) - 1, max(result$dates) + 1)) +
    ylim(options$r_ylim) +
    # ggtitle(expression("Estimated " * italic(R[Im]) * " and " * italic(R[Lc]))) +
    theme_few() +
    theme(legend.position = "none")
  
  if (class(result$dates) == "Date") {
    p_R <- p_R + scale_x_date(date_labels = "%m-%d", limits = x_limits)
  }
  
  gA <- ggplotGrob(p_incid)
  gB <- ggplotGrob(p_R)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  
  out <- list(incid = gA, R = gB)
  out.grid <- arrangeGrob(grobs = out,
                          nrow = 2,
                          ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  return(out.grid)
}


# 画出三组incidence: D_im I_im I_lc
plot_incidence_3groups<- function (incid) {
  # colors <- brewer_pal(palette = "Set1")(3)
  colors <- few_pal("Dark")(3)
  data <- reshape2::melt(incid, id = c("dates"), variable.name = "group", 
                         value.name = "incidence")
  labels <- c(expression(italic(D[Im])), expression(italic(I[Im])), 
              expression(italic(I[Lc])))
  p_incid <- ggplot(data, aes(x = dates, y = incidence, fill = group)) + 
    geom_col(position = "stack") + 
    scale_fill_manual(name = NULL, values = alpha(colors, 1), labels = labels) + 
    xlab("Time") +
    ylab("Incidence") +
    xlim(c(min(incid$dates), max(incid$dates) + 1)) +
    ggtitle("Epidemic curve") +
    theme_few() + 
    theme(legend.text = element_text(family = "serif"),
          legend.text.align = 0,
          legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1),
          legend.background = element_blank())
  
  if (class(incid$dates) == "Date") {
    p_incid <- p_incid + scale_x_date(date_labels = "%m-%d",
                                      limits = c(min(incid$dates), max(incid$dates) + 1))
  }
  
  out <- list(incid = p_incid)
  out.grid <- arrangeGrob(grobs = out, nrow = 1, ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  return(out.grid)
}


# 画出两组incidence： imported local
plot_incidence_2groups<- function (incid) {
  # colors <- brewer_pal(palette = "Set1")(3)
  colors <- few_pal("Dark")(3)
  data <- reshape2::melt(incid, id = c("dates"), variable.name = "group", 
                         value.name = "incidence")
  p_incid <- ggplot(data, aes(x = dates, y = incidence, fill = group)) + 
    geom_col(position = "stack") + 
    scale_fill_manual(name = NULL, values = alpha(colors, 1)) + 
    xlab("Time") +
    ylab("Incidence") +
    xlim(c(min(incid$dates), max(incid$dates) + 1)) +
    ggtitle("Epidemic curve") +
    theme_few() + 
    theme(legend.text = element_text(family = "serif"),
          legend.text.align = 0,
          legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1),
          legend.background = element_blank())
  
  if (class(incid$dates) == "Date") {
    p_incid <- p_incid + scale_x_date(date_labels = "%m-%d",
                                      limits = c(min(incid$dates), max(incid$dates) + 1))
  }
  
  out <- list(incid = p_incid)
  out.grid <- arrangeGrob(grobs = out, nrow = 1, ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  return(out.grid)
}


# 画出EpiEstim3估计的 R_im R_lc（result），以及原EpiEstim估计的R（result_v2）
plot_compare_Rs<- function (result, result_v2) {
  
  # colors <- brewer_pal(palette = "Set1")(3)
  colors <- few_pal("Dark")(3)
  df <- data.frame(
    start_im = result$R_im$dates[result$R_im$R$t_start],
    end_im = result$R_im$dates[result$R_im$R$t_end],
    R_im_mean = result$R_im$R[, "Mean(R)"],
    R_im_lower = result$R_im$R[, "Quantile.0.025(R)"],
    R_im_upper = result$R_im$R[, "Quantile.0.975(R)"],
    start_lc = result$R_lc$dates[result$R_lc$R$t_start],
    end_lc = result$R_lc$dates[result$R_lc$R$t_end],
    R_lc_mean = result$R_lc$R[, "Mean(R)"],
    R_lc_lower = result$R_lc$R[, "Quantile.0.025(R)"],
    R_lc_upper = result$R_lc$R[, "Quantile.0.975(R)"]
  )
  
  df2 <- data.frame(
    start = result_v2$dates[result_v2$R$t_start],
    end = result_v2$dates[result_v2$R$t_end],
    R_mean = result_v2$R[, "Mean(R)"],
    R_lower = result_v2$R[, "Quantile.0.025(R)"],
    R_upper = result_v2$R[, "Quantile.0.975(R)"]
  )
  df <- cbind(df, df2)
  
  labels_R <- c(expression(italic(R[Im])), expression(italic(R[Lc])), expression(italic(R)))
  
  p_R <- ggplot(df, aes(end_im, R_im_mean)) +
    geom_line(aes(y = R_im_mean, colour = "Rimmean")) +
    geom_line(aes(y = R_lc_mean, colour = "Rlcmean")) +
    geom_line(aes(y = R_mean, colour = "Rv2Mean")) +
    geom_hline(yintercept = 1, linetype = "dotted") +
    geom_ribbon(aes(ymin = R_im_lower, ymax = R_im_upper, fill = "Rim95%CI")) +
    geom_ribbon(aes(ymin = R_lc_lower, ymax = R_lc_upper, fill = "Rlc95%CI")) +
    geom_ribbon(aes(ymin = R_lower, ymax = R_upper, fill = "Rv295%CI")) +
    scale_colour_manual(name = NULL, values = colors, labels = labels_R) +
    scale_fill_manual(name = NULL, values = alpha(colors, 0.2), 
                      labels = labels_R) +
    xlab("Time") +
    ylab("R") +
    xlim(c(min(result$dates), max(result$dates) + 1)) +
    theme_few() + 
    theme(legend.text = element_text(family = "serif"),
          legend.text.align = 0,
          legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1),
          legend.background = element_blank())
  
  if (class(result$dates) == "Date") {
    p_R <- p_R + scale_x_date(date_labels = "%m-%d",
                              limits = c(min(result$dates), max(result$dates) + 1))
  }
  
  out <- list(R = p_R)
  out.grid <- arrangeGrob(grobs = out, nrow = 1, ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  return(out.grid)
  
}


# 模拟多次取平均
simulate_I_rep <- function (R,
                            I_imported = NULL,
                            mean_im = NULL,
                            si_distr = NULL,
                            mean_si = NULL,
                            std_si = NULL,
                            num_repeats = 1,
                            I_past = NULL) {
  I <- NULL
  for (i in seq(1, num_repeats)) {
    result <- simulate_I(
      R = R,
      I_imported = I_imported,
      mean_im = mean_im,
      si_distr = si_distr,
      mean_si = mean_si,
      std_si = std_si,
      I_past = I_past)
    
    # result <- simulate_incid(
    #   r = R,
    #   incid_imported = I_imported,
    #   si_distr = si_distr,
    #   mean_si = mean_si,
    #   std_si = std_si,
    #   incid_past = I_past)
    
    if (is.null(I)) {
      I <- data.frame(result$I)
    } else {
      I <- cbind(I, data.frame(result$I))
    }
  }
  
  names(I) <- paste0("I", seq(1, num_repeats))
  
  if (!is.null(I_past)) {
    I_past <- I_past$local + I_past$imported
    I_past <- as.data.frame(t(sapply(I_past, rep, num_repeats)))
    names(I_past) <- paste0("I", seq(1, num_repeats))
    I <- rbind(I_past, I)
  }
  
  I_mean <- apply(I, 1, mean)
  I_quantile_025 <- apply(I, 1, quantile, 0.025, na.rm = TRUE)
  I_quantile_975 <- apply(I, 1, quantile, 0.975, na.rm = TRUE)
  
  cum_I <- apply(I, 2, cumsum)
  
  cum_I_mean <- apply(cum_I, 1, mean)
  cum_I_quantile_025 <- apply(cum_I, 1, quantile, 0.025, na.rm = TRUE)
  cum_I_quantile_975 <- apply(cum_I, 1, quantile, 0.975, na.rm = TRUE)
  
  I <- data.frame(I_mean, I_quantile_025, I_quantile_975,
                  cum_I_mean, cum_I_quantile_025, cum_I_quantile_975)
  
  return(list(R = R, I = I))
}


# 模拟多次取平均
simulate_Is_rep <- function (R_im,
                             R_lc,
                             D_im = NULL,
                             mean_im = NULL,
                             si_distr = NULL,
                             mean_si = NULL,
                             std_si = NULL,
                             num_repeats = 1,
                             I_past = NULL) {
  I <- NULL
  
  for (i in seq(1, num_repeats)) {
    result <- simulate_Is(
      R_im,
      R_lc,
      D_im = D_im,
      mean_im = mean_im,
      si_distr = si_distr,
      mean_si = mean_si,
      std_si = std_si,
      I_past = I_past)
    
    I_tmp <- data.frame(result$D_im + result$I_im + result$I_lc)
    if (is.null(I)) {
      I <- I_tmp
    } else {
      I <- cbind(I, I_tmp)
    }
  }
  
  names(I) <- paste0("I", seq(1, num_repeats))
  
  if (!is.null(I_past)) {
    I_past <- I_past$D_im + I_past$I_im + I_past$I_lc
    I_past <- as.data.frame(t(sapply(I_past, rep, num_repeats)))
    names(I_past) <- paste0("I", seq(1, num_repeats))
    I <- rbind(I_past, I)
  }
  
  I_mean <- apply(I, 1, mean)
  I_quantile_025 <- apply(I, 1, quantile, 0.025, na.rm = TRUE)
  I_quantile_975 <- apply(I, 1, quantile, 0.975, na.rm = TRUE)
  
  cum_I <- apply(I, 2, cumsum)
  
  cum_I_mean <- apply(cum_I, 1, mean)
  cum_I_quantile_025 <- apply(cum_I, 1, quantile, 0.025, na.rm = TRUE)
  cum_I_quantile_975 <- apply(cum_I, 1, quantile, 0.975, na.rm = TRUE)
  
  I <- data.frame(I_mean, I_quantile_025, I_quantile_975,
                  cum_I_mean, cum_I_quantile_025, cum_I_quantile_975)
  
  return(list(R_im = R_im, R_lc = R_lc, I = I))
}


# 保存simulation结果
plot_and_save_simulation <- function(dates,
                                     I,
                                     t_index,
                                     output_dir,
                                     prefix) {
  df <- data.frame(dates = dates)
  df <- cbind(df, I)
  
  df2 <- df[df$dates >= dates[t_index], ]
  
  write.table(
    df,
    paste0(output_dir, prefix, "_result.csv"),
    sep = ",",
    row.names = FALSE
  )
  
  # colors <- brewer_pal(palette = "Set1")(3)[1]
  colors <- few_pal("Dark")(2)
  
  p1 <- ggplot(df, aes(dates, I_mean)) +
    geom_line(data = df2, aes(y = I_mean, colour = "Mean")) +
    geom_vline(xintercept = dates[t_index], linetype = "dotted") +
    annotate("text", x = dates[t_index], y = 0, 
             label = expression(italic(t==tau))) +
    geom_ribbon(data = df2,
                aes(ymin = I_quantile_025,
                    ymax = I_quantile_975,
                    fill = "95%CI")) +
    scale_colour_manual(name = NULL,
                        values = colors) +
    scale_fill_manual(name = NULL,
                      values = alpha(colors, 0.2)) +
    xlab("") +
    ylab("Incidence") +
    scale_x_date(date_labels = "%m-%d", limits = c(min(df$dates), max(df$dates) + 1)) +
    theme_few() + 
    theme(legend.position = "none")
  
  if ("I_real" %in% names(df)) {
    p1 <- p1 + geom_point(aes(y = I_real, colour = "Mean"), shape=1)
  }
  
  p2 <- ggplot(df, aes(dates, cum_I_mean)) +
    geom_line(data = df2, aes(y = cum_I_mean, colour = "Mean")) +
    geom_vline(xintercept = dates[t_index], linetype = "dotted") +
    annotate("text", x = dates[t_index] , y = 0, 
             label = expression(italic(t==tau))) +
    geom_ribbon(data = df2,
                aes(ymin = cum_I_quantile_025,
                    ymax = cum_I_quantile_975,
                    fill = "95%CI")) +
    scale_colour_manual(name = NULL,
                        values = colors) +
    scale_fill_manual(name = NULL,
                      values = alpha(colors, 0.2)) +
    xlab("Time") +
    ylab("Cumulative Incidence") +
    scale_x_date(date_labels = "%m-%d", limits = c(min(df$dates), max(df$dates) + 1)) +
    theme_few() + 
    theme(legend.position = "none")
  
  if ("cum_I_real" %in% names(df)) {
    p2 <- p2 + geom_point(aes(y = cum_I_real,  colour = "Mean"), shape=1)
  }
  
  gA <- ggplotGrob(p1)
  gB <- ggplotGrob(p2)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  out <- list(incid = gA, R = gB)
  out.grid <- arrangeGrob(grobs = out,
                          nrow = 2,
                          ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  
  ggsave(
    paste0(output_dir, prefix, "_incidence.png"),
    plot = out.grid,
    device = "png",
    dpi = 600,
    width = 6,
    height = 6
  )
  
  return(out.grid)
}


# 保存simulation结果
plot_multi_simulations <- function(country,
                                 scenario,
                                 labels,
                                 results, 
                                 plot_start_index = 1) {
  
  output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  t_index <- results[[1]]$dates[results[[1]]$t_index]
  T_index <- results[[1]]$dates[results[[1]]$T_index]
  plot_start_date <- results[[1]]$dates[plot_start_index]
  
  # colors <- brewer_pal(palette = "Set1")(3)[1]
  colors <- few_pal("Dark")(3)
  
  p1_labels <- lapply(labels, 
                      function (x) {bquote(italic(I)[paste("", .(x))])})
  p2_labels <- lapply(labels, 
                      function (x) {bquote(italic(C)[paste("", .(x))])})
  
  p1 <- NULL
  p2 <- NULL
  
  for (i in seq_len(length(results))) {
    
    df <- data.frame(dates = results[[i]]$dates)
    df <- cbind(df, results[[i]]$I)
    df2 <- df[seq(results[[i]]$t_index, nrow(df)), ]
    
    if (is.null(p1)) {
      p1 <- ggplot(df, aes(dates, I_real)) +
        geom_point(aes_string(y = "I_real", colour = shQuote(paste0(i))), shape=1) + 
        geom_vline(xintercept = t_index, linetype = "dotted") +
        annotate("text", x = t_index + 1, y = 0, 
                 label = "italic(tau)", parse = TRUE) +
        geom_vline(xintercept = T_index, linetype = "dotted") +
        annotate("text", x = T_index + 1, y = 0, 
                 label = "italic(T)", parse = TRUE) +
        scale_x_date(date_labels = "%m-%d", 
                     limits = c(plot_start_date - 1, max(df$dates) + 1))
    }
    
    if (is.null(p2)) {
      p2 <- ggplot(df, aes(dates, cum_I_real)) +
        geom_point(aes_string(y = "cum_I_real", colour = shQuote(paste0(i))), shape=1) + 
        geom_vline(xintercept = t_index, linetype = "dotted") +
        annotate("text", x = t_index + 1, y = 0, 
                 label = "italic(tau)", parse = TRUE) +
        geom_vline(xintercept = T_index, linetype = "dotted") +
        annotate("text", x = T_index + 1, y = 0, 
                 label = "italic(T)", parse = TRUE) +
        scale_x_date(date_labels = "%m-%d", 
                     limits = c(plot_start_date - 1, max(df$dates) + 1))
    }
    
    p1 <- p1 + 
      geom_line(data = df2, aes_string(y = "I_mean", colour = shQuote(paste0(i)))) +
      geom_point(data = df2, aes_string(y = "I_mean", colour = shQuote(paste0(i)))) + 
      geom_ribbon(data = df2,
                  aes_string(ymin = "I_quantile_025",
                      ymax = "I_quantile_975",
                      fill = shQuote(paste0(i))))
    
    p2 <- p2 + 
      geom_line(data = df2, aes_string(y = "cum_I_mean", colour = shQuote(paste0(i)))) +
      geom_point(data = df2, aes_string(y = "cum_I_mean", colour = shQuote(paste0(i)))) + 
      geom_ribbon(data = df2,
                  aes_string(ymin = "cum_I_quantile_025",
                      ymax = "cum_I_quantile_975",
                      fill = shQuote(paste0(i))))
  }
  
  p1 <- p1 + 
    scale_colour_manual(name = "",
                        values = colors,
                        labels = p1_labels) +
    scale_fill_manual(name = "",
                      values = alpha(colors, 0.2),
                      labels = p1_labels) +
    xlab("") +
    ylab("Incidence") +
    theme_few() + 
    theme(
      legend.text = element_text(family = "serif"),
      legend.text.align = 0,
      legend.position = c(0.01, 0.99),
      legend.justification = c(0, 1),
      legend.background = element_blank()
    ) + 
    guides(fill = FALSE)
  
  
  p2 <- p2 + 
    scale_colour_manual(name = "",
                        values = colors,
                        labels = p2_labels) +
    scale_fill_manual(name = "",
                      values = alpha(colors, 0.2),
                      labels = p2_labels) +
    xlab("Time") +
    ylab("Cumulative Incidence") +
    theme_few() + 
    theme(
      legend.text = element_text(family = "serif"),
      legend.text.align = 0,
      legend.position = c(0.01, 0.99),
      legend.justification = c(0, 1),
      legend.background = element_blank()
    ) + 
    guides(fill = FALSE)
  
  gA <- ggplotGrob(p1)
  gB <- ggplotGrob(p2)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  out <- list(incid = gA, R = gB)
  out.grid <- arrangeGrob(grobs = out,
                          nrow = 2,
                          ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  
  prefix <- paste0(country, "_", scenario)
  ggsave(
    paste0(output_dir, prefix, "_incidence.png"),
    plot = out.grid,
    device = "png",
    dpi = 600,
    width = 6,
    height = 6
  )
  
  ggsave(
    paste0(output_dir, prefix, "_cum_incidence.png"),
    plot = p2,
    device = "png",
    dpi = 600,
    width = 6,
    height = 4.5
  )
  
  return(out.grid)
}



# 保存simulation结果
plot_and_save_simulation_3in1 <- function(dates,
                                     I,
                                     I_lb,
                                     I_ub,
                                     output_dir,
                                     prefix) {
  df <- data.frame(dates = dates)
  df <- cbind(df, I)
  
  names(I_lb) <- paste0(names(I_lb), "_lb")
  names(I_ub) <- paste0(names(I_ub), "_ub")
  
  df <- cbind(df, I_lb, I_ub)
  
  write.table(
    df,
    paste0(output_dir, prefix, "_result.csv"),
    sep = ",",
    row.names = FALSE
  )
  
  colors <- few_pal("Dark")(3)
  labels = c("Mean", "Lower bound", "Upper bound")
  
  p1 <- ggplot(df, aes(dates, I_mean)) +
    geom_line(aes(y = I_mean, colour = "Mean")) +
    geom_ribbon(aes(
      ymin = I_quantile_025,
      ymax = I_quantile_975,
      fill = "Mean"
    )) +
    geom_line(aes(y = I_mean_lb, colour = "LowerBound")) +
    geom_ribbon(aes(
      ymin = I_quantile_025_lb,
      ymax = I_quantile_975_lb,
      fill = "LowerBound"
    )) +
    geom_line(aes(y = I_mean_ub, colour = "UpperBound")) +
    geom_ribbon(aes(
      ymin = I_quantile_025_ub,
      ymax = I_quantile_975_ub,
      fill = "UpperBound"
    )) +
    scale_colour_manual(name = NULL,
                        values = colors,
                        labels = labels) +
    scale_fill_manual(name = NULL,
                      values = alpha(colors, 0.2),
                      labels = labels) +
    xlab("Time") +
    ylab("Incidence") +
    scale_x_date(date_labels = "%m-%d", limits = c(min(df$dates), max(df$dates) + 1)) +
    theme_few() + 
    theme(legend.text = element_text(family = "serif"),
          legend.text.align = 0,
          legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1),
          legend.background = element_blank())
  
  p2 <- ggplot(df, aes(dates, cum_I_mean)) +
    geom_line(aes(y = cum_I_mean, colour = "Mean")) +
    geom_ribbon(aes(
      ymin = cum_I_quantile_025,
      ymax = cum_I_quantile_975,
      fill = "Mean"
    )) +
    geom_line(aes(y = cum_I_mean_lb, colour = "LowerBound")) +
    geom_ribbon(aes(
      ymin = cum_I_quantile_025_lb,
      ymax = cum_I_quantile_975_lb,
      fill = "LowerBound"
    )) +
    geom_line(aes(y = cum_I_mean_ub, colour = "UpperBound")) +
    geom_ribbon(aes(
      ymin = cum_I_quantile_025_ub,
      ymax = cum_I_quantile_975_ub,
      fill = "UpperBound"
    )) +
    scale_colour_manual(name = NULL,
                        values = colors,
                        labels = labels) +
    scale_fill_manual(name = NULL,
                      values = alpha(colors, 0.2),
                      labels = labels) +
    xlab("Time") +
    ylab("Cumulative Incidence") +
    scale_x_date(date_labels = "%m-%d", limits = c(min(df$dates), max(df$dates) + 1)) +
    theme_few() + 
    theme(legend.text = element_text(family = "serif"),
          legend.text.align = 0,
          legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1),
          legend.background = element_blank())
  
  out <- list(p1 = p1, p2 = p2)
  out.grid <- arrangeGrob(grobs = out, nrow = 2, ncol = 1)
  grid.arrange(out.grid, newpage = FALSE)
  
  ggsave(
    paste0(output_dir, prefix, "_incidence.png"),
    plot = out.grid,
    device = "png",
    dpi = 600,
    width = 6,
    height = 6
  )
  
  return(out.grid)
}


# 保存estimation结果
plot_and_save_estimation_R <- function(result,
                                       output_dir,
                                       prefix, 
                                       options = NULL) {
  
  result$R$date_start <- result$dates[result$R$t_start]
  result$R$date_end <- result$dates[result$R$t_end]
  
  write.table(result$R,
              paste0(output_dir, prefix, "_R.csv"),
              sep = ",",
              row.names = FALSE)
  
  incid <- data.frame(
    dates = result$dates,
    imported = result$I_imported,
    local = result$I_local
  )
  write.table(incid,
              paste0(output_dir, prefix, "_incid.csv"),
              sep = ",",
              row.names = FALSE)
  
  grid <- plot_estimate_R(result, options = options)
  ggsave(
    paste0(output_dir, prefix, "_R.png"),
    plot = grid,
    device = "png",
    dpi = 600,
    width = 6,
    height = 6
  )
}

# 保存estimation结果
plot_and_save_estimation_Rs <- function(result,
                                       output_dir,
                                       prefix) {
  
  result$R_im$R$date_start <- result$dates[result$R_im$R$t_start]
  result$R_im$R$date_end <- result$dates[result$R_im$R$t_end]
  
  result$R_lc$R$date_start <- result$dates[result$R_lc$R$t_start]
  result$R_lc$R$date_end <- result$dates[result$R_lc$R$t_end]
  
  write.table(result$R_im$R,
              paste0(output_dir, prefix, "_Rim.csv"),
              sep = ",",
              row.names = FALSE)
  
  write.table(result$R_lc$R,
              paste0(output_dir, prefix, "_Rlc.csv"),
              sep = ",",
              row.names = FALSE)

  grid <- plot_estimate_Rs(result)
  ggsave(
    paste0(output_dir, prefix, "_Rs.png"),
    plot = grid,
    device = "png",
    dpi = 600,
    width = 6,
    height = 6
  )
}


# 用于国外的再生数估计
estimate_R_simple_model <- function (country, 
                                     window = 6, 
                                     start = 2, 
                                     base_date = NULL,
                                     plot_options = NULL) {
  
  output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  output_dir = paste0(output_dir, window, "window/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
 
  data <- read_excel(paste0(DATA_DIR, "/", country, ".xlsx"))
  
  incid <- data.frame(
    dates = as.Date(data$date, "%Y%m%d"),
    imported = as.numeric(data$fromChina) + as.numeric(data$fromothers),
    local = as.numeric(data$local) + as.numeric(data$unknown)
  )
  
  if (!is.null(base_date) && (base_date != "none")) {
    incid <- incid[incid$dates <= base_date, ]
  }
  
  t_len <- nrow(incid)
  t_start <- seq(start, t_len - window)
  t_end <- t_start + window
  
  result <- estimate_R(incid,
                       method = "parametric_si",
                       config = make_config(
                         list(
                           t_start = t_start,
                           t_end = t_end,
                           mean_si = COVID19_SI_MEAN,
                           std_si = COVID19_SI_STD
                         )
                       ))
  
  plot_and_save_estimation_R(result, output_dir, country,
                             options = plot_options)
  
  return(result)
}

# null模型-R拟合预测
predict_R_null <- function (country, 
                            label,
                            R, 
                            predict_length = 10, 
                            fit_model = 1,
                            init_params = list(a=0.1,
                                               b=5,
                                               c=0.5)) {
  
  output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  R_len <- length(R)
  
  data <- data.frame(x = seq(1, R_len), y = R)
  
  if (fit_model == 1) {
    f <- function (x, a, b) {
      b * (1 + exp(a * x)) ^ (-1)
    }
    
    fit <- nls(
      y ~ f(x, a, b),
      data = data,
      start = list(a = init_params$a, b = init_params$b),
      control = nls.control(maxiter = 1000)
    )
  } else {
    f <- function (x, a, b, c) {
      b * (c + exp(a * x)) ^ (-1)
    }
    
    fit <- nls(
      y ~ f(x, a, b, c),
      data = data,
      start = init_params,
      control = nls.control(maxiter = 1000)
    )
  }
  
  x_p <- x_p <- data.frame(x=seq(R_len + 1, R_len + predict_length))
  y_p <- predict(fit, x_p)
  y_hat <- append(fitted(fit), y_p)
  
  png(paste0(output_dir, country, "_fit_", label, '_R.png'))
  plot(seq(1, R_len + predict_length), y_hat, pch = 1, ylim=c(0,5))
  points(seq(1, R_len), R, pch=3)
  dev.off()
  
  return(list(fit = fit, value = y_p))
}

# null模型-模拟
predict_country_null <- function (country,
                                  estimated_result,
                                  last_ndays = 0,
                                  next_ndays = 30,
                                  method = c("expandtail", "fittail", "replay"),
                                  fit_config = list(fit_length = 10,
                                                    model = 1,
                                                    params = list(a=0.1,
                                                                  b=3,
                                                                  c=0.5))
) {
  
  method = match.arg(method)
  
  output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  I_past <- data.frame(imported = estimated_result$I_imported, 
                       local = estimated_result$I_local)
  I_past_len <- nrow(I_past)
  
  R_past <- estimated_result$R$`Mean(R)`
  R_past_len <- length(R_past)
  
  if (method == "replay") {
    R <- R_past[seq(R_past_len - last_ndays + 1, R_past_len)]
    if (next_ndays > 0) {
      R <- append(R, rep(R_past[R_past_len], next_ndays))
    }
  } else if (method == "expandtail") {
    R <- rep(R_past[R_past_len - last_ndays], last_ndays + next_ndays)
  } else if (method == "fittail") {
    fit_start <- max(1, R_past_len - last_ndays - fit_config$fit_length + 1)
    predicted_R <- predict_R_null(country, 
                        paste0(last_ndays, "-", fit_config$fit_length),
                        R_past[seq(fit_start, R_past_len - last_ndays)], 
                        last_ndays + next_ndays,
                        fit_model = fit_config$model,
                        init_params = fit_config$params)
    R <- predicted_R$value
  } else {
    stop("method must be either'expandtail' or 'fittail'")
  }
  
  dates <- seq.Date(estimated_result$dates[1], by = 1, 
                    length.out = I_past_len + next_ndays)
  
  if (last_ndays > 0) {
    I_imported <- I_past$imported[seq(I_past_len - last_ndays + 1, I_past_len)]
    if (next_ndays > 0) {
      I_imported <- append(I_imported, rep(0, next_ndays))
    }
  } else {
    I_imported <- rep(0, next_ndays)
  }
  
  sim_data = simulate_I_rep(
    R = R,
    I_imported = I_imported,
    mean_si = COVID19_SI_MEAN,
    std_si = COVID19_SI_STD,
    num_repeats = 100,
    I_past = I_past[seq(1, I_past_len - last_ndays), ]
  )
  
  I_real <- vector()
  I_real[seq(1, I_past_len)] <- estimated_result$I
  if (next_ndays > 0) {
    I_real[seq(I_past_len + 1, I_past_len + next_ndays)] <- NA
  }
  sim_data$I$I_real <- I_real
  sim_data$I$cum_I_real <- cumsum(I_real)
  sim_data$dates <- dates
  sim_data$t_index <- I_past_len - last_ndays + 1
  sim_data$T_index <- I_past_len
  
  time_range <- paste0(last_ndays, "t", next_ndays)
  prefix <- paste0(country, "_null_", method, "_", time_range)
  plot_and_save_simulation(dates = dates,
                           I = sim_data$I,
                           t_index = sim_data$t_index,
                           output_dir = output_dir,
                           prefix = prefix)
  
  return(sim_data)
}


# scenario模型-模拟
predict_country_scenario <- function (country,
                                  estimated_result,
                                  last_ndays = 0,
                                  next_ndays = 30,
                                  scenario, 
                                  R_sce,
                                  R_sce_d
) {
  
  output_dir = paste0(RESULT_ROOT_DIR, "/", country, "/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  I_past <- data.frame(imported = estimated_result$I_imported, 
                       local = estimated_result$I_local)
  I_past_len <- nrow(I_past)
  
  R_past <- estimated_result$R$`Mean(R)`
  R_past_len <- length(R_past)
  
  R_sce_len <- length(R_sce)
  
  R_sce_start <- 0
  for (i in seq(R_sce_len, 1)) {
    if (R_past[R_past_len - last_ndays] > R_sce[i]) {
      R_sce_start <- i
    } else {
      break
    }
  }
  
  if (R_sce_start == 0) {
    R <- rep(R_past[R_past_len - last_ndays], last_ndays + next_ndays)
  } else {
    if (R_sce_start + last_ndays + next_ndays - 1 <= R_sce_len ) {
      R <- R_sce_d[seq(R_sce_start, R_sce_start + last_ndays + next_ndays - 1)]
    } else {
      R <- R_sce_d[seq(R_sce_start, R_sce_len)]
      R <- append(R, rep(R_sce_d[R_sce_len], 
                         last_ndays + next_ndays - (R_sce_len - R_sce_start + 1)))
    }
  }
  
  dates <- seq.Date(estimated_result$dates[1], by = 1, 
                    length.out = I_past_len + next_ndays)

  if (last_ndays > 0) {
    I_imported <- I_past$imported[seq(I_past_len - last_ndays + 1, I_past_len)]
    if (next_ndays > 0) {
      I_imported <- append(I_imported, rep(0, next_ndays))
    }
  } else {
    I_imported <- rep(0, next_ndays)
  }
  
  sim_data = simulate_I_rep(
    R = R,
    I_imported = I_imported,
    mean_si = COVID19_SI_MEAN,
    std_si = COVID19_SI_STD,
    num_repeats = 100,
    I_past = I_past[seq(1, I_past_len - last_ndays), ]
  )
  
  I_real <- vector()
  I_real[seq(1, I_past_len)] <- estimated_result$I
  if (next_ndays > 0) {
    I_real[seq(I_past_len + 1, I_past_len + next_ndays)] <- NA
  }
  sim_data$I$I_real <- I_real
  sim_data$I$cum_I_real <- cumsum(I_real)
  sim_data$dates <- dates
  sim_data$t_index <- I_past_len - last_ndays + 1
  sim_data$T_index <- I_past_len
  sim_data$R_sce_start <- R_sce_start
  
  time_range <- paste0(last_ndays, "t", next_ndays)
  prefix <- paste0(country, "_", scenario, "_", time_range)
  plot_and_save_simulation(dates = dates,
                           I = sim_data$I,
                           t_index = sim_data$t_index,
                           output_dir = output_dir,
                           prefix = prefix)
  
  return(sim_data)
}


get_typical_R <- function (country) {
  if (country == "China") {
    R_data <- read.table("results/ChinaModel/6window/China_R.csv", header = TRUE, sep = ",")
    R_d_data <- read.table("results/ChinaModel/0window/China_R.csv", header = TRUE, sep = ",")
  } else if (country == "Singapore") {
    
    R_data <- read.table("results/Singapore/6window/Singapore_R.csv", header = TRUE, sep = ",")
    R_d_data <- read.table("results/Singapore/0window/Singapore_R.csv", header = TRUE, sep = ",")
    R_data <- R_data[as.Date(R_data$date_end) <= "2020-03-21", ]
    R_d_data <- R_d_data[as.Date(R_d_data$date_end) <= "2020-03-21", ]
  } else if (country == "Korea") {
    R_data <- read.table("results/Korea/6window/Korea_R.csv", header = TRUE, sep = ",")
    R_d_data <- read.table("results/Korea/0window/Korea_R.csv", header = TRUE, sep = ",")
  } else {
    stop("unknown typical country")
  }

  R <- R_data$`Mean.R.`
  R_d <- R_d_data$`Mean.R.`[R_d_data$t_end >= R_data$t_end[1]]
  
  return(list(R = R, R_d = R_d))
}
