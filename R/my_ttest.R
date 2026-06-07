#' Test Whether Mean Height is the Same for Males and Females
#'
#' Performs a two-sample independent t-test (equal variances assumed) to
#' compare the mean heights of male and female participants.  Produces a
#' boxplot and per-group normal Q-Q plots as assumption checks.
#'
#' @param data A data frame containing at least the columns `height` (numeric,
#'   cm) and `gender` (character or factor with levels `"Male"` and
#'   `"Female"`).
#' @param alpha Significance level.  Default `0.05`.
#' @param plot  Logical.  If `TRUE` (default) assumption plots are printed.
#'
#' @return An object of class `"ttest_result"` (a named list) containing:
#' * `hypotheses` — character vector stating H0 and H1
#' * `group_summary` — data frame of per-group means and SDs
#' * `test` — the `htest` object returned by `t.test()`
#' * `test_statistic` — numeric t-statistic
#' * `df` — degrees of freedom
#' * `p_value` — numeric p-value
#' * `conf_int` — 95% confidence interval for the difference in means
#' * `alpha` — significance level used
#' * `decision` — character: "Reject H0" or "Fail to reject H0"
#' * `conclusion` — plain-English conclusion
#' * `plots` — named list of ggplot objects (boxplot, qq)
#'
#' @examples
#' data(project2026_data)
#' result <- my_ttest(project2026_data)
#' print(result)
#'
#' @importFrom stats t.test
#' @importFrom ggplot2 ggplot aes geom_boxplot stat_summary stat_qq
#'   stat_qq_line facet_wrap labs scale_fill_manual theme_bw theme
#' @importFrom rlang .data
#' @export
my_ttest <- function(data, alpha = 0.05, plot = TRUE) {

  # -- Input checks ------------------------------------------------------------
  stopifnot(is.data.frame(data))
  stopifnot(all(c("height", "gender") %in% names(data)))
  stopifnot(is.numeric(alpha), length(alpha) == 1L, alpha > 0, alpha < 1)

  data$gender <- factor(data$gender, levels = c("Male", "Female"))

  # -- 1. Hypotheses -----------------------------------------------------------
  hyp <- c(
    H0 = "H0: mu_Male = mu_Female  (mean heights are equal)",
    H1 = "H1: mu_Male != mu_Female (mean heights differ)"
  )

  # -- 2. Numerical summary ----------------------------------------------------
  grp_sum <- do.call(rbind, lapply(levels(data$gender), function(g) {
    x <- data$height[data$gender == g]
    data.frame(
      gender = g,
      n      = length(x),
      mean   = round(mean(x), 3),
      sd     = round(stats::sd(x), 3),
      stringsAsFactors = FALSE
    )
  }))

  # -- 2. Graphical summaries --------------------------------------------------
  pal <- c("Male" = "#2166AC", "Female" = "#D6604D")

  p_box <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data$gender, y = .data$height,
                                        fill = .data$gender)) +
    ggplot2::geom_boxplot(alpha = 0.6, width = 0.4,
                          outlier.shape = 21, outlier.size = 1.2) +
    ggplot2::stat_summary(fun = mean, geom = "point",
                          shape = 23, size = 3, fill = "white") +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::labs(
      title    = "Assumption check: Distribution of height by gender",
      subtitle = "Diamond = group mean",
      x = "Gender", y = "Height (cm)"
    ) +
    ggplot2::theme_bw(base_size = 11)

  p_qq <- ggplot2::ggplot(data,
                          ggplot2::aes(sample = .data$height)) +
    ggplot2::stat_qq(alpha = 0.4, size = 0.8) +
    ggplot2::stat_qq_line(colour = "red") +
    ggplot2::facet_wrap(~ .data$gender) +
    ggplot2::labs(
      title    = "Assumption check: Normality of height by gender",
      subtitle = "Normal Q-Q plots",
      x = "Theoretical quantiles", y = "Sample quantiles"
    ) +
    ggplot2::theme_bw(base_size = 11)

  if (plot) {
    print(p_box)
    print(p_qq)
  }

  # -- 3. Fit the test ---------------------------------------------------------
  tt      <- stats::t.test(height ~ gender, data = data, var.equal = TRUE)
  t_stat  <- tt$statistic
  df_val  <- tt$parameter
  p_val   <- tt$p.value
  ci      <- tt$conf.int

  # -- 4. Decision -------------------------------------------------------------
  dec <- decision(p_val, alpha)

  # -- 5. Conclusion -----------------------------------------------------------
  mean_m   <- grp_sum$mean[grp_sum$gender == "Male"]
  mean_f   <- grp_sum$mean[grp_sum$gender == "Female"]
  diff_val <- round(mean_m - mean_f, 3)
  p_fmt    <- format_pvalue(p_val)

  conc <- if (p_val < alpha) {
    paste0(
      "There is statistically significant evidence that mean heights differ ",
      "between males and females (t(", round(df_val, 1), ") = ",
      round(t_stat, 3), ", p ", p_fmt, "). ",
      "Males are on average ", abs(diff_val), " cm ",
      ifelse(diff_val > 0, "taller", "shorter"),
      " than females (95% CI: [",
      round(ci[1], 3), ", ", round(ci[2], 3), "] cm)."
    )
  } else {
    paste0(
      "There is insufficient evidence to conclude that mean heights differ ",
      "between males and females (t(", round(df_val, 1), ") = ",
      round(t_stat, 3), ", p = ", p_fmt, ")."
    )
  }

  # -- Return ------------------------------------------------------------------
  result <- list(
    hypotheses     = hyp,
    group_summary  = grp_sum,
    test           = tt,
    test_statistic = t_stat,
    df             = df_val,
    p_value        = p_val,
    conf_int       = ci,
    alpha          = alpha,
    decision       = dec,
    conclusion     = conc,
    plots          = list(boxplot = p_box, qq = p_qq)
  )
  class(result) <- "ttest_result"
  result
}


#' Print method for ttest_result objects
#'
#' Displays a formatted summary of the two-sample t-test results.
#'
#' @param x   An object of class `"ttest_result"`.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return `x`, invisibly.
#' @export
print.ttest_result <- function(x, ...) {
  cat("===========================================================\n")
  cat(" Research Question 2: Mean Height - Male vs. Female\n")
  cat("===========================================================\n\n")

  cat("-- Hypotheses ----------------------------------------------\n")
  cat(" ", x$hypotheses["H0"], "\n")
  cat(" ", x$hypotheses["H1"], "\n\n")

  cat("-- Assumption Checks ---------------------------------------\n")
  cat("  See boxplot (distribution) and Q-Q plots (normality).\n")
  cat("  Equal variances assumed as stated in the question.\n\n")

  cat("-- Group Summary -------------------------------------------\n")
  print(x$group_summary, row.names = FALSE)
  cat("\n")

  diff_val <- x$group_summary$mean[x$group_summary$gender == "Male"] -
    x$group_summary$mean[x$group_summary$gender == "Female"]
  cat(sprintf("  Difference (Male - Female): %.3f cm\n\n", diff_val))

  cat(sprintf("  t-statistic : %.4f\n", x$test_statistic))
  cat(sprintf("  df          : %.1f\n", x$df))
  cat(sprintf("  p-value     : %s  %s\n",
              format_pvalue(x$p_value), sig_stars(x$p_value)))
  cat(sprintf("  95%% CI      : [%.3f, %.3f] cm\n\n",
              x$conf_int[1], x$conf_int[2]))

  cat(sprintf("-- Decision (alpha = %.2f) ---------------------------\n",
              x$alpha))
  cat(" ", x$decision, "\n\n")

  cat("-- Conclusion ----------------------------------------------\n")
  cat(strwrap(x$conclusion, width = 58, prefix = "  "), sep = "\n")
  cat("\n===========================================================\n")

  invisible(x)
}
