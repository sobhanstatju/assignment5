#' Test for a Linear Relationship Between Height and Weight
#'
#' Fits a simple linear regression of `weight` on `height` and tests whether
#' the slope is zero.  Produces numerical and graphical assumption checks
#' (scatter plot with regression line, residuals-vs-fitted plot, and a normal
#' Q-Q plot of residuals).
#'
#' @param data A data frame containing at least the columns `height` (numeric,
#'   cm) and `weight` (numeric, kg).
#' @param alpha Significance level for the hypothesis test.  Default `0.05`.
#' @param plot  Logical.  If `TRUE` (default) diagnostic plots are printed.
#'
#' @return An object of class `"lm_result"` (a named list) containing:
#' * `hypotheses` — character vector stating H0 and H1
#' * `model` — the fitted `lm` object
#' * `coefficients` — data frame of coefficient estimates
#' * `r_squared` — numeric R-squared value
#' * `test_statistic` — numeric t-statistic for the slope
#' * `df` — degrees of freedom
#' * `p_value` — numeric p-value for the slope test
#' * `alpha` — significance level used
#' * `decision` — character: "Reject H0" or "Fail to reject H0"
#' * `conclusion` — plain-English conclusion
#' * `plots` — named list of ggplot objects (scatter, residuals, qq)
#'
#' @examples
#' data(project2026_data)
#' result <- linear_reg(project2026_data)
#' print(result)
#'
#' @importFrom stats lm residuals fitted
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline
#'   stat_qq stat_qq_line labs theme_bw theme
#' @importFrom rlang .data
#' @export
linear_reg <- function(data, alpha = 0.05, plot = TRUE) {

  # -- Input checks ------------------------------------------------------------
  stopifnot(is.data.frame(data))
  stopifnot(all(c("height", "weight") %in% names(data)))
  stopifnot(is.numeric(alpha), length(alpha) == 1L, alpha > 0, alpha < 1)

  # -- 1. Hypotheses -----------------------------------------------------------
  hyp <- c(
    H0 = "H0: beta_1 = 0  (no linear relationship between height and weight)",
    H1 = "H1: beta_1 != 0 (a linear relationship exists)"
  )

  # -- 2. Assumption checks — graphical summaries ------------------------------
  p_scatter <- ggplot2::ggplot(data,
                               ggplot2::aes(x = .data$height, y = .data$weight)) +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, colour = "#2166AC") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                         colour = "black", linewidth = 0.9) +
    ggplot2::labs(
      title    = "Assumption check: Linearity",
      subtitle = "Scatter plot of weight vs. height with regression line",
      x = "Height (cm)", y = "Weight (kg)"
    ) +
    ggplot2::theme_bw(base_size = 11)

  # -- 3. Fit the model --------------------------------------------------------
  fit      <- stats::lm(weight ~ height, data = data)
  fit_sum  <- summary(fit)
  coef_df  <- as.data.frame(fit_sum$coefficients)
  names(coef_df) <- c("Estimate", "Std.Error", "t.value", "p.value")
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  coef_df  <- coef_df[, c("term", "Estimate", "Std.Error", "t.value", "p.value")]

  slope_row <- coef_df[coef_df$term == "height", ]
  t_stat    <- slope_row$t.value
  p_val     <- slope_row$p.value
  r2        <- fit_sum$r.squared
  df_resid  <- fit$df.residual

  # -- Diagnostic plots --------------------------------------------------------
  diag_df <- data.frame(
    fitted    = stats::fitted(fit),
    residuals = stats::residuals(fit)
  )

  p_resid <- ggplot2::ggplot(diag_df,
                             ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, colour = "#4DAC26") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    ggplot2::labs(
      title    = "Assumption check: Homoscedasticity",
      subtitle = "Residuals vs. Fitted values",
      x = "Fitted values", y = "Residuals"
    ) +
    ggplot2::theme_bw(base_size = 11)

  p_qq <- ggplot2::ggplot(diag_df,
                          ggplot2::aes(sample = .data$residuals)) +
    ggplot2::stat_qq(alpha = 0.4, size = 1) +
    ggplot2::stat_qq_line(colour = "red") +
    ggplot2::labs(
      title    = "Assumption check: Normality of residuals",
      subtitle = "Normal Q-Q plot",
      x = "Theoretical quantiles", y = "Sample quantiles"
    ) +
    ggplot2::theme_bw(base_size = 11)

  if (plot) {
    print(p_scatter)
    print(p_resid)
    print(p_qq)
  }

  # -- 4. Decision -------------------------------------------------------------
  dec <- decision(p_val, alpha)

  # -- 5. Conclusion -----------------------------------------------------------
  slope_est <- round(slope_row$Estimate, 3)
  p_fmt     <- format_pvalue(p_val)

  conc <- if (p_val < alpha) {
    paste0(
      "There is statistically significant evidence of a linear relationship ",
      "between height and weight (t(", df_resid, ") = ",
      round(t_stat, 3), ", p ", p_fmt, "). ",
      "Each additional centimetre of height is associated with an estimated ",
      slope_est, " kg increase in weight (R\u00b2 = ",
      round(r2, 4), ")."
    )
  } else {
    paste0(
      "There is insufficient evidence to conclude that a linear relationship ",
      "exists between height and weight (t(", df_resid, ") = ",
      round(t_stat, 3), ", p = ", p_fmt, ")."
    )
  }

  # -- Return ------------------------------------------------------------------
  result <- list(
    hypotheses     = hyp,
    model          = fit,
    coefficients   = coef_df,
    r_squared      = r2,
    test_statistic = t_stat,
    df             = df_resid,
    p_value        = p_val,
    alpha          = alpha,
    decision       = dec,
    conclusion     = conc,
    plots          = list(scatter = p_scatter,
                          residuals = p_resid,
                          qq = p_qq)
  )
  class(result) <- "lm_result"
  result
}


#' Print method for lm_result objects
#'
#' Displays a formatted summary of the linear regression test results.
#'
#' @param x   An object of class `"lm_result"`.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return `x`, invisibly.
#' @export
print.lm_result <- function(x, ...) {
  cat("===========================================================\n")
  cat(" Research Question 1: Linear Relationship - Height vs. Weight\n")
  cat("===========================================================\n\n")

  cat("-- Hypotheses ----------------------------------------------\n")
  cat(" ", x$hypotheses["H0"], "\n")
  cat(" ", x$hypotheses["H1"], "\n\n")

  cat("-- Assumption Checks ---------------------------------------\n")
  cat("  See scatter plot (linearity), residuals vs. fitted\n")
  cat("  (homoscedasticity), and Q-Q plot (normality).\n\n")

  cat("-- Coefficient Table ---------------------------------------\n")
  cf <- x$coefficients
  cf$Estimate  <- round(cf$Estimate,  4)
  cf$Std.Error <- round(cf$Std.Error, 4)
  cf$t.value   <- round(cf$t.value,   4)
  cf$p.value   <- sapply(cf$p.value, format_pvalue)
  cf$sig       <- sapply(x$coefficients$p.value, sig_stars)
  print(cf, row.names = FALSE)
  cat("\n")

  cat(sprintf("  R-squared : %.4f\n", x$r_squared))
  cat(sprintf("  t-stat    : %.4f  (df = %d)\n",
              x$test_statistic, x$df))
  cat(sprintf("  p-value   : %s  %s\n\n",
              format_pvalue(x$p_value), sig_stars(x$p_value)))

  cat(sprintf("-- Decision (alpha = %.2f) ---------------------------\n",
              x$alpha))
  cat(" ", x$decision, "\n\n")

  cat("-- Conclusion ----------------------------------------------\n")
  cat(strwrap(x$conclusion, width = 58, prefix = "  "), sep = "\n")
  cat("\n===========================================================\n")

  invisible(x)
}
