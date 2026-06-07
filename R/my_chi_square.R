#' Test for an Association Between Gender and Physical Activity
#'
#' Performs Pearson's chi-square test of independence on a contingency table
#' of gender by physical activity level.  Missing values in the activity
#' column are recoded as `"None"` before testing (consistent with the dataset
#' documentation).  Produces a stacked bar chart and the expected-counts table
#' as assumption checks.
#'
#' @param data A data frame containing at least the columns `gender`
#'   (character or factor) and `phys` (character or factor with levels
#'   `"None"`, `"Moderate"`, `"Intense"`; `NA` is treated as `"None"`).
#' @param alpha Significance level.  Default `0.05`.
#' @param plot  Logical.  If `TRUE` (default) the bar chart is printed.
#'
#' @return An object of class `"chisq_result"` (a named list) with elements:
#' \describe{
#'   \item{hypotheses}{Character vector stating H0 and H1.}
#'   \item{observed}{Contingency table of observed counts.}
#'   \item{expected}{Matrix of expected counts under H0.}
#'   \item{min_expected}{Minimum expected cell count (assumption check).}
#'   \item{test}{The `htest` object returned by `chisq.test()`.}
#'   \item{test_statistic}{Numeric chi-square statistic.}
#'   \item{df}{Degrees of freedom.}
#'   \item{p_value}{Numeric p-value.}
#'   \item{alpha}{Significance level used.}
#'   \item{decision}{Character: "Reject H0" or "Fail to reject H0".}
#'   \item{conclusion}{Plain-English conclusion.}
#'   \item{plots}{Named list of `ggplot` objects (bar).}
#' }
#'
#' @examples
#' data(project2026_data)
#' result <- my_chi_square(project2026_data)
#' print(result)
#'
#' @importFrom stats chisq.test
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous
#'   scale_fill_brewer labs theme_bw theme element_text
#' @importFrom rlang .data
#' @export
my_chi_square <- function(data, alpha = 0.05, plot = TRUE) {

  # ── Input checks ────────────────────────────────────────────────────────────
  stopifnot(is.data.frame(data))
  stopifnot(all(c("gender", "phys") %in% names(data)))
  stopifnot(is.numeric(alpha), length(alpha) == 1L, alpha > 0, alpha < 1)

  # Recode NA -> "None" (documented missing-level meaning)
  data$phys   <- ifelse(is.na(data$phys), "None", as.character(data$phys))
  data$phys   <- factor(data$phys,   levels = c("None", "Moderate", "Intense"))
  data$gender <- factor(data$gender, levels = c("Male", "Female"))

  # ── 1. Hypotheses ────────────────────────────────────────────────────────────
  hyp <- c(
    H0 = paste0("H0: Gender and physical activity are independent",
                " (p_ij = p_i. * p_.j for all i,j)"),
    H1 = "H1: Gender and physical activity are NOT independent"
  )

  # ── 2. Observed contingency table ───────────────────────────────────────────
  obs_tbl <- table(Gender = data$gender,
                   `Physical Activity` = data$phys)

  # ── 2. Graphical summary ────────────────────────────────────────────────────
  plot_df <- as.data.frame(obs_tbl)
  names(plot_df) <- c("gender", "phys", "n")
  plot_df <- do.call(rbind, lapply(levels(data$gender), function(g) {
    sub_df <- plot_df[plot_df$gender == g, ]
    sub_df$prop <- sub_df$n / sum(sub_df$n)
    sub_df
  }))

  p_bar <- ggplot2::ggplot(plot_df,
                           ggplot2::aes(x = .data$gender, y = .data$prop,
                                        fill = .data$phys)) +
    ggplot2::geom_col(position = "fill", width = 0.5) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title    = "Assumption check: Physical activity distribution by gender",
      subtitle = "Stacked proportional bar chart",
      x = "Gender", y = "Proportion",
      fill = "Physical Activity"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  if (plot) print(p_bar)

  # ── 3. Chi-square test ───────────────────────────────────────────────────────
  chi_res   <- stats::chisq.test(obs_tbl)
  exp_mat   <- chi_res$expected
  min_exp   <- min(exp_mat)
  chi_stat  <- chi_res$statistic
  chi_df    <- chi_res$parameter
  p_val     <- chi_res$p.value

  # ── 4. Decision ───────────────────────────────────────────────────────────
  dec <- decision(p_val, alpha)

  # ── 5. Conclusion ─────────────────────────────────────────────────────────
  p_fmt <- format_pvalue(p_val)

  assump_note <- if (min_exp >= 5) {
    paste0("All expected counts >= 5 (minimum = ",
           round(min_exp, 2), "), so the chi-square approximation is valid.")
  } else {
    paste0("WARNING: some expected counts < 5 (minimum = ",
           round(min_exp, 2), "). Consider Fisher's exact test.")
  }

  conc <- if (p_val < alpha) {
    paste0(
      "There is statistically significant evidence of an association ",
      "between gender and physical activity level ",
      "(chi^2(", chi_df, ") = ", round(chi_stat, 3),
      ", p ", p_fmt, "). ",
      "The distribution of physical activity categories differs between ",
      "males and females."
    )
  } else {
    paste0(
      "There is insufficient evidence to conclude that gender and physical ",
      "activity level are associated ",
      "(chi^2(", chi_df, ") = ", round(chi_stat, 3),
      ", p = ", p_fmt, ")."
    )
  }

  # ── Return ────────────────────────────────────────────────────────────────
  result <- list(
    hypotheses      = hyp,
    observed        = obs_tbl,
    expected        = exp_mat,
    min_expected    = min_exp,
    assumption_note = assump_note,
    test            = chi_res,
    test_statistic  = chi_stat,
    df              = chi_df,
    p_value         = p_val,
    alpha           = alpha,
    decision        = dec,
    conclusion      = conc,
    plots           = list(bar = p_bar)
  )
  class(result) <- "chisq_result"
  result
}


#' Print method for `chisq_result` objects
#'
#' Displays a formatted summary of the chi-square test of independence results.
#'
#' @param x   An object of class `"chisq_result"`.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return `x`, invisibly.
#' @export
print.chisq_result <- function(x, ...) {
  cat("══════════════════════════════════════════════════════════\n")
  cat(" Research Question 3: Gender vs. Physical Activity\n")
  cat("══════════════════════════════════════════════════════════\n\n")

  cat("── Hypotheses ──────────────────────────────────────────\n")
  cat(strwrap(x$hypotheses["H0"], width = 58, prefix = "  "), sep = "\n")
  cat(" ", x$hypotheses["H1"], "\n\n")

  cat("── Assumption Check ────────────────────────────────────\n")
  cat(strwrap(x$assumption_note, width = 58, prefix = "  "), sep = "\n")
  cat("\n")

  cat("── Observed Counts ─────────────────────────────────────\n")
  print(x$observed)
  cat("\n")

  cat("── Expected Counts (under H0) ──────────────────────────\n")
  print(round(x$expected, 2))
  cat("\n")

  cat(sprintf("  chi^2 statistic : %.4f\n", x$test_statistic))
  cat(sprintf("  df              : %d\n",   x$df))
  cat(sprintf("  p-value         : %s  %s\n",
              format_pvalue(x$p_value), sig_stars(x$p_value)))
  cat("\n")

  cat(sprintf("── Decision (alpha = %.2f) ───────────────────────────\n",
              x$alpha))
  cat(" ", x$decision, "\n\n")

  cat("── Conclusion ──────────────────────────────────────────\n")
  cat(strwrap(x$conclusion, width = 58, prefix = "  "), sep = "\n")
  cat("\n══════════════════════════════════════════════════════════\n")

  invisible(x)
}
