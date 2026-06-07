# ── Internal helpers ──────────────────────────────────────────────────────────
# These functions are NOT exported.  Tag each with @noRd to suppress .Rd files.

#' Format a p-value for display
#'
#' @param p Numeric p-value.
#' @param digits Number of decimal places when p >= 0.001.
#' @return A character string such as "< 0.001" or "0.0234".
#' @noRd
format_pvalue <- function(p, digits = 4) {
  if (p < 0.001) "< 0.001" else formatC(p, format = "f", digits = digits)
}

#' Decide reject / fail-to-reject at a given significance level
#'
#' @param p    Numeric p-value.
#' @param alpha Significance level (default 0.05).
#' @return A character string: "Reject H0" or "Fail to reject H0".
#' @noRd
decision <- function(p, alpha = 0.05) {
  if (p < alpha) "Reject H0" else "Fail to reject H0"
}

#' Significance stars
#'
#' @param p Numeric p-value.
#' @return A character string of stars.
#' @noRd
sig_stars <- function(p) {
  if      (p < 0.001) "***"
  else if (p < 0.01)  "**"
  else if (p < 0.05)  "*"
  else                "(ns)"
}
