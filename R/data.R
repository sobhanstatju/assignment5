#' Height, Weight, and Physical Activity Dataset (project2026)
#'
#' A cross-sectional dataset of 1,000 adults aged 26–45 containing
#' anthropometric measurements and self-reported physical activity levels.
#'
#' @format A data frame with 1,000 rows and 5 variables:
#' \describe{
#'   \item{ID}{Character. Participant identifier (e.g. `"ID1"`).}
#'   \item{gender}{Character. Participant gender: `"Male"` or `"Female"`.}
#'   \item{height}{Numeric. Height in centimetres (cm).}
#'   \item{weight}{Numeric. Body weight in kilograms (kg).}
#'   \item{phys}{Character. Self-reported physical activity level:
#'     `"None"`, `"Moderate"`, or `"Intense"`.  In the raw CSV, `NA` values
#'     encode the `"None"` category.}
#' }
#'
#' @source Simulated dataset provided for STAT1379/COMP6179 (2026).
#'
#' @examples
#' data(project2026_data)
#' head(project2026_data)
#' summary(project2026_data)
"project2026_data"
