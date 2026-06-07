#' Height, Weight, and Physical Activity Dataset
#'
#' A cross-sectional dataset of 1,000 adults aged 26-45 containing
#' anthropometric measurements and self-reported physical activity levels.
#'
#' @format A data frame with 1,000 rows and 5 variables:
#' \describe{
#'   \item{ID}{Character. Participant identifier (e.g. \code{"ID1"}).}
#'   \item{gender}{Character. Participant gender: \code{"Male"} or \code{"Female"}.}
#'   \item{height}{Numeric. Height in centimetres (cm).}
#'   \item{weight}{Numeric. Body weight in kilograms (kg).}
#'   \item{phys}{Character. Physical activity level:
#'     \code{"None"}, \code{"Moderate"}, or \code{"Intense"}.}
#' }
#'
#' @source Simulated dataset provided for STAT1379/COMP6179 (2026).
#'
#' @examples
#' data(project2026_data)
#' head(project2026_data)
#' summary(project2026_data)
#'
#' @name project2026_data
#' @docType data
#' @keywords datasets
NULL
