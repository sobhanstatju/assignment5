#' project2026: Statistical Analysis of Height, Weight and Physical Activity
#'
#' @description
#' The `project2026` package provides three wrapper functions for answering
#' the research questions posed in the STAT1379/COMP6179 project:
#'
#' * [linear_reg()] — simple linear regression of weight on height.
#' * [my_ttest()] — two-sample *t*-test (equal variances) comparing
#'   mean heights of males and females.
#' * [my_chi_square()] — chi-square test of independence between gender
#'   and physical activity level.
#'
#' Each function returns a structured list with a custom [print()] method.
#' The package also ships the [project2026_data] dataset used in the analysis.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL
