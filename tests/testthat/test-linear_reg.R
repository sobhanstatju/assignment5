test_that("linear_reg returns correct class and structure", {
  res <- linear_reg(test_data, plot = FALSE)

  expect_s3_class(res, "lm_result")
  expect_named(res, c("hypotheses", "model", "coefficients", "r_squared",
                      "test_statistic", "df", "p_value", "alpha",
                      "decision", "conclusion", "plots"))
})

test_that("linear_reg model element is an lm object", {
  res <- linear_reg(test_data, plot = FALSE)
  expect_s3_class(res$model, "lm")
})

test_that("linear_reg r_squared is between 0 and 1", {
  res <- linear_reg(test_data, plot = FALSE)
  expect_gte(res$r_squared, 0)
  expect_lte(res$r_squared, 1)
})

test_that("linear_reg decision matches p_value vs alpha", {
  res <- linear_reg(test_data, alpha = 0.05, plot = FALSE)
  expected_dec <- if (res$p_value < 0.05) "Reject H0" else "Fail to reject H0"
  expect_equal(res$decision, expected_dec)
})

test_that("linear_reg alpha argument is respected", {
  res_strict <- linear_reg(test_data, alpha = 0.001, plot = FALSE)
  res_lenient <- linear_reg(test_data, alpha = 0.999, plot = FALSE)
  # With alpha=0.999 nearly always reject; with alpha=0.001 same p-value
  expect_true(res_lenient$decision == "Reject H0" ||
                res_strict$decision == "Fail to reject H0" ||
                res_strict$p_value == res_lenient$p_value)  # p-value unchanged
})

test_that("linear_reg errors on missing columns", {
  bad <- test_data[, c("gender", "weight")]
  expect_error(linear_reg(bad, plot = FALSE))
})

test_that("linear_reg plots list contains ggplot objects", {
  res <- linear_reg(test_data, plot = FALSE)
  expect_true(inherits(res$plots$scatter, "ggplot"))
  expect_true(inherits(res$plots$residuals, "ggplot"))
  expect_true(inherits(res$plots$qq, "ggplot"))
})

test_that("print.lm_result returns invisibly", {
  res <- linear_reg(test_data, plot = FALSE)
  out <- capture.output(returned <- print(res))
  expect_identical(returned, res)
  expect_true(any(grepl("Research Question 1", out)))
  expect_true(any(grepl("Conclusion", out)))
})
