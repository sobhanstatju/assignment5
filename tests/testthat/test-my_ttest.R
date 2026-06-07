test_that("test_mean_height returns correct class and structure", {
  res <- test_mean_height(test_data, plot = FALSE)

  expect_s3_class(res, "ttest_result")
  expect_named(res, c("hypotheses", "group_summary", "test",
                      "test_statistic", "df", "p_value", "conf_int",
                      "alpha", "decision", "conclusion", "plots"))
})

test_that("test_mean_height group_summary has both genders", {
  res <- test_mean_height(test_data, plot = FALSE)
  expect_setequal(res$group_summary$gender, c("Male", "Female"))
})

test_that("test_mean_height conf_int has length 2", {
  res <- test_mean_height(test_data, plot = FALSE)
  expect_length(res$conf_int, 2)
  expect_lt(res$conf_int[1], res$conf_int[2])
})

test_that("test_mean_height decision matches p_value vs alpha", {
  res <- test_mean_height(test_data, alpha = 0.05, plot = FALSE)
  expected_dec <- if (res$p_value < 0.05) "Reject H0" else "Fail to reject H0"
  expect_equal(res$decision, expected_dec)
})

test_that("test_mean_height errors on missing columns", {
  bad <- test_data[, c("height", "weight")]
  expect_error(test_mean_height(bad, plot = FALSE))
})

test_that("test_mean_height test element is an htest object", {
  res <- test_mean_height(test_data, plot = FALSE)
  expect_s3_class(res$test, "htest")
})

test_that("test_mean_height plots list contains ggplot objects", {
  res <- test_mean_height(test_data, plot = FALSE)
  expect_true(inherits(res$plots$boxplot, "ggplot"))
  expect_true(inherits(res$plots$qq, "ggplot"))
})

test_that("print.ttest_result returns invisibly", {
  res <- test_mean_height(test_data, plot = FALSE)
  out <- capture.output(returned <- print(res))
  expect_identical(returned, res)
  expect_true(any(grepl("Research Question 2", out)))
  expect_true(any(grepl("Group Summary", out)))
})
