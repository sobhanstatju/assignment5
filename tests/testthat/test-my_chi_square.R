test_that("test_gender_activity returns correct class and structure", {
  res <- test_gender_activity(test_data, plot = FALSE)

  expect_s3_class(res, "chisq_result")
  expect_named(res, c("hypotheses", "observed", "expected", "min_expected",
                      "assumption_note", "test", "test_statistic", "df",
                      "p_value", "alpha", "decision", "conclusion", "plots"))
})

test_that("test_gender_activity observed is a table", {
  res <- test_gender_activity(test_data, plot = FALSE)
  expect_true(is.table(res$observed))
})

test_that("test_gender_activity observed row sums match gender counts", {
  res   <- test_gender_activity(test_data, plot = FALSE)
  clean <- test_data
  clean$phys <- ifelse(is.na(clean$phys), "None", clean$phys)
  expect_equal(
    sum(res$observed["Male",   ]),
    sum(clean$gender == "Male")
  )
  expect_equal(
    sum(res$observed["Female", ]),
    sum(clean$gender == "Female")
  )
})

test_that("test_gender_activity NA in phys is treated as None", {
  d_with_na <- test_data
  d_with_na$phys[1:5] <- NA
  res <- test_gender_activity(d_with_na, plot = FALSE)
  # "None" column must exist in observed table
  expect_true("None" %in% colnames(res$observed))
})

test_that("test_gender_activity min_expected is positive", {
  res <- test_gender_activity(test_data, plot = FALSE)
  expect_gt(res$min_expected, 0)
})

test_that("test_gender_activity decision matches p_value vs alpha", {
  res <- test_gender_activity(test_data, alpha = 0.05, plot = FALSE)
  expected_dec <- if (res$p_value < 0.05) "Reject H0" else "Fail to reject H0"
  expect_equal(res$decision, expected_dec)
})

test_that("test_gender_activity errors on missing columns", {
  bad <- test_data[, c("gender", "height")]
  expect_error(test_gender_activity(bad, plot = FALSE))
})

test_that("test_gender_activity plots list contains a ggplot object", {
  res <- test_gender_activity(test_data, plot = FALSE)
  expect_true(inherits(res$plots$bar, "ggplot"))
})

test_that("print.chisq_result returns invisibly", {
  res <- test_gender_activity(test_data, plot = FALSE)
  out <- capture.output(returned <- print(res))
  expect_identical(returned, res)
  expect_true(any(grepl("Research Question 3", out)))
  expect_true(any(grepl("Observed Counts", out)))
})
