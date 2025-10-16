skip_if_not_installed("survival")

test_that("cindex_calc matches survival::concordance output", {
  library(survival)
  lung <- survival::lung
  lung <- lung[complete.cases(lung[, c("time", "status", "age", "sex")]), ]
  cox_model <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)

  reference <- survival::concordance(cox_model)
  ci_bounds <- coef(reference) + c(-1, 1) * sqrt(reference$var) * qnorm(0.975)
  expected <- c(
    Cindex = round(reference$concordance, 3),
    Lower  = round(ci_bounds[1], 3),
    Upper  = round(ci_bounds[2], 3)
  )

  result <- cindex_calc(cox_model)

  expect_equal(result, expected)

  # Additional sanity checks on result shape and range
  expect_type(result, "double")
  expect_true(all(!is.na(result)))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("cindex_calc accepts newdata and returns bounded result", {
  library(survival)
  lung <- survival::lung
  lung <- lung[complete.cases(lung[, c("time", "status", "age", "sex")]), ]
  cox_model <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)

  set.seed(42)
  idx <- sample(seq_len(nrow(lung)), 50)
  newdata <- lung[idx, ]

  result <- cindex_calc(cox_model, newdata = newdata)

  expect_silent(result)
  expect_type(result, "double")
  expect_true(all(!is.na(result)))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("cindex_calc handles no events gracefully", {
  # Force all status values to zero (no events)
  lung <- survival::lung
  lung$status <- 0

  cox_model <- survival::coxph(Surv(time, status) ~ age + sex, data = lung)

  # Expect cindex_calc to raise an error or warning in this situation
  expect_error_or_warning <- function(expr) {
    expect_true(tryCatch({
      expr
      FALSE  # Fail the test when no error/warning is raised
    }, error = function(e) TRUE, warning = function(w) TRUE))
  }

  expect_error_or_warning(cindex_calc(cox_model))
})


test_that("cindex_calc returns NA when all events occurred", {
  lung <- survival::lung
  lung$status <- 1

  cox_model <- survival::coxph(Surv(time, status) ~ age + sex, data = lung)
  result <- suppressWarnings(cindex_calc(cox_model))

  expect_true(all(is.na(result)))
})
