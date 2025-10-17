skip_if_not_installed("survival")

test_that("calc_risk_score matches predict() on training data", {
  library(survival)
  lung <- survival::lung
  lung <- lung[complete.cases(lung[, c("time", "status", "age")]), ]
  model <- survival::coxph(survival::Surv(time, status) ~ age, data = lung, x = TRUE)

  expect_equal(
    calc_risk_score(model),
    as.numeric(stats::predict(model, type = "lp"))
  )
})

test_that("calc_risk_score supports risk scale and newdata", {
  library(survival)
  lung <- survival::lung
  lung <- lung[complete.cases(lung[, c("time", "status", "age")]), ]
  model <- survival::coxph(survival::Surv(time, status) ~ age, data = lung, x = TRUE)

  expect_equal(
    calc_risk_score(model, type = "risk"),
    as.numeric(stats::predict(model, type = "risk"))
  )

  subset_rows <- lung[1:10, ]
  expect_equal(
    calc_risk_score(model, data = subset_rows, type = "lp"),
    as.numeric(stats::predict(model, newdata = subset_rows, type = "lp"))
  )
})

test_that("calc_risk_score errors on non-coxph objects", {

  dummy_lm <- stats::lm(mpg ~ cyl, data = mtcars)
  expect_error(
    calc_risk_score(dummy_lm),
    "must be a 'coxph' object"
  )
})
