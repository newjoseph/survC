skip_if_not_installed("timeROC")
skip_if_not_installed("survival")

test_that("tdroc_calc returns time/AUC table with ROC object attached", {
  library(timeROC)
  library(survival)
  set.seed(123)
  n <- 80
  sim_time <- rexp(n, rate = 0.2)
  sim_status <- rbinom(n, size = 1, prob = 0.6)
  sim_marker <- rnorm(n)
  target_times <- c(1, 2, 3)

  roc_tbl <- tdroc_calc(
    time = sim_time,
    status = sim_status,
    marker = sim_marker,
    times = target_times
  )

  expect_s3_class(roc_tbl, "data.frame")
  expect_equal(roc_tbl$time, target_times)

  roc_obj <- attr(roc_tbl, "roc_obj")
  expect_s3_class(roc_obj, "timeROC")
  expect_equal(roc_tbl$AUC, as.numeric(roc_obj$AUC))
})
