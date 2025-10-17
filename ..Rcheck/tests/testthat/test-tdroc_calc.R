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

test_that("tdroc_calc computes ROC curves with expected discrimination", {
  library(timeROC)
  library(survival)
  set.seed(42)
  n <- 200
  marker <- rnorm(n)
  event_time <- rexp(n, rate = exp(marker))
  censor_time <- rexp(n, rate = 0.15)
  time <- pmin(event_time, censor_time)
  status <- as.integer(event_time <= censor_time)
  target_times <- c(1, 2, 3)

  roc_tbl <- tdroc_calc(
    time = time,
    status = status,
    marker = marker,
    times = target_times
  )

  expect_equal(
    roc_tbl$AUC,
    c(0.8224654, 0.9003468, 0.9028587),
    tolerance = 1e-6
  )

  roc_obj <- attr(roc_tbl, "roc_obj")
  monotone_increasing <- function(mat) {
    apply(mat, 2, function(col) all(diff(col) >= -1e-10))
  }

  expect_true(all(monotone_increasing(roc_obj$TP)))
  expect_true(all(monotone_increasing(roc_obj$FP)))
  expect_equal(
    unname(roc_obj$TP[nrow(roc_obj$TP), ]),
    rep(1, length(target_times)),
    tolerance = 1e-12
  )
  expect_equal(
    unname(roc_obj$FP[1, ]),
    rep(0, length(target_times)),
    tolerance = 1e-12
  )
})
