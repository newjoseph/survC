skip_if_not_installed("survival")
skip_if_not_installed("timeROC")
skip_if_not_installed("officer")
skip_if_not_installed("rvg")



test_that("validation_report creates a PPTX with one slide per time point", {
  library(survival)

  set.seed(2024)
  lung <- survival::lung
  lung <- lung[complete.cases(lung[, c("time", "status", "age", "ph.ecog")]), ]
  lung$ESRD_day <- lung$time
  lung$ESRD <- as.integer(lung$status == 2)

  shuffled_idx <- sample(seq_len(nrow(lung)))
  train_idx <- shuffled_idx[1:110]
  val_idx <- shuffled_idx[111:200]

  train_df <- lung[train_idx, ]
  val_df <- lung[val_idx, ]

  expect_gt(length(unique(train_df$ESRD)), 1)
  expect_gt(length(unique(val_df$ESRD)), 1)

  model <- survival::coxph(
    survival::Surv(ESRD_day, ESRD) ~ age + ph.ecog,
    data = train_df,
    x = TRUE
  )

  target_times <- c(200, 400)
  time_unit <- "weeks"
  output_path <- tempfile(fileext = ".pptx")
  on.exit(unlink(output_path), add = TRUE)

  validation_report(
    train_data = train_df,
    val_data = val_df,
    model = model,
    times = target_times,
    time_unit = time_unit,
    output = output_path
  )

  expect_true(file.exists(output_path))

  pres <- officer::read_pptx(output_path)
  summary_df <- officer::pptx_summary(pres)

  expect_equal(length(unique(summary_df$slide_id)), length(target_times))

  title_rows <- summary_df[grepl("^ROC @", summary_df$text), ]
  expect_equal(
    title_rows$text[order(title_rows$slide_id)],
    paste0("ROC @ ", target_times, " ", time_unit)
  )

  for (tt in target_times) {
    expect_true(
      any(grepl(
        paste0("^Train - ", tt, " ", time_unit, " \\("),
        summary_df$text
      )),
      info = paste("Missing train caption for time", tt)
    )
    expect_true(
      any(grepl(
        paste0("^Validation - ", tt, " ", time_unit, " \\("),
        summary_df$text
      )),
      info = paste("Missing validation caption for time", tt)
    )
  }
})
