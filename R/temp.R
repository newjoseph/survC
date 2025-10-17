if (interactive()) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for this example.")
  }

  set.seed(2024)
  lung <- survival::lung
  lung <- lung[complete.cases(lung[, c("time", "status", "age", "ph.ecog")]), ]

  # Split into training and validation cohorts
  split_ids <- sample(seq_len(nrow(lung)))
  train_idx <- split_ids[1:110]
  val_idx <- split_ids[111:200]

  train_df <- lung[train_idx, ]
  val_df <- lung[val_idx, ]

  # Fit a simple Cox model on the training data
  cox_fit <- survival::coxph(
    survival::Surv(time, status == 2) ~ age + ph.ecog,
    data = train_df,
    x = TRUE
  )

  # 1. Linear predictor / risk scores
  train_lp <- calc_risk_score(cox_fit)
  val_lp <- calc_risk_score(cox_fit, data = val_df)

  # 2. Harrell's concordance on the validation cohort
  c_index_val <- cindex_calc(cox_fit, newdata = val_df)
  print(c_index_val)

  # 3. Time-dependent ROC summary at selected horizons
  horizons <- c(200, 400)
  roc_tbl <- tdroc_calc(
    time = val_df$time,
    status = as.integer(val_df$status == 2),
    marker = val_lp,
    times = horizons
  )
  print(roc_tbl)

  validation_report(
    train_data = transform(train_df, time = time, status = as.integer(status == 2)),
    val_data = transform(val_df, time = time, status = as.integer(status == 2)),
    model = cox_fit,
    time_col = "time",
    status_col = "status",
    times = horizons,
    time_unit = "days",
    output = "validation_report.pptx"
  )
}
