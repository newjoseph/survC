#' Generate survival model validation report
#' @param train_data training dataset containing survival outcomes.
#' @param val_data validation dataset containing survival outcomes.
#' @param model fitted 'coxph'
#' @param time_col name of the survival time column present in both datasets
#' @param status_col name of the event indicator column present in both datasets
#' @param times follow-up timepoints
#' @param time_unit character label for the time axis (default = "days")
#' @param output file path (.pptx or .html)
#' @return Writes validation report
#' @importFrom graphics par title
#' @export
validation_report <- function(train_data, val_data, model,
                              time_col,
                              status_col,
                              times = c(365, 730, 1095),
                              time_unit = "days",
                              output = "validation_report.pptx") {

  if (missing(time_col) || is.null(time_col)) {
    stop("`time_col` must be supplied as the name of the survival time column.")
  }
  if (missing(status_col) || is.null(status_col)) {
    stop("`status_col` must be supplied as the name of the event indicator column.")
  }

  if (!all(c(time_col, status_col) %in% names(train_data))) {
    stop("`train_data` must contain columns `", time_col, "` and `", status_col, "`.")
  }
  if (!all(c(time_col, status_col) %in% names(val_data))) {
    stop("`val_data` must contain columns `", time_col, "` and `", status_col, "`.")
  }

  train_time <- train_data[[time_col]]
  train_status <- train_data[[status_col]]
  val_time <- val_data[[time_col]]
  val_status <- val_data[[status_col]]

  # Risk marker computation via helper
  train_marker <- calc_risk_score(model, data = train_data)
  val_marker <- calc_risk_score(model, data = val_data)

  # Time-dependent ROC using helper (retain underlying object for plotting)
  roc_train_tbl <- tdroc_calc(time = train_time,
                              status = train_status,
                              marker = train_marker,
                              times = times)
  roc_val_tbl <- tdroc_calc(time = val_time,
                            status = val_status,
                            marker = val_marker,
                            times = times)

  roc_train <- attr(roc_train_tbl, "roc_obj")
  roc_val <- attr(roc_val_tbl, "roc_obj")

  # Report via officer
  doc <- officer::read_pptx()
  for (t in times) {
    auc_tr <- round(roc_train_tbl$AUC[roc_train_tbl$time == t], 3)
    auc_val <- round(roc_val_tbl$AUC[roc_val_tbl$time == t], 3)
    plot_code <- rvg::dml(code = {
      graphics::par(mfrow = c(1, 2))
      plot(roc_train, time = t, lwd = 2, title = FALSE)
      graphics::title(main = paste0("Train - ", t, " ", time_unit, " (AUC=", auc_tr, ")"))
      plot(roc_val, time = t, lwd = 2, title = FALSE)
      graphics::title(main = paste0("Validation - ", t, " ", time_unit, " (AUC=", auc_val, ")"))
    })
    doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
    doc <- officer::ph_with(doc, value = paste0("ROC @ ", t, " ", time_unit),
                            location = officer::ph_location_type("title"))
    doc <- officer::ph_with(doc, plot_code, location = officer::ph_location_type("body"))
  }
  print(doc, target = output)
}
