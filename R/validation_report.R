#' Generate survival model validation report
#' @param train_data training dataset containing survival outcomes.
#' @param val_data validation dataset containing survival outcomes.
#' @param model fitted 'coxph'
#' @param times follow-up timepoints
#' @param time_unit character label for the time axis (default = "days")
#' @param output file path (.pptx or .html)
#' @return Writes validation report
#' @importFrom graphics par title
#' @export
validation_report <- function(train_data, val_data, model, times = c(365, 730, 1095),
                              time_unit = "days",
                              output = "validation_report.pptx") {

  # Risk marker computation via helper
  train_marker <- calc_risk_score(model, data = train_data)
  val_marker <- calc_risk_score(model, data = val_data)

  # Time-dependent ROC using helper (retain underlying object for plotting)
  roc_train_tbl <- tdroc_calc(time = train_data$ESRD_day,
                              status = train_data$ESRD,
                              marker = train_marker,
                              times = times)
  roc_val_tbl <- tdroc_calc(time = val_data$ESRD_day,
                            status = val_data$ESRD,
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
