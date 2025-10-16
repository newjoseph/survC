#' Calculate time-dependent ROC and AUC
#' @param time Survival time vector
#' @param status Event indicator (1 = event, 0 = censor)
#' @param marker Risk score or linear predictor
#' @param times Vector of time points (e.g., c(365, 730, 1095))
#' @return A data.frame with AUCs for each time
#' @export
#'


tdroc_calc <- function(time, status, marker, times){
  ## ---- Core validation ----
  if (!requireNamespace("timeROC", quietly = TRUE)) {
    stop("Package 'timeROC' must be installed to use tdroc_calc().")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' must be installed to use tdroc_calc().")
  }

  if (!is.numeric(time) || !is.numeric(status) || !is.numeric(marker)) {
    stop("All inputs (time, status, marker) must be numeric vectors.")
  }

  n <- length(time)
  if (length(status) != n || length(marker) != n) {
    stop("time, status, and marker must have the same length.")
  }

  if (anyNA(c(time, status, marker))) {
    stop("Inputs must not contain NA values.")
  }

  if (!all(status %in% c(0, 1))) {
    stop("Event indicator 'status' must contain only 0 and 1.")
  }

  if (length(unique(status)) == 1) {
    stop("All status values are identical (all 0 or all 1). Cannot compute ROC/AUC.")
  }

  if (length(unique(marker)) == 1) {
    stop("Marker values are constant. ROC/AUC cannot be computed.")
  }

  if (!is.numeric(times) || any(times <= 0)) {
    stop("Times must be a positive numeric vector.")
  }

  if (max(times) > max(time)) {
    warning("Some 'times' exceed the maximum observed survival time.")
  }

  Surv <- survival::Surv  # ensure timeROC internals can find Surv()
  roc_res <- timeROC::timeROC(T=time, delta=status, marker=marker,
                              cause=1, weighting="marginal",
                              times=times, iid=TRUE)
  if (!inherits(roc_res, "timeROC")) {
    class(roc_res) <- c("timeROC", class(roc_res))
  }
  auc_table <- data.frame(time=roc_res$times, AUC=roc_res$AUC)
  attr(auc_table, "roc_obj") <- roc_res
  return(auc_table)
  #return(roc_res)
}
