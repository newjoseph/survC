#' Calculate Harrell's C-index with 95% CI
#' @param model a 'coxph' object
#' @param newdata optional validation dataset
#' @param digits number of decimal places for rounding (default 3).
#' @return numeric vector of C-index (lower, upper)
#' @export
#'
#' @examples
#' library(survival)
#' fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
#' cindex_calc(fit)
#'
#'
#'

cindex_calc <- function(model, newdata = NULL, digits = 3){

  if (!inherits(model, "coxph")) {
    stop("The model must be a coxph object.")
  }

  ## ---- Helpers to locate the status indicator ----
  extract_status_from_data <- function(data_obj, model_formula) {
    if (is.null(data_obj)) {
      return(NULL)
    }
    surv_vars <- all.vars(model_formula[[2]])
    if (length(surv_vars) < 2) {
      return(NULL)
    }
    status_var <- surv_vars[2]
    if (status_var %in% names(data_obj)) {
      return(data_obj[[status_var]])
    }
    NULL
  }

  extract_status_from_model <- function(model_obj) {
    if (!is.null(model_obj$y) && inherits(model_obj$y, "Surv")) {
      return(model_obj$y[, "status"])
    }
    if (!is.null(model_obj$model)) {
      status_vec <- extract_status_from_data(model_obj$model, model_obj$formula)
      if (!is.null(status_vec)) {
        return(status_vec)
      }
    }
    model_data <- tryCatch(
      eval(model_obj$call$data, parent.frame()),
      error = function(e) NULL
    )
    extract_status_from_data(model_data, model_obj$formula)
  }

  ## ---- Determine status vector ----
  status_vec <- NULL
  if (!is.null(newdata)) {
    status_vec <- extract_status_from_data(newdata, model$formula)
    if (is.null(status_vec)) {
      stop("'newdata' must contain the status column used in the model formula.")
    }
  } else {
    status_vec <- extract_status_from_model(model)
  }

  ## ---- Handle degenerate event patterns ----
  if (!is.null(status_vec)) {
    status_clean <- status_vec[!is.na(status_vec)]
    if (length(status_clean) == 0) {
      stop("Event variable ('status') is missing for all observations.")
    }
    uniq_status <- unique(status_clean)
    if (length(uniq_status) == 1) {
      single_val <- uniq_status[[1]]
      if (identical(single_val, 1L) || identical(single_val, 1)) {
        warning("All observations correspond to events (status == 1). Returning NA for the C-index.")
        na_out <- rep(NA_real_, 3)
        names(na_out) <- c("Cindex", "Lower", "Upper")
        return(na_out)
      }
      stop("Event variable ('status') has only one value -- all 0 or all 1. C-index cannot be computed.")
    }
  }

  # Concordance calculation via survival::concordance()
  cobj <- survival::concordance(model, newdata = newdata)

  # Build 95% confidence interval around the concordance estimate
  ci <- stats::coef(cobj) + c(-1, 1) * sqrt(cobj$var) * stats::qnorm(1 - 0.05/2)

  # Assemble rounded output vector
  out <- c(
    Cindex = round(cobj$concordance, digits),
    Lower  = round(ci[1], digits),
    Upper  = round(ci[2], digits)
  )

  return(out)
}
