#' Compute risk scores from a fitted survival model
#'
#' This helper wraps `stats::predict()` for `coxph` objects so that package users
#' can easily obtain linear predictors (default) or risk scores to feed into
#' downstream metrics such as time-dependent ROC or Harrell's C-index.
#'
#' @param model A fitted `coxph` object.
#' @param data Optional dataset on which to score the model. Defaults to the
#'   training data stored within `model`.
#' @param type Scale of the predictions to return. Either `"lp"` (linear
#'   predictor, the default) or `"risk"`. If `NULL` or omitted, `"lp"` is used.
#' @param ... Additional arguments passed to [stats::predict()].
#'
#' @return A numeric vector containing the requested risk scores.
#' @export
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   fit <- survival::coxph(survival::Surv(time, status) ~ age, data = survival::lung)
#'   # Linear predictor on the training data
#'   calc_risk_score(fit)
#'
#'   # Risk scale predictions on new data
#'   calc_risk_score(fit, survival::lung, type = "risk")
#' }
calc_risk_score <- function(model, data = NULL, type = "lp", ...) {
  if (!inherits(model, "coxph")) {
    stop("`model` must be a 'coxph' object.")
  }

  type <- if (missing(type) || is.null(type)) {
    "lp"
  } else {
    match.arg(type, c("lp", "risk"), several.ok = FALSE)
  }

  prediction_data <- data

  if (is.null(prediction_data)) {
    data_expr <- model$call$data
    if (!is.null(data_expr)) {
      data_env <- attr(stats::terms(model), ".Environment")
      prediction_data <- tryCatch(
        eval(data_expr, envir = data_env),
        error = function(...) NULL
      )
    }
  }

  preds <- stats::predict(model, newdata = prediction_data, type = type, ...)

  return(as.numeric(preds))
}
