#' Prepare ADPKD cohort data for survival modelling
#'
#' This helper reproduces the preprocessing pipeline that was previously in
#' `temp.R`. It reads the baseline and follow-up Excel files, performs column
#' harmonisation, derives follow-up times and laboratory summaries, and returns a
#' cleaned dataset split into training and validation subsets.
#'
#' @param baseline_path Path to the baseline Excel file (sheet 1, skip = 1).
#' @param followup_path Path to the follow-up Excel file (sheet 1).
#' @param followup_reference Date used when the RRT start date is missing. Either
#'   a `Date` or something coercible via [as.Date()]. Defaults to
#'   "2025-08-01".
#' @param train_size Number of subjects to sample into the training set. If this
#'   exceeds the number of rows it falls back to `nrow(data)`.
#' @param seed Integer seed for the train/validation split (default `123456`).
#'
#' @return A list with elements `data` (cleaned dataset), `train`, `validation`,
#'   `varlist`, and `labels` (if `jstable` is available). All datasets are
#'   returned as `data.table` objects.
#' @import data.table
#' @export
prepare_adpkd_dataset <- function(baseline_path,
                                  followup_path,
                                  followup_reference = as.Date("2025-08-01"),
                                  train_size = 300,
                                  seed = 123456L) {
  if (!file.exists(baseline_path)) {
    stop("`baseline_path` does not exist: ", baseline_path)
  }
  if (!file.exists(followup_path)) {
    stop("`followup_path` does not exist: ", followup_path)
  }

  baseline <- readxl::read_excel(baseline_path, skip = 1, sheet = 1)
  follow <- readxl::read_excel(followup_path, sheet = 1)

  baseline <- data.table::as.data.table(baseline)
  follow <- data.table::as.data.table(follow)

  # Encoded column names that appear in the ADPKD worksheets
  col_patient_id <- "\uD658\uC790\uBC88\uD638"
  col_registration <- "\uB4F1\uB85D\uBC88\uD638"
  col_death_date <- "\uC0AC\uB9DD\uC77C\uC790"
  col_last_visit <- "\uCD5C\uC885\uC9C4\uB8CC\uC77C\uC790"
  col_lab_result <- "\uAC80\uC0AC\uACB0\uACFC"
  col_birth_date <- "\uC0DD\uB144\uC6D4\uC77C"
  col_weight <- "\uBAB8\uBB34\uAC8C"
  col_height <- "\uD0A4"
  col_subject_id <- "D658C790BC88D638"
  pattern_lab_date <- paste0("^", "\uAC80\uC0AC\uC77C\uC790")
  pattern_lab_result <- paste0("^", "\uAC80\uC0AC\uACB0\uACFC")

  required_cols <- c("B4F1B85DBC88D638", "C0ACB9DDC77CC790", "CD5CC885C9C4B8CCC77CC790", "AC80C0ACACB0ACFC")
  missing_cols <- setdiff(required_cols, names(follow))
  if (length(missing_cols) > 0) {
    stop("`followup_path` is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  follow_processed <- follow[, {
    cols <- list(
      patient_id_tmp = get(col_registration),
      LastFUdate = as.Date(pmin(as.Date(get(col_death_date)), as.Date(get(col_last_visit)), na.rm = TRUE)),
      SUPCR = get(col_lab_result)
    )
    if (col_subject_id %in% names(.SD)) {
      cols[[col_subject_id]] <- get(col_subject_id)
    }
    data.table::as.data.table(cols)
  }]
  data.table::setnames(follow_processed, old = "patient_id_tmp", new = col_patient_id)

  merge_key <- if (col_subject_id %in% names(follow_processed)) col_subject_id else col_patient_id
  baseline <- merge(baseline, follow_processed, by = merge_key, all.x = TRUE)

  col_map <- c(
    "Hemoglobin" = "HB",
    "BUN" = "BUN",
    "Creatinine" = "SCR",
    "sodium" = "Sodium",
    "potassium" = "K",
    "chloride" = "CL",
    "albumin" = "albumin",
    "uric acid" = "UA",
    "glucose" = "FSG",
    "total calcium" = "LBCA",
    "magnesium" = "magnesium",
    "inorganic phosphorus" = "P",
    "LDL-cholesterol" = "LDL",
    "HDL-cholesterol" = "HDL",
    "triglyceride" = "triglyceride",
    "protein (urine)" = "protein (urine)",
    "Creatinine (Urine)" = "Creatinine (Urine)"
  )

  data.table::setnames(baseline, old = "D658C790BC88D638", new = "SUBJ_ID")

  visit_cols <- grep("^Q612CD5CCD08C9C4B2E8C77CC790", names(baseline), value = TRUE)
  if (length(visit_cols) == 0) {
    stop("Baseline data must include a column that matches '^Q612CD5CCD08C9C4B2E8C77CC790'.")
  }
  data.table::setnames(baseline, old = visit_cols[1], new = "VISIT_DT")
  if (length(visit_cols) > 1) {
    baseline[, (visit_cols[-1]) := NULL]
  }

  sex_cols <- grep("^C131BCC4", names(baseline), value = TRUE)
  if (length(sex_cols) == 0) {
    stop("Baseline data must include a column that matches '^C131BCC4'.")
  }
  data.table::setnames(baseline, old = sex_cols[1], new = "SEX")
  if (length(sex_cols) > 1) {
    baseline[, (sex_cols[-1]) := NULL]
  }

  baseline[, SEX := data.table::fifelse(SEX == "M", 1L, 2L)]

  baseline[, VISIT_DT := as.Date(VISIT_DT)]
  if ("C0DDB144C6D4C77C" %in% names(baseline)) {
    baseline[, (col_birth_date) := as.Date(get(col_birth_date))]
  } else {
    stop("Baseline data must include the birth-date column 'C0DDB144C6D4C77C'.")
  }
  baseline[, AGE := as.numeric((VISIT_DT - get(col_birth_date)) / 365.25)]
  baseline[, VISIT_NM := "Baseline"]
  if (!all(c("BAB8BB34AC8C", "D0A4") %in% names(baseline))) {
    stop("Baseline data must include the anthropometric columns 'BAB8BB34AC8C' and 'D0A4'.")
  }
  baseline[, BMI := get(col_weight) / (get(col_height) / 100) ^ 2]

  if ("ACE0D608C555C57DC81C BCF5C6A9C720BB34" %in% names(baseline)) {
    data.table::setnames(baseline, old = "ACE0D608C555C57DC81C BCF5C6A9C720BB34", new = "HTN")
    baseline[, HTN := data.table::fifelse(HTN == "Y", 1L, 0L)]
  } else {
    stop("Baseline data must include 'ACE0D608C555C57DC81C BCF5C6A9C720BB34'.")
  }
  if ("B2F9B178C720BB34(E11CF54B4DCC720BB34)" %in% names(baseline)) {
    data.table::setnames(baseline, old = "B2F9B178C720BB34(E11CF54B4DCC720BB34)", new = "DM")
    baseline[, DM := data.table::fifelse(DM == "Y", 1L, 0L)]
  } else {
    stop("Baseline data must include 'B2F9B178C720BB34(E11CF54B4DCC720BB34)'.")
  }
  if ("N185B4F1B85DC5ECBD80( 0/1)" %in% names(baseline)) {
    data.table::setnames(baseline, old = "N185B4F1B85DC5ECBD80( 0/1)", new = "ESRD")
  } else {
    stop("Baseline data must include 'N185B4F1B85DC5ECBD80( 0/1)'.")
  }
  if ("N185CD5CCD08C9C4B2E8C77CC790" %in% names(baseline)) {
    data.table::setnames(baseline, old = "N185CD5CCD08C9C4B2E8C77CC790", new = "RRTDST")
  } else {
    stop("Baseline data must include 'N185CD5CCD08C9C4B2E8C77CC790'.")
  }

  followup_reference <- as.Date(followup_reference)

  baseline[, ESRD_day := data.table::fifelse(
    is.na(RRTDST),
    as.integer(followup_reference - VISIT_DT),
    as.integer(as.Date(RRTDST) - VISIT_DT)
  )]

  if ("Laboratory Data" %in% names(baseline)) {
    baseline[, `Laboratory Data` := NULL]
  }
  lab_date_cols <- grep(pattern_lab_date, names(baseline), value = TRUE)
  if (length(lab_date_cols) > 0) {
    baseline[, (lab_date_cols) := NULL]
  }

  lab_result_cols <- grep(pattern_lab_result, names(baseline), value = TRUE)
  if (length(lab_result_cols) < length(col_map)) {
    stop("Baseline data must include at least ", length(col_map),
         " lab result columns matching '^\\uAC80\\uC0AC\\uACB0\\uACFC'.")
  }
  data.table::setnames(baseline, old = lab_result_cols[seq_along(col_map)], new = names(col_map))
  extra_lab_cols <- lab_result_cols[-seq_along(col_map)]
  if (length(extra_lab_cols) > 0) {
    baseline[, (extra_lab_cols) := NULL]
  }

  baseline[, (names(col_map)) := lapply(.SD, as.numeric), .SDcols = names(col_map)]
  data.table::setnames(baseline, old = names(col_map), new = unname(col_map))

  baseline[, SCCGFR := ((140 - AGE) * get(col_weight)) / (72 * SCR) *
             data.table::fifelse(SEX == 2L, 0.85, 1)]
  baseline[, CKDGFR := 142 *
             (pmin(SCR / data.table::fifelse(SEX == 2L, 0.7, 0.9), 1) ^ data.table::fifelse(SEX == 2L, -0.241, -0.302)) *
             (pmax(SCR / data.table::fifelse(SEX == 2L, 0.7, 0.9), 1) ^ -1.2) *
             (0.9938 ^ AGE) * data.table::fifelse(SEX == 2L, 1.012, 1)]

  baseline[, date_only := as.Date(data.table::fifelse(ESRD %in% "0", LastFUdate, RRTDST))]
  baseline[, ESRD_day := as.integer(as.Date(date_only) - VISIT_DT)]

  baseline[, SUPCR := data.table::fifelse(
    grepl("neg", SUPCR, ignore.case = TRUE) | grepl("Trace", SUPCR, ignore.case = TRUE),
    "neg",
    data.table::fifelse(is.na(SUPCR), NA_character_, "pos")
  )]

  set.seed(seed)
  n <- nrow(baseline)
  train_n <- min(train_size, n)
  train_idx <- sample.int(n, size = train_n)
  baseline[, Type := "val"]
  baseline[train_idx, Type := "train"]

  varlist <- list(
    Base = c("Type", "SEX", "AGE", "HTN", "DM", "BMI"),
    Lab = c("UA", "SUPCR", "CKDGFR", "HB", "FSG",
            "LBCA", "P", "Sodium", "K", "CL"),
    Outcome = c("ESRD"),
    Time = c("ESRD_day")
  )

  keep_cols <- c("SUBJ_ID", unlist(varlist, use.names = FALSE))
  keep_cols <- intersect(keep_cols, names(baseline))
  out <- data.table::copy(baseline[!is.na(ESRD_day), ..keep_cols])

  factor_vars <- names(out)[vapply(out, function(col) length(unique(col)), integer(1L)) <= 8]
  if (length(factor_vars) > 0) {
    out[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]
  }
  conti_vars <- setdiff(names(out), c("SUBJ_ID", factor_vars))
  if (length(conti_vars) > 0) {
    out[, (conti_vars) := lapply(.SD, as.numeric), .SDcols = conti_vars]
  }

  labels <- NULL
  if (requireNamespace("jstable", quietly = TRUE)) {
    labels <- data.table::as.data.table(jstable::mk.lev(data.table::as.data.table(out)))
    if (nrow(labels) > 0) {
      labels[variable == "SEX", val_label := c("Male", "Female")]
      labels[variable == "HTN", val_label := c("No", "Yes")]
      labels[variable == "DM", val_label := c("No", "Yes")]
    }
  }

  result <- list(
    data = out,
    train = data.table::copy(out[Type == "train"]),
    validation = data.table::copy(out[Type == "val"]),
    varlist = varlist,
    labels = labels,
    train_indices = train_idx
  )

  return(result)
}

utils::globalVariables(c(
  "variable", "val_label", "\uC21C\uBC88", "VISIT_DT", "C0DDB144C6D4C77C", "BAB8BB34AC8C", "D0A4",
  "LastFUdate", "RRTDST", "SUPCR", "ESRD_day", "Type", "..keep_cols", ".SD",
  "AGE", "BMI", "CKDGFR", "DM", "ESRD", "HTN", "Laboratory Data", "SCCGFR",
  "SCR", "SEX", "VISIT_NM", "date_only"
))
