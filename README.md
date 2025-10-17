
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survC

<!-- badges: start -->

[![R-CMD-check](https://github.com/newjoseph/survC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/newjoseph/survC/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`survC` provides lightweight utilities for validating survival models.
The package focuses on Cox regression workflows, wrapping common
validation tasks such as extracting linear predictors, computing
Harrell’s concordance, and summarising time-dependent ROC curves. It
also includes helpers for preparing cohort data and producing
slide-based validation reports.

## Installation

Install the development version of `survC` from GitHub with:

``` r
# install.packages("pak")
pak::pak("newjoseph/survC")
```

The examples below require the `survival`, `timeROC`, `officer`, and
`rvg` packages. They are listed in `Imports` and will be pulled in
automatically when `survC` is installed.

## Key features

- Convert fitted `coxph` models into linear predictors or risk scores
  with `calc_risk_score()` for downstream metrics.
- Summarise time-dependent ROC curves across custom horizons via
  `tdroc_calc()` and inspect the underlying `timeROC` object.
- Compute Harrell’s C-index (with a 95% confidence interval) for
  training or validation cohorts using `cindex_calc()`.
- Generate multi-slide PowerPoint reports that compare training and
  validation ROC curves at designated time points with
  `validation_report()`.
- Prepare Excel-based cohort extracts for downstream modelling with
  `prepare_adpkd_dataset()`.

## Quick start

The example below demonstrates a typical validation workflow using the
`survival::lung` dataset.

``` r
library(survC)
library(survival)

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
c_index_val
#> Cindex  Lower  Upper 
#>  0.561  0.481  0.641

# 3. Time-dependent ROC summary at selected horizons
horizons <- c(200, 400)
roc_tbl <- tdroc_calc(
  time = val_df$time,
  status = as.integer(val_df$status == 2),
  marker = val_lp,
  times = horizons
)
roc_tbl
#>       time       AUC
#> t=200  200 0.5981693
#> t=400  400 0.5054571
```

The returned AUC table mirrors `timeROC` output and carries the full ROC
object as the `roc_obj` attribute for plotting or inspection:

``` r
str(attr(roc_tbl, "roc_obj"))
#> List of 12
#>  $ TP                 : num [1:53, 1:2] 0 0 0.0319 0.0319 0.0628 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:2] "t=200" "t=400"
#>  $ FP                 : num [1:53, 1:2] 0 0.0185 0.0185 0.037 0.037 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:2] "t=200" "t=400"
#>  $ AUC                : Named num [1:2] 0.598 0.505
#>   ..- attr(*, "names")= chr [1:2] "t=200" "t=400"
#>  $ times              : num [1:2] 200 400
#>  $ CumulativeIncidence: Named num [1:2] 0.359 0.666
#>   ..- attr(*, "names")= chr [1:2] "t=200" "t=400"
#>  $ survProb           : Named num [1:2] 0.641 0.334
#>   ..- attr(*, "names")= chr [1:2] "t=200" "t=400"
#>  $ n                  : int 90
#>  $ Stats              : int [1:2, 1:3] 32 54 54 21 4 15
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:2] "t=200" "t=400"
#>   .. ..$ : chr [1:3] "Cases" "survivor at t" "Censored at t"
#>  $ weights            :List of 6
#>   ..$ times            : num [1:2] 200 400
#>   ..$ IPCW.times       : num [1:2] 0.936 0.7
#>   ..$ IPCW.subjectTimes: num [1:90] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ fit              :List of 29
#>   .. ..$ time                 : num [1:83] 5 13 30 53 60 61 62 65 79 81 ...
#>   .. ..$ n.risk               : num [1:83] 90 89 88 87 86 85 84 83 82 81 ...
#>   .. ..$ n.event              : num [1:83] 1 1 1 1 1 1 1 1 1 2 ...
#>   .. ..$ n.lost               : num [1:83] 0 0 0 0 0 0 0 0 0 0 ...
#>   .. ..$ surv                 : num [1:83] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..$ se.surv              : num [1:83] 0 0 0 0 0 0 0 0 0 0 ...
#>   .. ..$ hazard               : num [1:83] 0 0 0 0 0 0 0 0 0 0 ...
#>   .. ..$ first.strata         : int 1
#>   .. ..$ size.strata          : int 83
#>   .. ..$ model                : chr "survival"
#>   .. ..$ maxtime              : num 840
#>   .. ..$ lower                : num [1:83] 0 0 0 0 0 0 0 0 0 0 ...
#>   .. ..$ upper                : num [1:83] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..$ call                 : language prodlim::prodlim(formula = formula, data = data, reverse = TRUE)
#>   .. ..$ formula              :Class 'formula'  language Surv(failure_time, status) ~ 1
#>   .. .. .. ..- attr(*, ".Environment")=<environment: 0x56261d918d98> 
#>   .. ..$ model.response       : 'Hist' num [1:90, 1:2] 5 13 30 53 60 61 62 65 79 81 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:90] "1" "2" "3" "4" ...
#>   .. .. .. ..$ : chr [1:2] "time" "status"
#>   .. .. ..- attr(*, "model")= chr "survival"
#>   .. .. ..- attr(*, "cens.type")= chr "rightCensored"
#>   .. .. ..- attr(*, "cens.code")= num 0
#>   .. .. ..- attr(*, "entry.type")= chr ""
#>   .. ..$ originalDataOrder    : int [1:90] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..$ X                    : NULL
#>   .. ..$ model.matrix         : NULL
#>   .. ..$ discrete.predictors  : NULL
#>   .. ..$ continuous.predictors: NULL
#>   .. ..$ xlevels              : NULL
#>   .. ..$ clustervar           : NULL
#>   .. ..$ covariate.type       : num 1
#>   .. ..$ cens.type            : chr "rightCensored"
#>   .. ..$ conf.int             : num 0.95
#>   .. ..$ reverse              : logi TRUE
#>   .. ..$ type                 : chr "risk"
#>   .. ..$ na.action            : NULL
#>   .. ..- attr(*, "class")= chr "prodlim"
#>   ..$ call             : language ipcw.marginal(formula = Surv(failure_time, status) ~ 1, data = data.frame(failure_time = T,      status = as.nume| __truncated__ ...
#>   ..$ method           : chr "marginal"
#>   ..- attr(*, "class")= chr "IPCW"
#>  $ inference          :List of 5
#>   ..$ mat_iid_rep_2     : logi [1:90, 1:2] NA NA NA NA NA NA ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : NULL
#>   .. .. ..$ : chr [1:2] "t=200" "t=400"
#>   ..$ mat_iid_rep_1     : num [1:90, 1:2] -1.2273 0.0103 1.0157 -1.1499 -0.0671 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : NULL
#>   .. .. ..$ : chr [1:2] "t=200" "t=400"
#>   ..$ vect_sd_1         : Named num [1:2] 0.0638 0.0745
#>   .. ..- attr(*, "names")= chr [1:2] "t=200" "t=400"
#>   ..$ vect_sd_2         : Named logi [1:2] NA NA
#>   .. ..- attr(*, "names")= chr [1:2] "t=200" "t=400"
#>   ..$ vect_iid_comp_time: Named num [1:2] 0.00299 0.00297
#>   .. ..- attr(*, "names")= chr [1:2] "t=200" "t=400"
#>  $ computation_time   : 'difftime' num 0.0150535106658936
#>   ..- attr(*, "units")= chr "secs"
#>  $ iid                : logi TRUE
#>  - attr(*, "class")= chr [1:2] "timeROC" "ipcwsurvivalROC"
```

### Validation report (PowerPoint)

Use `validation_report()` to export one slide per horizon with training
and validation ROC curves side-by-side. The function relies on `officer`
and `rvg` so plots remain editable.

``` r
validation_report(
  train_data = transform(train_df, ESRD_day = time, ESRD = as.integer(status == 2)),
  val_data = transform(val_df, ESRD_day = time, ESRD = as.integer(status == 2)),
  model = cox_fit,
  time_col = "ESRD_day",
  status_col = "ESRD",
  times = horizons,
  time_unit = "days",
  output = "validation_report.pptx"
)
```

## Development

Re-knit the README after editing with:

``` r
devtools::build_readme()
```

This regenerates `README.md` so GitHub reflects the latest examples.
