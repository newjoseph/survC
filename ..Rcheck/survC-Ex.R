pkgname <- "survC"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('survC')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calc_risk_score")
### * calc_risk_score

flush(stderr()); flush(stdout())

### Name: calc_risk_score
### Title: Compute risk scores from a fitted survival model
### Aliases: calc_risk_score

### ** Examples

if (requireNamespace("survival", quietly = TRUE)) {
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = survival::lung)
  # Linear predictor on the training data
  calc_risk_score(fit)

  # Risk scale predictions on new data
  calc_risk_score(fit, survival::lung, type = "risk")
}



cleanEx()
nameEx("cindex_calc")
### * cindex_calc

flush(stderr()); flush(stdout())

### Name: cindex_calc
### Title: Calculate Harrell's C-index with 95% CI
### Aliases: cindex_calc

### ** Examples

library(survival)
fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
cindex_calc(fit)






### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
