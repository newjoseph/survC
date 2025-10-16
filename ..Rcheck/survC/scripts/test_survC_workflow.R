# Reproduction of the workflow from test.R using exported survC helpers

library(data.table)
library(survival)
setwd("/home/minhyuk.kim/survC/R")

# Ensure required helper packages are available
if (!requireNamespace("jstable", quietly = TRUE)) {
  stop("Package 'jstable' is required for this workflow.")
}
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required to write Excel outputs.")
}

# Load helper functions (replace with library(survC) once packaged)
source("calc_risk_score.R")
source("c_index.R")
source("td_roc.R")
source("validation_report.R")

# -----------------------------------------------------------------------------
# Data import and labelling
# -----------------------------------------------------------------------------
out <- readRDS("out.rds")

out.label <- jstable::mk.lev(out)
out.label[variable == "SEX", `:=`(val_label = c("Male", "Female"))]
out.label[variable == "HTN", `:=`(val_label = c("No", "Yes"))]
out.label[variable == "DM", `:=`(val_label = c("No", "Yes"))]

# -----------------------------------------------------------------------------
# Part 5. Univariate / Multivariate Cox modelling
# -----------------------------------------------------------------------------
out.train <- copy(out[Type == "train"])[, ESRD := as.integer(as.character(ESRD))][]
out.train.df <- as.data.frame(out.train)
var.cov <- names(out.train)[3:17]

form <- as.formula(paste0("Surv(ESRD_day, ESRD) ~ ", paste(var.cov, collapse = "+")))
model.cox <- eval(substitute(coxph(.form, data = out.train, model = TRUE), list(.form = form)))

tb.cox <- jstable::cox2.display(model.cox, data_for_univariate = out.train.df, pcut.univariate = 0.05)

sig_vars_idx <- !is.na(tb.cox$table[, 3])
significant_vars_raw <- rownames(tb.cox$table)[sig_vars_idx]
significant_vars <- sub(":.*", "", significant_vars_raw)

selected_form <- as.formula(paste0("Surv(ESRD_day, ESRD) ~ ", paste(significant_vars, collapse = "+")))

new.model.cox <- eval(substitute(
  coxph(.form, data = out.train[complete.cases(out.train[, ..significant_vars])], model = TRUE),
  list(.form = selected_form)
))
new.tb.cox <- jstable::cox2.display(new.model.cox, data_for_univariate = out.train.df, pcut.univariate = 0.05)

new.model.cox.step <- step(new.model.cox)
new.tb.cox.step <- jstable::cox2.display(new.model.cox.step)

# -----------------------------------------------------------------------------
# Comparison tables
# -----------------------------------------------------------------------------
tb.cox.df <- as.data.frame(tb.cox$table)
tb.cox.df$variable <- rownames(tb.cox.df)

new.tb.cox.step.df <- as.data.frame(new.tb.cox.step$table[, 3:4])
colnames(new.tb.cox.step.df) <- paste("stepwise", colnames(new.tb.cox.step.df))
new.tb.cox.step.df$variable <- rownames(new.tb.cox.step.df)

final.tb.cox <- merge(tb.cox.df, new.tb.cox.step.df, by = "variable", all = TRUE)
rownames(final.tb.cox) <- final.tb.cox$variable
final.tb.cox$variable <- NULL

openxlsx::write.xlsx(final.tb.cox, file = "cox_table.xlsx", rowNames = TRUE)

# -----------------------------------------------------------------------------
# Part 6. Harrell's C-index, ROC/AUC @ fixed times (Internal / External)
# -----------------------------------------------------------------------------
times <- c(365, 730, 1095)

# Internal (train) set
out.train.naomit <- out.train[complete.cases(out.train[, ..significant_vars])]
out.train.naomit$risk_score <- calc_risk_score(new.model.cox, data = out.train.naomit)
roc_train_tbl <- tdroc_calc(time = out.train.naomit$ESRD_day,
                            status = out.train.naomit$ESRD,
                            marker = out.train.naomit$risk_score,
                            times = times)

# External (validation) set
out.val <- copy(out[Type == "val"])[, ESRD := as.integer(as.character(ESRD))][]
out.val$risk_score <- calc_risk_score(new.model.cox, data = out.val)
roc_val_tbl <- tdroc_calc(time = out.val$ESRD_day,
                          status = out.val$ESRD,
                          marker = out.val$risk_score,
                          times = times)

# Store underlying timeROC objects for downstream plotting if needed
roc_train_obj <- attr(roc_train_tbl, "roc_obj")
roc_val_obj <- attr(roc_val_tbl, "roc_obj")

# Optional: recreate the PowerPoint ROC curves using the helper wrapper
validation_report(
  train_data = out.train.naomit,
  val_data = out.val,
  model = new.model.cox,
  times = times,
  output = "ROC_Curves.pptx"
)

# -----------------------------------------------------------------------------
# Harrell's C-index via helper
# -----------------------------------------------------------------------------
model.cidx.train <- cindex_calc(new.model.cox)
model.cidx.val   <- cindex_calc(new.model.cox, newdata = out.val)
model.cidx.train.step <- cindex_calc(new.model.cox.step)
model.cidx.val.step   <- cindex_calc(new.model.cox.step, newdata = out.val)

print(model.cidx.train)
print(model.cidx.val)
print(model.cidx.train.step)
print(model.cidx.val.step)

# -----------------------------------------------------------------------------
# Stepwise: store results if needed
# -----------------------------------------------------------------------------
list(
  roc_train = roc_train_tbl,
  roc_val = roc_val_tbl,
  cindex_train = model.cidx.train,
  cindex_val = model.cidx.val,
  cindex_train_step = model.cidx.train.step,
  cindex_val_step = model.cidx.val.step
)
