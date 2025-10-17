
## import RDS
out <- readRDS("out.rds")

#out$Subject <- factor(out$Subject)
out.label <- jstable::mk.lev(out)

out.label[variable == "SEX", `:=`(val_label = c("Male", "Female"))]
out.label[variable == "HTN", `:=`(val_label = c("No", "Yes"))]
out.label[variable == "DM", `:=`(val_label = c("No", "Yes"))]




################################################################################
# Part 5. Univariate/Multivariate
################################################################################
out.train <- copy(out[Type == "train"])[, ESRD := as.integer(as.character(ESRD))][]
var.cov <- names(out.train)[3:17]
# "SEX", "AGE","HTN","DM","BMI","UA","SUPCR","CKDGFR", "HB","FSG","LBCA","P","Sodium", "K","CL"


form <- as.formula(paste0("Surv(ESRD_day, ESRD) ~ ", paste(var.cov, collapse = "+")))
model.cox <- eval(substitute(coxph(.form, data = out.train, model = T), list(.form = form)))
tb.cox <- cox2.display(model.cox, data_for_univariate = out.train, pcut.univariate = 0.05)

sig_vars_idx <- tb.cox$table[, 3] %>% is.na() == F
significant_vars_raw <- rownames(tb.cox$table)[sig_vars_idx]
significant_vars <- sub(":.*", "", significant_vars_raw)
significant_vars

selected_form <- as.formula(paste0("Surv(ESRD_day, ESRD) ~ ", paste(significant_vars, collapse = "+")))
selected_form


new.model.cox <- eval(substitute(coxph(.form, data = out.train[complete.cases(out.train[, ..significant_vars])], model = T), list(.form = selected_form)))
new.tb.cox <- cox2.display(new.model.cox, data_for_univariate = out.train, pcut.univariate = 0.05)
new.tb.cox

#step(new.model.cox) %>% cox2.display()
new.model.cox.step <- step(new.model.cox)
new.tb.cox.step <- new.model.cox.step %>% cox2.display()
new.tb.cox.step

tb.cox.df <- as.data.frame(tb.cox$table)
tb.cox.df$variable = rownames(tb.cox.df)
tb.cox.df

new.tb.cox.step.df <- as.data.frame(new.tb.cox.step$table[,3:4])
colnames(new.tb.cox.step.df) = paste("stepwise",colnames(new.tb.cox.step.df))
new.tb.cox.step.df$variable = rownames(new.tb.cox.step.df)
new.tb.cox.step.df


final.tb.cox <- merge(tb.cox.df, new.tb.cox.step.df, by = "variable", all = TRUE)
rownames(final.tb.cox) <- final.tb.cox$variable
final.tb.cox$variable = NULL
final.tb.cox
write.xlsx(final.tb.cox, file = "cox_table.xlsx", rowNames = T)


################################################################################
# Part 6. Harrell's c-index, ROC/AUC(95%CI, time fix): Internal/External validation
################################################################################

## ROC/AUC(95%CI, time fix)

## internal set
out.train.naomit <- out.train[complete.cases(out.train[, ..significant_vars])]
out.train.naomit$risk_score <- predict(new.model.cox, type = "lp")
#saveRDS(out.train.naomit, file = "out_train_naomit.rds")
out.train.naomit <- readRDS("out_train_naomit.rds")

time.roc.train <- timeROC(T=out.train.naomit$ESRD_day, delta=out.train.naomit$ESRD,
                          marker=out.train.naomit$risk_score,
                          cause=1,weighting="marginal",
                          times=c(365, 730, 1095),
                          iid=TRUE)
time.roc.train

## external set
out.val <- copy(out[Type == "val"])[, ESRD := as.integer(as.character(ESRD))][]
out.val$risk_score <- predict(new.model.cox, newdata = out.val, type = "lp")
#saveRDS(out.val, file = "out_val.rds")
out.val <- readRDS("out_val.rds")

time.roc.val <- timeROC(T=out.val$ESRD_day, delta=out.val$ESRD,
                        marker=out.val$risk_score,
                        cause=1, weighting="marginal",
                        times=c(365, 730, 1095),
                        iid=TRUE)

time.roc.val


## AUC curve plotting

doc <- read_pptx()

times <- c(365, 730, 1095)

for (t in times) {
  auc_train <- round(time.roc.train$AUC[time.roc.train$times == t], 3)
  auc_val   <- round(time.roc.val$AUC[time.roc.val$times == t], 3)

  dml_plot <- dml(code = {
    par(mfrow = c(1, 2))

    plot(time.roc.train, time = t, lwd = 2, title = FALSE)
    title(main = paste0("Train - Time: ", t, " days\nAUC = ", auc_train))

    plot(time.roc.val, time = t, lwd = 2, title = FALSE)
    title(main = paste0("Validation - Time: ", t, " days\nAUC = ", auc_val))
  })

  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = paste0("ROC Curve - ", t, " days"), location = ph_location_type(type = "title"))
  doc <- ph_with(doc, dml_plot, location = ph_location_type(type = "body"))
}

print(doc, target = "ROC_Curves.pptx")


## Harrell's c-index with train and validate set

## train
model.cidx.train <- concordance(new.model.cox)
model.cidx.train.step <- concordance(new.model.cox.step)
#summary(new.model.cox)$concordance

## validate

# surv.test <- Surv(out.val$ESRD_day, out.val$ESRD)
# concordance(surv.test ~ out.val$risk_score, reverse=TRUE)
#concordance(Surv(out.val$ESRD_day, out.val$ESRD) ~ out.val$risk_score, reverse=TRUE)
model.cidx.val<- concordance(new.model.cox, newdata = out.val)
model.cidx.val.step <- concordance(new.model.cox.step, newdata = out.val)

contr <- c(-1, 1)

## base function
# ci <- coef(model.cidx.val) + c(-1, 1) * sqrt(model.cidx.val$var) * qnorm(1-0.05/2)
# paste0(round(model.cidx.val$concordance,3), paste0( " (", paste0( round(ci[1],3), ","), round(ci[2],3) ), ")")

## train model
ci.train <- coef(model.cidx.train) + c(-1, 1) * sqrt(model.cidx.train$var) * qnorm(1-0.05/2)
paste0(round(model.cidx.train$concordance,3), paste0( " (", paste0( round(ci.train[1],3), ","), round(ci.train[2],3) ), ")")

## validate model
ci.val <- coef(model.cidx.val) + c(-1, 1) * sqrt(model.cidx.val$var) * qnorm(1-0.05/2)
paste0(round(model.cidx.val$concordance,3), paste0( " (", paste0( round(ci.val[1],3), ","), round(ci.val[2],3) ), ")")

## stepwise train model
step.ci.train <- coef(model.cidx.train.step) + c(-1, 1) * sqrt(model.cidx.train.step$var) * qnorm(1-0.05/2)
paste0(round(model.cidx.train.step$concordance,3), paste0( " (", paste0( round(step.ci.train[1],3), ","), round(step.ci.train[2],3) ), ")")


## stepwise validate model
step.ci.val <- coef(model.cidx.val.step) + c(-1, 1) * sqrt(model.cidx.val.step$var) * qnorm(1-0.05/2)
paste0(round(model.cidx.val.step$concordance,3), paste0( " (", paste0( round(step.ci.val[1],3), ","), round(step.ci.val[2],3) ), ")")

base_model_ci <- rbind(paste0(round(model.cidx.train$concordance,3), paste0( " (", paste0( round(ci.train[1],3), ","), round(ci.train[2],3) ), ")"),
                       paste0(round(model.cidx.val$concordance,3), paste0( " (", paste0( round(ci.val[1],3), ","), round(ci.val[2],3) ), ")"))
step_model_ci <- rbind(paste0(round(model.cidx.train.step$concordance,3), paste0( " (", paste0( round(step.ci.train[1],3), ","), round(step.ci.train[2],3) ), ")"),
                       paste0(round(model.cidx.val.step$concordance,3), paste0( " (", paste0( round(step.ci.val[1],3), ","), round(step.ci.val[2],3) ), ")"))

out_ci <- cbind(c("train","validate"), base_model_ci, step_model_ci)
rownames(out_ci) <- c("train","validate")
colnames(out_ci) <- c("model", "base model (95%CI)", "stepwise model (95%CI)")
out_ci

## add C-index and CI in xlsx file

wb <- loadWorkbook("cox_table.xlsx")
addWorksheet(wb, "Harrell's c-index")
writeData(wb, "Harrell's c-index", out_ci)
saveWorkbook(wb, "cox_table.xlsx", overwrite = TRUE)






