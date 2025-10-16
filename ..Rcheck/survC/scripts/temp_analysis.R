#install.packages("timeROC")
#remotes::install_github("jinseob2kim/jstable")
library(data.table);library(magrittr);library(readxl);library(openxlsx)
library(jstable); library(survival); library(MASS); library(timeROC); library(ggplot2)
library(officer); library(rvg)
setwd("~/ShinyApps/koges_sshnam/ADPKD2025")

################################################################################
# Part 1. New data (2025.9.4 change)
################################################################################

a <- read_excel("Q612_N185 데이터추출 요청_고혈압수정.xlsx", skip = 1, sheet = 1) %>% data.table()

new_data <- read_excel("Q612_N185_사망_최종진료일자_추가검사.xlsx", sheet = 1) %>% data.table()

a <- merge(a, new_data[, .(환자번호 = 등록번호, LastFUdate = pmin(사망일자, 최종진료일자, na.rm = T), SUPCR = 검사결과)],
           by = "환자번호", all.x = T)

#cbind(names(a), names(new_data))

col <- c("Hemoglobin" = "HB",
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
         "Creatinine (Urine)" = "Creatinine (Urine)")

a %>% setnames(old = "환자번호", new = "SUBJ_ID")

a %>% setnames(old = grep("^Q612최초진단일자", names(.), value = TRUE)[1], new = "VISIT_DT")
a[, (grep("^Q612최초진단일자", names(a), value = TRUE)) := NULL]

a %>% setnames(old = grep("^성별", names(.), value = TRUE)[1], new = "SEX")
a[, (grep("^성별", names(a), value = TRUE)) := NULL]
a[, SEX := fifelse(SEX == "M", 1, 2)]

a[, AGE := (as.Date(VISIT_DT, format = "%Y-%m-%d") - as.Date(생년월일)) / 365.25]

a[, VISIT_NM := "Baseline"]

a[, BMI := 몸무게 / (키 / 100) ^ 2]

#a %>% setnames(old = "고혈압유무(I10~I15코드유무)", new = "HTN")
a %>% setnames(old = "고혈압약제 복용유무", new = "HTN")
a[, HTN := fifelse(HTN == "Y", 1, 0)]

a %>% setnames(old = "당뇨유무(E11코드유무)", new = "DM")
a[, DM := fifelse(DM == "Y", 1, 0)]

a %>% setnames(old = "N185등록여부( 0/1)", new = "ESRD")

a %>% setnames(old = "N185최초진단일자", new = "RRTDST")

# Might have to change follow up later
a[, ESRD_day := fifelse(
  is.na(RRTDST),
  as.Date("2025-08-01") - as.Date(VISIT_DT, format = "%Y-%m-%d"),
  as.Date(RRTDST, format = "%Y-%m-%d") - as.Date(VISIT_DT, format = "%Y-%m-%d")
)]

a[, "Laboratory Data" := NULL]
a[, (grep("^검사일자", names(a), value = TRUE)) := NULL]
a %>% setnames(old = grep("^검사결과", names(.), value = TRUE),
               new = names(col))
a[, (names(col)) := lapply(.SD, as.numeric), .SDcols = names(col)]

a %>% setnames(old = names(col), new = unname(col))

#a[, SUPCR := `protein (urine)` / `Creatinine (Urine)`]

a[, SCCGFR := ((140 - AGE) * 몸무게) / (72 * SCR) *
    fifelse(SEX == 2, 0.85, 1)]

a[, CKDGFR := 142 *
    (pmin(SCR / fifelse(SEX==2, 0.7, 0.9), 1) ^ fifelse(SEX==2, -0.241, -0.302)) *
    (pmax(SCR / fifelse(SEX==2, 0.7, 0.9), 1) ^ -1.2) *
    (0.9938 ^ as.numeric(AGE)) * fifelse(SEX==2, 1.012, 1)]


a[, date_only := as.Date(fifelse(ESRD %in% "0", LastFUdate, RRTDST))]

a[, ESRD_day := as.integer(as.Date(date_only, format = "%Y-%m-%d") - as.Date(VISIT_DT, format = "%Y-%m-%d"))]

a[, SUPCR := fifelse(
  grepl("neg", SUPCR, ignore.case = TRUE) | grepl("Trace", SUPCR, ignore.case = TRUE),
  "neg",
  fifelse(is.na(SUPCR), NA_character_, "pos")
)]


################################################################################
# Part 2.train & test split
################################################################################

# remove NA values
#sum(is.na(new_a$ESRD_day))

set.seed(123456)
n <- nrow(a)
train_size = 300
train_indices <- sample(n, size = train_size)

a[, Type := fifelse(순번 %in% train_indices, "train", "val")]


# sum(new_a$Type == 'train')
# sum(new_a$Type == 'val')


################################################################################
# Part 3. varlist & saving & importing & labeling
################################################################################
varlist <- list(
  Base = c("Type", "SEX", "AGE", "HTN", "DM", "BMI"),
  Lab = c("UA", "SUPCR", "CKDGFR", "HB", "FSG",
          "LBCA", "P", "Sodium", "K", "CL"),
  Outcome = c("ESRD"),
  Time = c("ESRD_day")
)

out <- a[!is.na(ESRD_day), .SD, .SDcols = c("SUBJ_ID", unlist(varlist))]

factor_vars <- names(out)[sapply(out, function(x){length(table(x))}) <= 8]
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

conti_vars <- setdiff(names(out), c("SUBJ_ID", factor_vars))

out[, (conti_vars) := lapply(.SD, as.numeric), .SDcols = conti_vars]

## save RDS
#saveRDS(out, file = "out.rds")

## import RDS
out <- readRDS("out.rds")

#out$Subject <- factor(out$Subject)
out.label <- jstable::mk.lev(out)

out.label[variable == "SEX", `:=`(val_label = c("Male", "Female"))]
out.label[variable == "HTN", `:=`(val_label = c("No", "Yes"))]
out.label[variable == "DM", `:=`(val_label = c("No", "Yes"))]



################################################################################
# Part 4. Table 1
################################################################################
tb1 <- CreateTableOneJS(vars = c(varlist$Base[-1], varlist$Lab, varlist$Outcome, varlist$Time), strata = "Type",
                        data = out, labeldata = out.label, Labels = T, nonnormal = "ESRD_day",
                        showAllLevels = F)$table
# tb1
write.xlsx(tb1, file = "tb1.xlsx", rowNames = T)

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

## lapply
# tb.cinx <- lapply (c(model.cidx.train, model.cidx.val), function(x){
#   #print(x)
#   #print(x[4])
#   ci <- x[1][[1]] + c(-1, 1) * sqrt(x[[4]]) * qnorm(1 - 0.05 / 2)
#   paste0(round(x[1], 3)," (", paste0(round(ci[1], 3), ", "), round(ci[2], 3), ")" )
# })





