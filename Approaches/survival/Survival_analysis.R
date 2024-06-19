# Load necessary libraries
install.packages("survival")
install.packages("survminer")
install.packages("survcomp")
library(survminer)
library(survival)
library(ggplot2)
library(rms)
library(MASS)

# Read the data 
data <- read.csv("Downloads/survival_data.csv")
str(data)

# Data Preparation
data$Recurrence.Free.Status <- ifelse(data$Recurrence.Free.Status == "Recurrence", 1, 0)

# Data Visualization
boxplot(data$Recurrence.Free.Status..Month., main = "Boxplot of RS (Month)", ylab = "Values")
hist(data$Recurrence.Free.Status..Month., main = "Histogram of RS (Month)", xlab = "Values")

status_counts <- table(data$Recurrence.Free.Status)
barplot(status_counts, main = "Distribution of Recurence", xlab = "Status", ylab = "Frequency", 
        names.arg = c("Not Recurred", "Recurred"), col = c("lightblue", "blue"))

##__________________________________Kaplan-Meier Curves___________________________________________
# Survival object
surv_obj <- Surv(time = data$Recurrence.Free.Status..Month. , event = data$Recurrence.Free.Status)

# Kaplan-Meier survival curve
km_fit <- survfit(surv_obj ~ 1)
ggsurvplot(km_fit, data = data, conf.int = FALSE, palette = "blue", risk.table = TRUE,  surv.median.line = "hv",
           title = "Kaplan-Meier Recurrence-Free Survival Curve", xlab = "Time (Months)", ylab = "Recurrence-Free Survival Probability")

# Kaplan-Meier survival curve against Menopausal.Status covariant
fit <- survfit(surv_obj ~ data$Menopausal.Status)
ggsurvplot(fit, data = data, palette = "Menopausal.Status" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by Menopausal Status")

# Kaplan-Meier survival curve against Tumor.Size covariant
fit <- survfit(surv_obj ~ data$Tumor.Size)
ggsurvplot(fit, data = data, palette = "Tumor.Size" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by Tumor Size")

# Kaplan-Meier survival curve against Lymph.Node.Status covariant
fit <- survfit(surv_obj ~ data$Lymph.Node.Status)
ggsurvplot(fit, data = data, palette = "Lymph.Node.Status" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by Lymph Node Status")

# Kaplan-Meier survival curve against Tumor.Grade covariant
fit <- survfit(surv_obj ~ data$Tumor.Grade)
ggsurvplot(fit, data = data, palette = "Tumor.Grade" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by Tumor Grade")

# Kaplan-Meier survival curve against Mol.Subtype covariant
fit <- survfit(surv_obj ~ data$Mol.Subtype)
ggsurvplot(fit, data = data, palette = "Mol.Subtype" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by Mol Subtype")

# Kaplan-Meier survival curve against Histological.Type covariant
fit <- survfit(surv_obj ~ data$Histological.Type)
ggsurvplot(fit, data = data, palette = "Histological.Type" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by Histological Type")

# Kaplan-Meier survival curve against ER covariant
fit <- survfit(surv_obj ~ data$ER)
ggsurvplot(fit, data = data, palette = "ER" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by ER")

# Kaplan-Meier survival curve against PR covariant
fit <- survfit(surv_obj ~ data$PR)
ggsurvplot(fit, data = data, palette = "PR" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by PR")

# Kaplan-Meier survival curve against HER2 covariant
fit <- survfit(surv_obj ~ data$HER2)
ggsurvplot(fit, data = data, palette = "HER2" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by HER2")

# Kaplan-Meier survival curve against Tumor.Location covariant
fit <- survfit(surv_obj ~ data$Tumor.Location)
ggsurvplot(fit, data = data, palette = "Tumor.Location" ,pval = TRUE, conf.int = FALSE, title = "Kaplan-Meier Survival Curves by Tumor Location")


##__________________________________Log-rank tests___________________________________________

# Define a function to perform survdiff analysis
perform_survdiff <- function(covariate) {
  formula <- as.formula(paste("surv_obj ~", covariate))
  result <- survdiff(formula, data = data)
  print(result)
}

# List of covariates
covariates <- c("Menopausal.Status", "Tumor.Size", "Lymph.Node.Status", "Tumor.Grade", 
                "Mol.Subtype", "Histological.Type", "ER", "PR", "HER2", "Tumor.Location")

# Loop through the covariates and perform survdiff analysis
for (covariate in covariates) {
  perform_survdiff(covariate)
}

##__________________________________Cox univariant___________________________________________

# Define a function to fit and summarize Cox models
fit_and_summarize_cox <- function(covariate) {
  formula <- as.formula(paste("Surv(Recurrence.Free.Status..Month., Recurrence.Free.Status) ~", covariate))
  cox_model <- coxph(formula, data = data, x = TRUE, y = TRUE)
  summary_cox <- summary(cox_model)
  print(summary_cox)
}

# List of covariates
covariates <- c("Menopausal.Status", "Tumor.Size", "Lymph.Node.Status", "Tumor.Grade", 
                "Mol.Subtype", "Histological.Type", "ER", "PR", "HER2", "Tumor.Location", 
                "Overall.Survival.Status..Month.")

# Loop through the covariates and fit Cox models
for (covariate in covariates) {
  fit_and_summarize_cox(covariate)
}

##__________________________________Cox multivariant___________________________________________

cox_model <- coxph(Surv(Recurrence.Free.Status..Month., Recurrence.Free.Status) ~ . , data = data, x = TRUE, y = TRUE)
summary(cox_model)

# Backward Feature selection
cox_backward <- coxph(Surv(Recurrence.Free.Status..Month., Recurrence.Free.Status) ~ . , data = data, x = TRUE, y = TRUE)
cox_backward_step <- stepAIC(cox_backward, direction = "backward", trace = 0)
summary(cox_backward_step)

# Step wise feature selection (Forword & Backword)
cox_backward <- coxph(Surv(Recurrence.Free.Status..Month., Recurrence.Free.Status) ~ . , data = data, x = TRUE, y = TRUE)
cox_backward_step <- stepAIC(cox_backward, direction = "both", trace = 0)
summary(cox_backward_step)

##__________________________________Hazard Forest Plot___________________________________________

# Load necessary packages
install.packages("forestplot")
library(forestplot)

# Calculate summary of the Cox model
summary_cox <- summary(cox_model)

# Extract significant variables (p-value < 0.05)
significant_vars <- summary_cox$coefficients[summary_cox$coefficients[, "Pr(>|z|)"] < 0.05, ]

# Extract hazard ratios and confidence intervals
hr <- exp(significant_vars[, "coef"])
ci_lower <- exp(significant_vars[, "coef"] - 1.96 * significant_vars[, "se(coef)"])
ci_upper <- exp(significant_vars[, "coef"] + 1.96 * significant_vars[, "se(coef)"])
variables <- rownames(significant_vars)

# Create a data frame for plotting
forest_data <- data.frame(
  Variable = variables,
  HR = hr,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Prepare the data frame for labeltext
labeltext <- cbind(
  Variable = forest_data$Variable,
  HR = sprintf("%.2f", forest_data$HR),
  CI = paste0(sprintf("%.2f", forest_data$CI_Lower), " - ", sprintf("%.2f", forest_data$CI_Upper))
)

# Create forest plot
forestplot(labeltext = labeltext,
           mean = log(forest_data$HR),
           lower = log(forest_data$CI_Lower),
           upper = log(forest_data$CI_Upper),
           graphwidth = unit(10, "cm"),
           col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"),
           is.summary = rep(FALSE, nrow(forest_data)),  # Set all covariates as individual, not summary
           xlab = "Hazard Ratio (log scale)",
           txt_gp = fpTxtGp(label = gpar(fontsize = 12)),
           new_page = TRUE)

# Create the forest plot
ggplot(forest_data, aes(x = Variable, y = HR, ymin = CI_Lower, ymax = CI_Upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Forest Plot of Significant Hazard Ratios", y = "Hazard Ratio", x = "Variable") +
  theme_minimal()


##___________________________________Nomogram___________________________________________
label(data$Menopausal.Status) <- "Menopausal Status"
label(data$Tumor.Size) <- "Tumor Size"
label(data$Lymph.Node.Status) <- "Lymph Node Status"
label(data$Tumor.Grade) <- "Tumor Grade"
label(data$Mol.Subtype) <- "Molecular Subtype"
label(data$HER2) <- "HER2" 

# Define the data distribution object
dd <- datadist(data)
options(datadist = "dd")

MultiCox.nomogram <- cph(Surv(Recurrence.Free.Status..Month., Recurrence.Free.Status) ~ 
                           Menopausal.Status + Tumor.Size +Lymph.Node.Status + Tumor.Grade 
                         + Mol.Subtype + HER2, data = data, x = TRUE, y = TRUE, surv = TRUE)


surv <- Survival(MultiCox.nomogram)
surv1 <- function(x) surv(5 * 12, lp = x)

# Create the nomogram with custom labels applied through the datadist object
nom <- nomogram(MultiCox.nomogram, fun = list(surv1), lp = FALSE,  funlabel = c("5-year RFS"), 
                maxscale = 10, fun.at = c(0.95, 0.9, 0.8, 0.6, 0.4, 0.2, 0.05))

# Plot the nomogram with adjusted layout
plot(nom)


