# --- Packages ---
#pkgs <- c("readxl","ggplot2","car","MASS","dplyr","lmtest","sandwich")
#to_install <- pkgs[!pkgs %in% installed.packages()[,1]]
#if(length(to_install)) install.packages(to_install, dependencies = TRUE)
#invisible(lapply(pkgs, library, character.only = TRUE))


library(readxl)
library(ggplot2)
library(MASS)
library(dplyr)
library(lmtest)
library(car)
library(lmtest)
library(sandwich)

# --- Load data ---
# CHANGE THIS TO YOUR PATH
dat_path <- "C:/Users/User/Downloads/Sales_data.xlsx"
homes <- read_excel(dat_path)

# --- Basic typing ---
homes <- homes %>%
  mutate(
    Air_conditioning   = factor(Air_cond, levels = c(0,1), labels = c("No","Yes")),
    Pool               = factor(Pool,     levels = c(0,1), labels = c("No","Yes")),
    Adjacent_to_highway= factor(Highway,  levels = c(0,1), labels = c("No","Yes")),
    Quality            = factor(Quality,  levels = c(1,2,3), labels = c("High","Medium","Low")),
    Style              = factor(Style),
    Age                = 2002 - Year_built,
    Sales_price=log(Sales_price)
  )

homes
# --- Collapse rare levels (<5%) for categorical vars ---
consolidate_rare_levels <- function(data, variable, threshold = 0.05){
  freq <- table(data[[variable]])
  rare <- names(freq)[freq < nrow(data) * threshold]
  if(length(rare)){
    data[[variable]] <- factor(ifelse(as.character(data[[variable]]) %in% rare, "Other",
                                      as.character(data[[variable]])))
  }
  data
}

factor_vars <- c("Air_conditioning","Pool","Adjacent_to_highway","Style","Quality")
for (v in factor_vars) homes <- consolidate_rare_levels(homes, v)

v
# --- Train / Test split ---
set.seed(123)
idx   <- sample(seq_len(nrow(homes)), size = 0.7 * nrow(homes))
train <- homes[idx, ]
test  <- homes[-idx, ]

# Align factor levels in test to training
for (v in names(train)) {
  if (is.factor(train[[v]])) {
    test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
  }
}

# Set reference level for Quality (low as baseline)
if("Low" %in% levels(train$Quality)) train$Quality <- relevel(train$Quality, ref = "Low")

# --- Full model ---
full_formula <- Sales_price ~ Finished_square_feet + Bedrooms + Bathrooms +
  Air_conditioning + Garage_size + Pool + Age + Quality + Style + Lot_size +
  Adjacent_to_highway

full_model  <- lm(full_formula, data = train)

# --- Stepwise AIC selection ---
final_model <- stepAIC(full_model, direction = "both", trace = FALSE)

# --- Summary & VIF ---
model_summary <- summary(final_model)
vif_values    <- vif(final_model)

print(model_summary)
print(vif_values)

# --- Diagnostics (panel) ---
png("model_diagnostics.png", width = 1200, height = 900)
par(mfrow = c(2,2))
plot(final_model)
dev.off()
par(mfrow = c(1,1))

# --- Assumption tests ---
# Shapiro–Wilk (note: large n -> sensitive)
shapiro_test <- shapiro.test(resid(final_model))
print(shapiro_test)

# Breusch–Pagan heteroskedasticity test
bp_test <- bptest(final_model)
print(bp_test)

# Robust SEs (if needed for inference)
robust_coefs <- lmtest::coeftest(final_model, vcov = sandwich::vcovHC(final_model, type = "HC1"))
print(robust_coefs)

# --- Predict on test & metrics ---
pred <- predict(final_model, newdata = test)
rmse <- sqrt(mean((test$Sales_price - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test$Sales_price - pred), na.rm = TRUE)
r2   <- cor(test$Sales_price, pred, use = "complete.obs")^2

# Naive baseline: predict training mean
baseline <- mean(train$Sales_price, na.rm = TRUE)
rmse_base <- sqrt(mean((test$Sales_price - baseline)^2, na.rm = TRUE))

cat("\nTest Performance:\n",
    "RMSE:", round(rmse, 2), "\n",
    "MAE:", round(mae, 2), "\n",
    "R-squared:", round(r2, 4), "\n",
    "Baseline RMSE (mean-only):", round(rmse_base, 2), "\n")

# --- Plots for report ---
# 1) Actual vs Predicted
ggplot(data.frame(Actual = test$Sales_price, Predicted = pred),
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Figure 4. Actual vs Predicted (Test)",
       x = "Actual Sale Price", y = "Predicted Sale Price") +
  theme_minimal()
ggsave("fig_actual_vs_pred.png", width = 7, height = 5, dpi = 300)

# 2) Residuals vs Fitted (train)
res_df <- data.frame(Fitted = fitted(final_model),
                     Residuals = resid(final_model))
ggplot(res_df, aes(Fitted, Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Figure 1. Residuals vs Fitted", x = "Fitted", y = "Residuals") +
  theme_minimal()
ggsave("fig_resid_vs_fitted.png", width = 7, height = 5, dpi = 300)

# 3) Residual distribution
ggplot(data.frame(Residuals = resid(final_model)), aes(Residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black") +
  geom_density() +
  labs(title = "Figure 5. Residual Distribution", x = "Residuals", y = "Density") +
  theme_minimal()
ggsave("fig_resid_dist.png", width = 7, height = 5, dpi = 300)

# 4) Coefficient magnitudes (absolute)
coef_df <- data.frame(Variable = names(coef(final_model)),
                      Importance = abs(coef(final_model)))
coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]
coef_df <- coef_df[order(coef_df$Importance, decreasing = TRUE), ]
ggplot(coef_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() + coord_flip() +
  labs(title = "Figure 6. Absolute Coefficient Magnitudes",
       x = "Variables", y = "Absolute Coefficient") +
  theme_minimal()
ggsave("fig_coef_importance.png", width = 7, height = 5, dpi = 300)

# --- Export key outputs for Appendix ---
sink("model_summary.txt")
print(model_summary)
cat("\nVIF Values:\n"); print(vif_values)
cat("\nShapiro–Wilk:\n"); print(shapiro_test)
cat("\nBreusch–Pagan:\n"); print(bp_test)
cat("\nRobust Coefficients (HC1):\n"); print(robust_coefs)
cat("\nTest Set Performance:\n")
cat("RMSE:", round(rmse, 2), "\n",
    "MAE:", round(mae, 2), "\n",
    "R-squared:", round(r2, 4), "\n",
    "Baseline RMSE:", round(rmse_base, 2), "\n")
sink()
