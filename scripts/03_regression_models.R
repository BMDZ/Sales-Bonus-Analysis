# ================================================
# SCRIPT 03: CAUSAL ANALYSIS - REGRESSION MODELS
# ================================================
# Purpose: Fit 3-stage regression models for causal inference
# Output: Coefficients, p-values, model comparisons
# Runtime: ~5 seconds

library(dplyr)
library(broom)

# Load clean data
sbd <- read.csv("sbd_clean.csv")

cat("═══════════════════════════════════════\n")
cat("CAUSAL ANALYSIS: REGRESSION MODELS\n")
cat("═══════════════════════════════════════\n\n")

# ==================== MODEL 1: RAW EFFECT ====================
cat("MODEL 1: Raw Bonus Effect (Unadjusted)\n")
cat("─────────────────────────────────────\n")
cat("Formula: Margin ~ Time\n")
cat("Interpretation: Total effect of bonus on profit margins\n\n")

reg1 <- lm(sales_profitmargin ~ time, data = sbd)
coef_reg1 <- tidy(reg1)

print(coef_reg1)

r2_reg1 <- summary(reg1)$r.squared
adj_r2_reg1 <- summary(reg1)$adj.r.squared

cat("\nModel 1 Summary:\n")
cat("R²:", round(r2_reg1, 4), "\n")
cat("Adjusted R²:", round(adj_r2_reg1, 4), "\n")
cat("N:", nrow(sbd), "\n\n")

# Extract key values
raw_effect <- round(coef_reg1$estimate[coef_reg1$term == "time"], 3)
raw_pval <- coef_reg1$p.value[coef_reg1$term == "time"]

cat("KEY FINDING - Raw Bonus Effect:", raw_effect, "pp")
cat(ifelse(raw_pval < 0.001, " ***", 
           ifelse(raw_pval < 0.01, " **", 
                  ifelse(raw_pval < 0.05, " *", " (ns)"))), "\n\n")

# ==================== MODEL 2: CONTROLLED FOR PRODUCT MIX ====================
cat("─────────────────────────────────────\n")
cat("MODEL 2: Controlled for Product Mix\n")
cat("─────────────────────────────────────\n")
cat("Formula: Margin ~ Time + Device + Insurance + Accessory\n")
cat("Interpretation: Direct bonus effect after removing product mix confounding\n\n")

reg2 <- lm(sales_profitmargin ~ time + device + insurance + accessory, data = sbd)
coef_reg2 <- tidy(reg2)

print(coef_reg2)

r2_reg2 <- summary(reg2)$r.squared
adj_r2_reg2 <- summary(reg2)$adj.r.squared

cat("\nModel 2 Summary:\n")
cat("R²:", round(r2_reg2, 4), "\n")
cat("Adjusted R²:", round(adj_r2_reg2, 4), "\n")
cat("N:", nrow(sbd), "\n\n")

# Extract key values
direct_effect <- round(coef_reg2$estimate[coef_reg2$term == "time"], 3)
direct_pval <- coef_reg2$p.value[coef_reg2$term == "time"]

cat("KEY FINDING - Direct Bonus Effect:", direct_effect, "pp")
cat(ifelse(direct_pval < 0.001, " ***", 
           ifelse(direct_pval < 0.01, " **", 
                  ifelse(direct_pval < 0.05, " *", " (ns)"))), "\n\n")

# ==================== MODEL 3: STORE FIXED EFFECTS ====================
cat("─────────────────────────────────────\n")
cat("MODEL 3: Store Fixed Effects\n")
cat("─────────────────────────────────────\n")
cat("Formula: Margin ~ Time + Device + Insurance + Accessory + Store\n")
cat("Interpretation: Direct effect controlling for store-specific differences\n\n")

reg3 <- lm(sales_profitmargin ~ time + device + insurance + accessory + factor(store), data = sbd)
coef_reg3 <- tidy(reg3)

# Display first 6 coefficients (to avoid clutter with store dummies)
print(coef_reg3[1:6, ])
cat("... [", nrow(coef_reg3) - 6, " store fixed effect coefficients omitted]\n\n")

r2_reg3 <- summary(reg3)$r.squared
adj_r2_reg3 <- summary(reg3)$adj.r.squared

cat("Model 3 Summary:\n")
cat("R²:", round(r2_reg3, 4), "\n")
cat("Adjusted R²:", round(adj_r2_reg3, 4), "\n")
cat("N:", nrow(sbd), "\n")
cat("Number of stores:", n_distinct(sbd$store), "\n\n")

# Extract key values
fe_effect <- round(coef_reg3$estimate[coef_reg3$term == "time"], 3)
fe_pval <- coef_reg3$p.value[coef_reg3$term == "time"]

cat("KEY FINDING - Effect (with store FE):", fe_effect, "pp")
cat(ifelse(fe_pval < 0.001, " ***", 
           ifelse(fe_pval < 0.01, " **", 
                  ifelse(fe_pval < 0.05, " *", " (ns)"))), "\n\n")

# ==================== MODEL COMPARISON ====================
cat("═════════════════════════════════════\n")
cat("MODEL COMPARISON & EFFECT ATTENUATION\n")
cat("═════════════════════════════════════\n\n")

comparison <- data.frame(
  Model = c("Model 1 (Raw)", "Model 2 (Product Controls)", "Model 3 (Store FE)"),
  Coefficient = c(raw_effect, direct_effect, fe_effect),
  R_Squared = round(c(r2_reg1, r2_reg2, r2_reg3), 4),
  Adj_R2 = round(c(adj_r2_reg1, adj_r2_reg2, adj_r2_reg3), 4),
  P_Value = c(raw_pval, direct_pval, fe_pval)
)

print(comparison)

# Calculate attenuation
attenuation_abs <- raw_effect - direct_effect
attenuation_pct <- (attenuation_abs / raw_effect) * 100

cat("\n")
cat("Attenuation from Model 1 → Model 2:\n")
cat("  Absolute:", round(attenuation_abs, 3), "pp\n")
cat("  Percentage:", round(attenuation_pct, 1), "%\n")
cat("  → Interpretation: Product mix accounts for", round(attenuation_pct, 1), 
    "% of the bonus effect\n\n")

# ==================== SAVE RESULTS ====================
# Create comprehensive coefficient table - use base R to avoid select() conflict
coef_table <- rbind(
  coef_reg1 %>% mutate(Model = "Model 1: Raw Effect"),
  coef_reg2 %>% mutate(Model = "Model 2: Product Controls"),
  coef_reg3[1:5, ] %>% mutate(Model = "Model 3: Store FE (first 5 terms)")
)

# Keep only needed columns using base R
coef_table <- coef_table[, c("Model", "term", "estimate", "std.error", "statistic", "p.value")]

# Round values
coef_table$estimate <- round(coef_table$estimate, 4)
coef_table$std.error <- round(coef_table$std.error, 4)
coef_table$statistic <- round(coef_table$statistic, 3)
coef_table$p.value <- round(coef_table$p.value, 4)

write.csv(coef_table, "03_regression_coefficients.csv", row.names = FALSE)

# Model comparison table
write.csv(comparison, "03_model_comparison.csv", row.names = FALSE)

cat("✓ Saved: 03_regression_coefficients.csv\n")
cat("✓ Saved: 03_model_comparison.csv\n\n")

# ==================== ASSUMPTIONS TESTING ====================
cat("═════════════════════════════════════\n")
cat("MODEL DIAGNOSTIC CHECKS\n")
cat("═════════════════════════════════════\n\n")

# Test normality of residuals (Model 2)
residuals_model2 <- resid(reg2)
shapiro_test <- shapiro.test(residuals_model2)

cat("Normality of Residuals (Shapiro-Wilk Test):\n")
cat("  W-statistic:", round(shapiro_test$statistic, 4), "\n")
cat("  P-value:", round(shapiro_test$p.value, 4), "\n")
cat("  Interpretation:", 
    ifelse(shapiro_test$p.value > 0.05, 
           "✓ Residuals appear normal (p > 0.05)", 
           "⚠️ Residuals may not be normal (p < 0.05)"), "\n\n")

# Heteroskedasticity check (Breusch-Pagan)
# Manual calculation
model2_fitted <- fitted(reg2)
model2_resid_sq <- residuals(reg2)^2

bp_test <- lm(model2_resid_sq ~ model2_fitted)
bp_rsq <- summary(bp_test)$r.squared
bp_stat <- nrow(sbd) * bp_rsq

cat("Heteroskedasticity (Breusch-Pagan Test - approx):\n")
cat("  Test statistic:", round(bp_stat, 4), "\n")
cat("  Interpretation:", 
    ifelse(bp_stat < 3.84, 
           "✓ No strong evidence of heteroskedasticity", 
           "⚠️ Possible heteroskedasticity detected"), "\n\n")

# ==================== FINAL SUMMARY ====================
cat("═════════════════════════════════════\n")
cat("CAUSAL ANALYSIS SUMMARY\n")
cat("═════════════════════════════════════\n\n")

cat("Model 1 (Raw Effect):\n")
cat("  Coefficient:", raw_effect, "pp")
cat(ifelse(raw_pval < 0.001, " ***", 
           ifelse(raw_pval < 0.01, " **", 
                  ifelse(raw_pval < 0.05, " *", " (ns)"))), "\n")
cat("  R²:", round(r2_reg1, 4), "\n")
cat("  Interpretation: Total bonus effect on profit margins\n\n")

cat("Model 2 (Product Controlled):\n")
cat("  Coefficient:", direct_effect, "pp")
cat(ifelse(direct_pval < 0.001, " ***", 
           ifelse(direct_pval < 0.01, " **", 
                  ifelse(direct_pval < 0.05, " *", " (ns)"))), "\n")
cat("  R²:", round(r2_reg2, 4), "\n")
cat("  Interpretation: Direct margin improvement per product\n\n")

cat("Model 3 (Store Fixed Effects):\n")
cat("  Coefficient:", fe_effect, "pp")
cat(ifelse(fe_pval < 0.001, " ***", 
           ifelse(fe_pval < 0.01, " **", 
                  ifelse(fe_pval < 0.05, " *", " (ns)"))), "\n")
cat("  R²:", round(r2_reg3, 4), "\n")
cat("  Interpretation: Effect controlling for store differences\n\n")

cat("Mediation Analysis:\n")
cat("  Indirect effect:", round(attenuation_abs, 3), "pp\n")
cat("  Mediation %:", round(attenuation_pct, 1), "%\n")
cat("  Interpretation:", round(attenuation_pct, 1), 
    "% of bonus effect works through product mix\n\n")

cat("Product-Specific Effects (from Model 2):\n")
insurance_coef <- round(coef_reg2$estimate[coef_reg2$term == "insurance"], 3)
accessory_coef <- round(coef_reg2$estimate[coef_reg2$term == "accessory"], 3)
device_coef <- round(coef_reg2$estimate[coef_reg2$term == "device"], 3)
cat("  Device (baseline):\n")
cat("  Insurance vs Device:", insurance_coef, "pp higher\n")
cat("  Accessory vs Device:", accessory_coef, "pp higher\n\n")

cat("✓ Causal analysis complete!\n")
cat("✓ Ready for Script 04: Mediation Analysis\n")

# ==================== SESSION INFO ====================
cat("\n═════════════════════════════════════\n")
cat("SESSION INFO\n")
cat("═════════════════════════════════════\n")
cat("Script:", "03_causal_analysis.R\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version$version.string, "\n")

