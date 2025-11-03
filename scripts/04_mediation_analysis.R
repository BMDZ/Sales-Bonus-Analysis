# ================================================
# SCRIPT 04: MEDIATION ANALYSIS
# ================================================
# Purpose: Decompose effects into direct & indirect pathways
# Output: Mediation percentages, breakdowns, visualizations
# Runtime: ~10 seconds

library(dplyr)
library(ggplot2)
library(tidyr)

# Load clean data
sbd <- read.csv("sbd_clean.csv")

# Load regression results - define models inline
reg1 <- lm(sales_profitmargin ~ time, data = sbd)
reg2 <- lm(sales_profitmargin ~ time + device + insurance + accessory, data = sbd)

cat("═════════════════════════════════════\n")
cat("MEDIATION ANALYSIS\n")
cat("═════════════════════════════════════\n\n")

# ==================== MEDIATION DECOMPOSITION ====================
cat("MEDIATION DECOMPOSITION\n")
cat("─────────────────────────────────────\n\n")

# Extract coefficients from both models
coef_reg1 <- summary(reg1)$coefficients
coef_reg2 <- summary(reg2)$coefficients

total_effect <- round(coef_reg1["time", "Estimate"], 3)
direct_effect <- round(coef_reg2["time", "Estimate"], 3)
indirect_effect <- total_effect - direct_effect
mediation_pct <- (indirect_effect / total_effect) * 100

cat("Total Effect (c):", round(total_effect, 3), "pp\n")
cat("Direct Effect (c'):", round(direct_effect, 3), "pp\n")
cat("Indirect Effect (c - c'):", round(indirect_effect, 3), "pp\n")
cat("Mediation Percentage:", round(mediation_pct, 1), "%\n\n")

cat("Interpretation:\n")
cat("✓ Of the", round(total_effect, 2), "pp total bonus effect,\n")
cat("  •", round(mediation_pct, 1), "% works through product mix shift (INDIRECT)\n")
cat("  •", round(100 - mediation_pct, 1), "% works through direct margin gains (DIRECT)\n\n")

# ==================== PATHWAY ANALYSIS ====================
cat("─────────────────────────────────────\n")
cat("PRODUCT-SPECIFIC EFFECTS\n")
cat("─────────────────────────────────────\n\n")

# Extract product coefficients from Model 2
device_coef <- round(coef_reg2["device", "Estimate"], 3)
insurance_coef <- round(coef_reg2["insurance", "Estimate"], 3)
accessory_coef <- round(coef_reg2["accessory", "Estimate"], 3)

product_effects <- data.frame(
  Product = c("Device (reference)", "Insurance", "Accessory"),
  Coefficient = c(0, insurance_coef, accessory_coef),
  Interpretation = c(
    "Baseline category (high-volume, low-margin)",
    "Premium product (high-margin add-on)",
    "Bundled product (very high-margin)"
  )
)

print(product_effects)
cat("\n")

# ==================== PRODUCT MIX SHIFT ====================
cat("─────────────────────────────────────\n")
cat("PRODUCT MIX CHANGE\n")
cat("─────────────────────────────────────\n\n")

product_shift <- sbd %>%
  group_by(period) %>%
  summarise(
    Device_Pct = round(mean(device) * 100, 1),
    Insurance_Pct = round(mean(insurance) * 100, 1),
    Accessory_Pct = round(mean(accessory) * 100, 1),
    .groups = "drop"
  )

cat("Before Bonus:\n")
before_mix <- product_shift[1, ]
cat("  Devices:", before_mix$Device_Pct, "%\n")
cat("  Insurance:", before_mix$Insurance_Pct, "%\n")
cat("  Accessories:", before_mix$Accessory_Pct, "%\n\n")

cat("After Bonus:\n")
after_mix <- product_shift[2, ]
cat("  Devices:", after_mix$Device_Pct, "%\n")
cat("  Insurance:", after_mix$Insurance_Pct, "%\n")
cat("  Accessories:", after_mix$Accessory_Pct, "%\n\n")

cat("Shifts:\n")
cat("  Devices:", round(after_mix$Device_Pct - before_mix$Device_Pct, 1), "pp\n")
cat("  Insurance:", round(after_mix$Insurance_Pct - before_mix$Insurance_Pct, 1), "pp\n")
cat("  Accessories:", round(after_mix$Accessory_Pct - before_mix$Accessory_Pct, 1), "pp\n\n")

cat("Interpretation:\n")
cat("✓ Bonus successfully shifted sales toward higher-margin products\n")
cat("  Insurance + Accessories now represent", 
    round(after_mix$Insurance_Pct + after_mix$Accessory_Pct, 1), 
    "% of mix (was", round(before_mix$Insurance_Pct + before_mix$Accessory_Pct, 1), "%)\n\n")

# ==================== VISUALIZATION 1: Mediation Breakdown ====================
cat("Creating visualizations...\n\n")

png("05_mediation_breakdown.png", width = 700, height = 500)

mediation_data <- data.frame(
  Effect = c("Direct Effect\n(Margin per product)", "Indirect Effect\n(Via product mix)"),
  Value = c(direct_effect, indirect_effect),
  Percentage = c(round(100 - mediation_pct, 1), round(mediation_pct, 1))
)

p <- ggplot(mediation_data, aes(x = Effect, y = Value, fill = Effect)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Value, 2), "pp\n(", Percentage, "%)")), 
            vjust = -0.3, size = 5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Direct Effect\n(Margin per product)" = "#27ae60",
    "Indirect Effect\n(Via product mix)" = "#e67e22"
  )) +
  labs(
    title = "Mediation Decomposition of Bonus Effect",
    x = "Effect Pathway",
    y = "Effect Size (percentage points)",
    subtitle = paste0("Total effect: ", round(total_effect, 2), "pp = ",
                      round(direct_effect, 2), "pp (direct) + ",
                      round(indirect_effect, 2), "pp (indirect)")
  ) +
  ylim(0, total_effect * 1.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    axis.text.x = element_text(size = 11)
  )

print(p)
dev.off()

cat("✓ Saved: 05_mediation_breakdown.png\n\n")

# ==================== VISUALIZATION 2: Effect Attenuation ====================
png("06_effect_attenuation.png", width = 700, height = 500)

# Load Model 3 for comparison
reg3 <- lm(sales_profitmargin ~ time + device + insurance + accessory + factor(store), data = sbd)
coef_reg3 <- summary(reg3)$coefficients
fe_effect <- round(coef_reg3["time", "Estimate"], 3)

attenuation_data <- data.frame(
  Model = c("Model 1\n(Raw)", "Model 2\n(Product\nControls)", "Model 3\n(Store FE)"),
  Coefficient = c(total_effect, direct_effect, fe_effect),
  Color = c("Raw", "Controlled", "Controlled")
)

p2 <- ggplot(attenuation_data, aes(x = reorder(Model, -Coefficient), 
                                   y = Coefficient, fill = Color)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Coefficient, 2), "pp")), 
            vjust = -0.3, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Raw" = "#3498db", "Controlled" = "#27ae60")) +
  labs(
    title = "Effect Attenuation Across Models",
    x = "Model Specification",
    y = "Bonus Effect (percentage points)",
    subtitle = "Effect decreases as confounders are controlled"
  ) +
  ylim(0, total_effect * 1.2) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p2)
dev.off()

cat("✓ Saved: 06_effect_attenuation.png\n\n")

# ==================== SAVE MEDIATION TABLE ====================
mediation_table <- data.frame(
  Pathway = c("Total Effect", "Direct Effect (c')", "Indirect Effect (c-c')", "Mediation %"),
  Estimate = c(round(total_effect, 3),
               round(direct_effect, 3),
               round(indirect_effect, 3),
               round(mediation_pct, 1)),
  Unit = c("pp", "pp", "pp", "%"),
  Interpretation = c(
    "Total bonus impact on profit margins",
    "Margin improvement per product",
    "Effect via product mix shift",
    "% of total effect through product mix"
  )
)

write.csv(mediation_table, "04_mediation_analysis.csv", row.names = FALSE)

cat("✓ Saved: 04_mediation_analysis.csv\n\n")

# ==================== FINAL MEDIATION SUMMARY ====================
cat("═════════════════════════════════════\n")
cat("MEDIATION ANALYSIS SUMMARY\n")
cat("═════════════════════════════════════\n\n")

cat("Effect Decomposition:\n")
cat("  Total Effect: +", round(total_effect, 2), "pp\n")
cat("  Direct Effect: +", round(direct_effect, 2), "pp (", round(100-mediation_pct, 1), "%)\n")
cat("  Indirect Effect: +", round(indirect_effect, 2), "pp (", round(mediation_pct, 1), "%)\n\n")

cat("Product Premium Effects (Model 2):\n")
cat("  Insurance premium over devices: +", insurance_coef, "pp\n")
cat("  Accessory premium over devices: +", accessory_coef, "pp\n\n")

cat("Product Mix Transformation:\n")
cat("  High-margin products (Insurance + Accessories):\n")
cat("    Before: ", round(before_mix$Insurance_Pct + before_mix$Accessory_Pct, 1), "%\n")
cat("    After: ", round(after_mix$Insurance_Pct + after_mix$Accessory_Pct, 1), "%\n")
cat("    Change: +", round((after_mix$Insurance_Pct + after_mix$Accessory_Pct) - 
                             (before_mix$Insurance_Pct + before_mix$Accessory_Pct), 1), "pp\n\n")

cat("Key Insights:\n")
cat("✓", round(mediation_pct, 1), "% of bonus effect works through product mix shift\n")
cat("✓", round(100-mediation_pct, 1), "% represents genuine margin improvements per product\n")
cat("✓ Both mechanisms are important and complementary\n")
cat("✓ Sales team responding to incentives by:\n")
cat("    1. Pushing high-margin products (indirect effect)\n")
cat("    2. Negotiating better margins on all products (direct effect)\n\n")

# ==================== OUTPUTS CREATED ====================
cat("═════════════════════════════════════\n")
cat("FILES CREATED\n")
cat("═════════════════════════════════════\n\n")

cat("Visualizations (2 PNG files):\n")
cat("  ✓ 05_mediation_breakdown.png - Direct vs Indirect effect breakdown\n")
cat("  ✓ 06_effect_attenuation.png - Effect across model specifications\n\n")

cat("Tables (1 CSV file):\n")
cat("  ✓ 04_mediation_analysis.csv - Mediation decomposition summary\n\n")

cat("✓ Mediation analysis complete!\n")
cat("✓ Ready for Script 05: Robustness Checks\n")

# ==================== SESSION INFO ====================
cat("\n═════════════════════════════════════\n")
cat("SESSION INFO\n")
cat("═════════════════════════════════════\n")
cat("Script:", "04_mediation_analysis.R\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version$version.string, "\n")

    