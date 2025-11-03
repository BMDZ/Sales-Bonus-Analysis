library(dplyr)
library(readxl)
library(tidyr)

sbd <- read_excel("Part3_casestudy_salesbonus.xlsx")

cat("Raw data loaded:\n")
cat("Dimensions:", nrow(sbd), "rows ×", ncol(sbd), "columns\n\n")

sbd <- sbd %>%
  mutate(
    time = time - 1,
    period = ifelse(time == 0, "Before", "After"),
    device = ifelse(type == "electronic device", 1, 0),
    insurance = ifelse(type == "insurance + device", 1, 0),
    accessory = ifelse(type == "accessory", 1, 0),
    undeclared = ifelse(type == ".", 1, 0),
    region_name = case_when(
      region == "metropole city centre" ~ "Metropole Centre",
      region == "shopping mall" ~ "Shopping Mall",
      region == "town city centre" ~ "Town Centre",
      TRUE ~ region
    ),
    store_label = paste0("Store ", store)
  )

sbd <- sbd[, colnames(sbd) != "region"]

# Validation checks
missing_summary <- sbd %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count") %>%
  filter(Missing_Count > 0)

if (nrow(missing_summary) > 0) {
  cat("\n⚠️ WARNING: Missing values:\n")
  print(missing_summary)
} else {
  cat("\n✓ No missing values\n")
}

# Product validation
sbd <- sbd %>% mutate(product_sum = device + insurance + accessory + undeclared)
if (all(sbd$product_sum == 1)) {
  cat("✓ Product types valid (sum = 1)\n")
} else {
  cat("⚠️ Product types invalid\n")
}
sbd <- sbd[, colnames(sbd) != "product_sum"]

# Summary
cat("\n═══════════════════════════════════════\n")
cat("DATA SUMMARY\n")
cat("═══════════════════════════════════════\n\n")

cat("Sample Sizes:\n")
print(sbd %>% group_by(period) %>% summarise(N = n(), .groups = "drop"))

cat("\nProfit Margin Statistics:\n")
print(
  sbd %>%
    group_by(period) %>%
    summarise(
      Mean = round(mean(sales_profitmargin), 2),
      Median = round(median(sales_profitmargin), 2),
      SD = round(sd(sales_profitmargin), 2),
      .groups = "drop"
    )
)

cat("\nStores:", n_distinct(sbd$store), "| Regions:", n_distinct(sbd$region_name), "\n")

cat("\nProduct Mix:\n")
print(
  sbd %>%
    group_by(period) %>%
    summarise(
      Devices = round(mean(device) * 100, 1),
      Insurance = round(mean(insurance) * 100, 1),
      Accessories = round(mean(accessory) * 100, 1),
      .groups = "drop"
    )
)

# Save
write.csv(sbd, "sbd_clean.csv", row.names = FALSE)
cat("\n✓ Data saved: sbd_clean.csv\n")
cat("✓ Complete!\n")
cat("\nR Version:", R.version$version.string, "\n")




