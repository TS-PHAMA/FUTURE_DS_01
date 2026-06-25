# ==================================================
# FUTURE_DS_01
# Statistical Analysis
# ==================================================

library(tidyverse)
library(scales)
library(broom)

# Load cleaned data
sales <- read.csv("data/cleaned/Superstore_clean.csv")

cat("\n================================================")
cat("\n  STATISTICAL ANALYSIS")
cat("\n================================================\n")

# ==================================================
# 1. CORRELATION ANALYSIS
# ==================================================

cat("\n==================== 1. CORRELATION ANALYSIS ====================\n")

# Overall correlation: Sales vs Profit
cor_sales_profit <- cor(sales$sales, sales$profit)
cat("\nOverall Correlation (Sales vs Profit):", round(cor_sales_profit, 4), "\n")

# Correlation by Category
cor_by_category <- sales %>%
  group_by(category) %>%
  summarise(
    correlation = round(cor(sales, profit), 4),
    .groups = 'drop'
  )

cat("\nCorrelation by Category:\n")
print(cor_by_category)

# Correlation by Region
cor_by_region <- sales %>%
  group_by(region) %>%
  summarise(
    correlation = round(cor(sales, profit), 4),
    .groups = 'drop'
  )

cat("\nCorrelation by Region:\n")
print(cor_by_region)

# Interpretation helper
cat("\nInterpretation:\n")
cat("- Close to +1: Strong positive relationship (higher sales → higher profit)\n")
cat("- Close to 0: Weak or no relationship\n")
cat("- Negative: Higher sales → lower profit (PROBLEM!)\n")

# ==================================================
# 2. ANOVA - Are Profit Differences Significant?
# ==================================================

cat("\n==================== 2. ANOVA ANALYSIS ====================\n")

# Test 1: Profit differences across Categories
cat("\nH0: All categories have the same mean profit\n")
cat("H1: At least one category has a significantly different mean profit\n\n")

anova_category <- aov(profit ~ category, data = sales)
summary_category <- summary(anova_category)

cat("ANOVA: Profit ~ Category\n")
print(summary_category)

# Extract p-value
p_value_category <- summary_category[[1]]$`Pr(>F)`[1]
cat("\nP-value:", format(p_value_category, scientific = TRUE, digits = 4))

if(p_value_category < 0.05) {
  cat("\n✅ Statistically significant at α = 0.05")
  cat("\n   Categories DO have significantly different profit levels.\n")
} else {
  cat("\n❌ Not statistically significant at α = 0.05")
  cat("\n   No evidence that categories differ in profit.\n")
}

# Test 2: Profit differences across Regions
cat("\n-------------------------------------------\n")
cat("H0: All regions have the same mean profit\n")
cat("H1: At least one region has a significantly different mean profit\n\n")

anova_region <- aov(profit ~ region, data = sales)
summary_region <- summary(anova_region)

cat("ANOVA: Profit ~ Region\n")
print(summary_region)

p_value_region <- summary_region[[1]]$`Pr(>F)`[1]
cat("\nP-value:", format(p_value_region, scientific = TRUE, digits = 4))

if(p_value_region < 0.05) {
  cat("\n✅ Statistically significant at α = 0.05")
  cat("\n   Regions DO have significantly different profit levels.\n")
} else {
  cat("\n❌ Not statistically significant at α = 0.05")
  cat("\n   No evidence that regions differ in profit.\n")
}

# ==================================================
# 3. TUKEY HSD - Which Groups Differ?
# ==================================================

cat("\n==================== 3. POST-HOC ANALYSIS ====================\n")

if(p_value_category < 0.05) {
  cat("\nTukey HSD: Category\n")
  cat("Which specific categories differ?\n\n")
  tukey_category <- TukeyHSD(anova_category)
  print(tukey_category)
}

if(p_value_region < 0.05) {
  cat("\nTukey HSD: Region\n")
  cat("Which specific regions differ?\n\n")
  tukey_region <- TukeyHSD(anova_region)
  print(tukey_region)
}

# ==================================================
# 4. LINEAR REGRESSION - Predict Profit from Sales
# ==================================================

cat("\n==================== 4. LINEAR REGRESSION ====================\n")
cat("Model: Profit = β₀ + β₁(Sales) + ε\n\n")

# Build model
model <- lm(profit ~ sales, data = sales)

# Model summary
model_summary <- summary(model)
print(model_summary)

# Extract key statistics
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared
intercept <- coef(model)[1]
slope <- coef(model)[2]
p_value_model <- model_summary$coefficients[2, 4]

cat("\n--- Key Model Statistics ---\n")
cat("R-squared:", round(r_squared, 4), "\n")
cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")
cat("Intercept (β₀):", round(intercept, 4), "\n")
cat("Slope (β₁):", round(slope, 4), "\n")
cat("P-value (Sales):", format(p_value_model, scientific = TRUE, digits = 4), "\n")

cat("\nInterpretation:\n")
cat("For every $1 increase in Sales, Profit changes by $", round(slope, 4), "\n")
cat("Sales explains", round(r_squared * 100, 2), "% of the variation in Profit\n")

# ==================================================
# 5. MULTIPLE REGRESSION - Category & Region Effects
# ==================================================

cat("\n==================== 5. MULTIPLE REGRESSION ====================\n")
cat("Model: Profit = β₀ + β₁(Sales) + β₂(Category) + β₃(Region) + ε\n\n")

model_multi <- lm(profit ~ sales + category + region, data = sales)
summary_multi <- summary(model_multi)
print(summary_multi)

cat("\nR-squared (Multiple):", round(summary_multi$r.squared, 4))
cat("\nAdjusted R-squared:", round(summary_multi$adj.r.squared, 4), "\n")

# ==================================================
# 6. DISCOUNT IMPACT ANALYSIS
# ==================================================

cat("\n==================== 6. DISCOUNT IMPACT ====================\n")

discount_model <- lm(profit ~ discount, data = sales)
discount_summary <- summary(discount_model)
print(discount_summary)

cat("\nCorrelation (Discount vs Profit):", 
    round(cor(sales$discount, sales$profit), 4), "\n")

# Average profit at different discount levels
discount_analysis <- sales %>%
  mutate(discount_level = case_when(
    discount == 0 ~ "No Discount",
    discount <= 0.2 ~ "Low (≤20%)",
    discount <= 0.5 ~ "Medium (21-50%)",
    TRUE ~ "High (>50%)"
  )) %>%
  group_by(discount_level) %>%
  summarise(
    avg_profit = mean(profit, na.rm = TRUE),
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE),
    transactions = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    profit_per_order = round(total_profit / transactions, 2),
    pct_transactions = round(transactions / sum(transactions) * 100, 1)
  ) %>%
  arrange(desc(total_profit))

cat("\nProfit by Discount Level:\n")
print(discount_analysis, width = 200)

# ==================================================
# SAVE ALL RESULTS
# ==================================================

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Save model summaries
sink("outputs/tables/statistical_results.txt")

cat("========================================\n")
cat("STATISTICAL ANALYSIS RESULTS\n")
cat("========================================\n\n")

cat("1. CORRELATION ANALYSIS\n")
cat("Overall Sales-Profit Correlation:", round(cor_sales_profit, 4), "\n\n")
cat("By Category:\n")
print(cor_by_category)
cat("\nBy Region:\n")
print(cor_by_region)

cat("\n\n2. ANOVA: Profit ~ Category\n")
print(summary_category)
cat("\nANOVA: Profit ~ Region\n")
print(summary_region)

cat("\n\n3. LINEAR REGRESSION\n")
cat("R-squared:", round(r_squared, 4), "\n")
cat("Slope:", round(slope, 4), "\n")
cat("P-value:", format(p_value_model, scientific = TRUE, digits = 4), "\n")

cat("\n\n4. MULTIPLE REGRESSION\n")
cat("R-squared:", round(summary_multi$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_multi$adj.r.squared, 4), "\n")

cat("\n\n5. DISCOUNT IMPACT\n")
print(discount_analysis)

sink()

# Save discount analysis
write.csv(discount_analysis, 
          "outputs/tables/discount_impact.csv", 
          row.names = FALSE)

# Save correlation results
cor_results <- bind_rows(
  cor_by_category,
  cor_by_region %>% rename(category = region)
)

# ==================================================
# VISUALIZATIONS
# ==================================================

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Scatter plot: Sales vs Profit with regression line
scatter_plot <- ggplot(sales, aes(x = sales, y = profit, color = category)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
  scale_color_manual(values = c("Technology" = "#2E86AB",
                                "Furniture" = "#A23B72",
                                "Office Supplies" = "#F18F01"),
                     name = "Category") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Relationship Between Sales and Profit",
    subtitle = paste("Correlation:", round(cor_sales_profit, 3), 
                     "| R-squared:", round(r_squared, 3)),
    x = "Sales (USD)",
    y = "Profit (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/sales_vs_profit_scatter.png", 
       scatter_plot, 
       width = 10, 
       height = 7, 
       dpi = 300,
       bg = "white")

# Boxplot: Profit by Category
category_boxplot <- ggplot(sales, aes(x = category, y = profit, fill = category)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Technology" = "#2E86AB",
                               "Furniture" = "#A23B72",
                               "Office Supplies" = "#F18F01"),
                    guide = "none") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Profit Distribution by Category",
    subtitle = paste("ANOVA p-value:", format(p_value_category, scientific = TRUE, digits = 4)),
    x = NULL,
    y = "Profit (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d")
  )

ggsave("outputs/figures/profit_boxplot_category.png", 
       category_boxplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Discount Impact Bar Chart
discount_plot <- ggplot(discount_analysis, 
                        aes(x = reorder(discount_level, avg_profit), 
                            y = avg_profit,
                            fill = avg_profit > 0)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(avg_profit, 2), "\n(", pct_transactions, "% of orders)")),
            hjust = -0.1, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Average Profit by Discount Level",
    subtitle = paste("Correlation Discount vs Profit:", 
                     round(cor(sales$discount, sales$profit), 3)),
    x = NULL,
    y = "Average Profit per Order (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    axis.text.y = element_text(size = 11, face = "bold")
  )

ggsave("outputs/figures/discount_impact.png", 
       discount_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# FINAL SUMMARY
# ==================================================

cat("\n\n================================================")
cat("\n  STATISTICAL ANALYSIS COMPLETE")
cat("\n================================================\n")

cat("\nKey Statistical Findings:\n")
cat("1. Sales-Profit Correlation:", round(cor_sales_profit, 3), "\n")
cat("2. Category ANOVA p-value:", format(p_value_category, digits = 4), "\n")
cat("3. Region ANOVA p-value:", format(p_value_region, digits = 4), "\n")
cat("4. R-squared (Sales only):", round(r_squared, 3), "\n")
cat("5. R-squared (Multiple):", round(summary_multi$adj.r.squared, 3), "\n")
cat("6. Discount-Profit Correlation:", round(cor(sales$discount, sales$profit), 3), "\n")

cat("\n✓ Results saved to outputs/tables/statistical_results.txt\n")
cat("✓ Visualizations saved to outputs/figures/\n")
