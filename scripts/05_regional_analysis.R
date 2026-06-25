# ==================================================
# FUTURE_DS_01
# Regional Analysis
# ==================================================

library(tidyverse)
library(scales)

# Load cleaned data
sales <- read.csv("data/cleaned/Superstore_clean.csv")

# ==================================================
# Regional Performance Summary
# ==================================================

region_analysis <- sales %>%
  group_by(region) %>%
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    transactions = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    sales_pct = round(total_sales / sum(total_sales) * 100, 2),
    profit_pct = round(total_profit / sum(total_profit) * 100, 2),
    profit_margin = round(total_profit / total_sales * 100, 2)
  ) %>%
  arrange(desc(total_sales))

cat("\n==================== REGIONAL PERFORMANCE ====================\n")
print(region_analysis, width = 200)

# ==================================================
# State-Level Analysis
# ==================================================

state_analysis <- sales %>%
  group_by(region, state) %>%
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    transactions = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    profit_margin = round(total_profit / total_sales * 100, 2),
    sales_pct = round(total_sales / sum(total_sales) * 100, 2)
  ) %>%
  arrange(desc(total_sales))

cat("\n==================== TOP 10 STATES BY SALES ====================\n")
print(head(state_analysis, 10), width = 200)

cat("\n==================== BOTTOM 10 STATES BY PROFIT ====================\n")
state_analysis %>%
  arrange(total_profit) %>%
  head(10) %>%
  print(width = 200)

# ==================================================
# Region-Category Interaction
# ==================================================

region_category <- sales %>%
  group_by(region, category) %>%
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    profit_margin = round(total_profit / total_sales * 100, 2)
  ) %>%
  arrange(region, desc(total_sales))

cat("\n==================== REGION-CATEGORY MATRIX ====================\n")
print(region_category, n = 20, width = 200)

# ==================================================
# Save Tables
# ==================================================

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(region_analysis, 
          "outputs/tables/region_analysis.csv", 
          row.names = FALSE)

write.csv(state_analysis, 
          "outputs/tables/state_analysis.csv", 
          row.names = FALSE)

write.csv(region_category, 
          "outputs/tables/region_category_matrix.csv", 
          row.names = FALSE)

# ==================================================
# Formatted Regional Report
# ==================================================

region_report <- region_analysis %>%
  mutate(
    total_sales = dollar(total_sales, accuracy = 0.01, big.mark = ","),
    total_profit = dollar(total_profit, accuracy = 0.01, big.mark = ","),
    sales_pct = paste0(sprintf("%.1f", sales_pct), "%"),
    profit_pct = paste0(sprintf("%.1f", profit_pct), "%"),
    profit_margin = paste0(sprintf("%.1f", profit_margin), "%")
  )

write.csv(region_report, 
          "outputs/tables/region_report.csv", 
          row.names = FALSE)

# ==================================================
# Visualization 1: Sales by Region
# ==================================================

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

region_colors <- c(
  "West" = "#2E86AB",
  "East" = "#A23B72", 
  "Central" = "#F18F01",
  "South" = "#73A580"
)

region_sales_plot <- ggplot(
  region_analysis,
  aes(x = reorder(region, total_sales), 
      y = total_sales,
      fill = region)
) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(dollar(round(total_sales/1000, 1), suffix = "K"),
                               "\n(", sales_pct, "%)")),
            hjust = -0.1, size = 3.8, fontface = "bold", color = "gray20") +
  scale_fill_manual(values = region_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Total Sales by Region",
    subtitle = "Geographic revenue distribution",
    x = NULL,
    y = "Total Sales (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_blank()
  )

ggsave("outputs/figures/region_sales.png", 
       region_sales_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# Visualization 2: Profit by Region
# ==================================================

region_profit_plot <- ggplot(
  region_analysis,
  aes(x = reorder(region, total_profit), 
      y = total_profit,
      fill = region)
) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(dollar(round(total_profit/1000, 1), suffix = "K"),
                               " (", profit_margin, "%)")),
            hjust = -0.1, size = 3.8, fontface = "bold", color = "gray20") +
  scale_fill_manual(values = region_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Total Profit by Region",
    subtitle = paste("Overall Profit Margin:", 
                     sprintf("%.1f%%", sum(region_analysis$total_profit) / 
                               sum(region_analysis$total_sales) * 100)),
    x = NULL,
    y = "Total Profit (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_blank()
  )

ggsave("outputs/figures/region_profit.png", 
       region_profit_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# Visualization 3: Top 10 States by Sales
# ==================================================

top10_states <- state_analysis %>%
  arrange(desc(total_sales)) %>%
  head(10)

state_sales_plot <- ggplot(
  top10_states,
  aes(x = reorder(state, total_sales), 
      y = total_sales,
      fill = region)
) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = dollar(round(total_sales/1000, 1), suffix = "K")),
            hjust = -0.1, size = 3.5, color = "gray20") +
  scale_fill_manual(values = region_colors, name = "Region") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Top 10 States by Total Sales",
    subtitle = "Highest revenue-generating states",
    x = NULL,
    y = "Total Sales (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/top10_states_sales.png", 
       state_sales_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# Visualization 4: Region-Category Heatmap Data
# ==================================================

region_category_plot <- ggplot(
  region_category,
  aes(x = region, y = category, fill = total_profit)
) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = paste0("$", round(total_profit/1000, 0), "K\n", 
                               profit_margin, "%")),
            size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#e74c3c", mid = "#f39c12", high = "#2ecc71",
                       midpoint = 0, labels = dollar_format(),
                       name = "Profit") +
  labs(
    title = "Profit by Region and Category",
    subtitle = "Where should the business focus regionally?",
    x = NULL,
    y = NULL,
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

ggsave("outputs/figures/region_category_heatmap.png", 
       region_category_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# Summary Statistics
# ==================================================

cat("\n==================== REGIONAL SUMMARY ====================\n")
cat("Best Region (Sales):", region_analysis$region[1], 
    "-", dollar(region_analysis$total_sales[1]), "\n")
cat("Best Region (Profit):", 
    region_analysis$region[which.max(region_analysis$total_profit)], 
    "-", dollar(max(region_analysis$total_profit)), "\n")
cat("Best Profit Margin:", 
    region_analysis$region[which.max(region_analysis$profit_margin)],
    "-", sprintf("%.1f%%", max(region_analysis$profit_margin)), "\n")
cat("Worst Region (Profit):", 
    region_analysis$region[which.min(region_analysis$total_profit)],
    "-", dollar(min(region_analysis$total_profit)), "\n")

cat("\n✓ Regional analysis complete!\n")