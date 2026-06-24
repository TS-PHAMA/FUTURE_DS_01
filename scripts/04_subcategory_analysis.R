# ==================================================
# FUTURE_DS_01
# Sub-Category Analysis
# ==================================================

library(tidyverse)
library(scales)

# Load cleaned data
sales <- read.csv("data/cleaned/superstore_clean.csv")

# ==================================================
# Sub-Category Performance
# ==================================================

subcategory_analysis <- sales %>%
  group_by(category, sub_category) %>%
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    transactions = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    profit_margin = round(total_profit / total_sales * 100, 2),
    sales_pct = round(total_sales / sum(total_sales) * 100, 2),
    profit_pct = round(total_profit / sum(total_profit) * 100, 2)
  ) %>%
  arrange(desc(total_sales))

cat("\n==================== SUB-CATEGORY PERFORMANCE ====================\n")
print(subcategory_analysis, n = 50, width = 200)

# ==================================================
# Identify Loss-Making Sub-Categories
# ==================================================

loss_makers <- subcategory_analysis %>%
  filter(total_profit < 0) %>%
  arrange(total_profit)

cat("\n==================== LOSS-MAKING SUB-CATEGORIES ====================\n")
print(loss_makers, width = 200)

# ==================================================
# Top Performers by Profit
# ==================================================

top_profit <- subcategory_analysis %>%
  arrange(desc(total_profit)) %>%
  slice_head(n = 10)

cat("\n==================== TOP 10 BY PROFIT ====================\n")
print(top_profit, width = 200)

# ==================================================
# Bottom Performers by Profit
# ==================================================

bottom_profit <- subcategory_analysis %>%
  arrange(total_profit) %>%
  slice_head(n = 10)

cat("\n==================== BOTTOM 10 BY PROFIT ====================\n")
print(bottom_profit, width = 200)

# ==================================================
# Save Tables
# ==================================================

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(subcategory_analysis, 
          "outputs/tables/subcategory_analysis.csv", 
          row.names = FALSE)

write.csv(loss_makers, 
          "outputs/tables/loss_making_subcategories.csv", 
          row.names = FALSE)

# ==================================================
# Formatted Report Table
# ==================================================

subcategory_report <- subcategory_analysis %>%
  mutate(
    total_sales = dollar(total_sales, accuracy = 0.01, big.mark = ","),
    total_profit = dollar(total_profit, accuracy = 0.01, big.mark = ","),
    profit_margin = paste0(sprintf("%.1f", profit_margin), "%"),
    sales_pct = paste0(sprintf("%.1f", sales_pct), "%"),
    profit_pct = paste0(sprintf("%.1f", profit_pct), "%")
  )

write.csv(subcategory_report, 
          "outputs/tables/subcategory_report.csv", 
          row.names = FALSE)

# ==================================================
# Visualization 1: All Sub-Categories Sales vs Profit
# ==================================================

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Define colors for profit/loss
subcategory_analysis <- subcategory_analysis %>%
  mutate(profit_status = ifelse(total_profit >= 0, "Profitable", "Loss-Making"))

subcategory_plot <- ggplot(
  subcategory_analysis,
  aes(x = reorder(sub_category, total_sales), 
      y = total_sales,
      fill = profit_status)
) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0("$", round(total_sales/1000, 1), "K")),
            hjust = -0.1, size = 3.2, color = "gray20") +
  scale_fill_manual(values = c("Profitable" = "#2ecc71", 
                               "Loss-Making" = "#e74c3c"),
                    name = "Status") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Total Sales by Sub-Category",
    subtitle = "Green = Profitable | Red = Loss-Making",
    x = NULL,
    y = "Total Sales (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    plot.caption = element_text(size = 9, color = "#95a5a6", hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave("outputs/figures/subcategory_sales.png", 
       subcategory_plot, 
       width = 10, 
       height = 8, 
       dpi = 300,
       bg = "white")

# ==================================================
# Visualization 2: Top 10 by Profit
# ==================================================

top10_plot <- ggplot(
  top_profit,
  aes(x = reorder(sub_category, total_profit), 
      y = total_profit,
      fill = category)
) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0("$", round(total_profit/1000, 1), "K")),
            hjust = -0.1, size = 3.5, fontface = "bold", color = "gray20") +
  scale_fill_manual(values = c("Technology" = "#2E86AB",
                               "Furniture" = "#A23B72",
                               "Office Supplies" = "#F18F01"),
                    name = "Category") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Top 10 Most Profitable Sub-Categories",
    subtitle = "Highest profit contributors across all categories",
    x = NULL,
    y = "Total Profit (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/top10_profit_subcategories.png", 
       top10_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# Visualization 3: Bottom 10 by Profit (Loss Makers)
# ==================================================

bottom10_plot <- ggplot(
  bottom_profit,
  aes(x = reorder(sub_category, -total_profit), 
      y = total_profit,
      fill = category)
) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0("$", round(total_profit/1000, 1), "K")),
            hjust = ifelse(bottom_profit$total_profit < 0, 1.1, -0.1),
            size = 3.5, fontface = "bold", color = "gray20") +
  scale_fill_manual(values = c("Technology" = "#2E86AB",
                               "Furniture" = "#A23B72",
                               "Office Supplies" = "#F18F01"),
                    name = "Category") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Bottom 10 Sub-Categories by Profit",
    subtitle = "Largest loss-makers requiring immediate attention",
    x = NULL,
    y = "Total Profit (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#e74c3c"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/bottom10_profit_subcategories.png", 
       bottom10_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# Visualization 4: Furniture Deep Dive
# ==================================================

furniture_only <- subcategory_analysis %>%
  filter(category == "Furniture") %>%
  arrange(desc(total_profit))

furniture_plot <- ggplot(
  furniture_only,
  aes(x = reorder(sub_category, total_profit), 
      y = total_profit,
      fill = profit_status)
) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
  geom_text(aes(label = paste0("$", round(total_profit/1000, 1), "K",
                               "\n(", profit_margin, "%)")),
            hjust = ifelse(furniture_only$total_profit < 0, 1.1, -0.1),
            size = 3.5, color = "gray20") +
  scale_fill_manual(values = c("Profitable" = "#2ecc71", 
                               "Loss-Making" = "#e74c3c"),
                    guide = "none") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Furniture Sub-Categories: Profit Analysis",
    subtitle = paste("Total Furniture Profit: $", 
                     comma(sum(furniture_only$total_profit))),
    x = NULL,
    y = "Total Profit (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    axis.text.y = element_text(size = 11, face = "bold")
  )

ggsave("outputs/figures/furniture_deep_dive.png", 
       furniture_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# ==================================================
# Summary Statistics
# ==================================================

cat("\n==================== SUMMARY ====================\n")
cat("Total Sub-Categories:", n_distinct(sales$sub_category), "\n")
cat("Loss-Making Sub-Categories:", nrow(loss_makers), "\n")
cat("Total Losses:", dollar(sum(loss_makers$total_profit)), "\n")
cat("Most Profitable:", top_profit$sub_category[1], 
    "-", dollar(top_profit$total_profit[1]), "\n")
cat("Biggest Loss-Maker:", loss_makers$sub_category[1], 
    "-", dollar(loss_makers$total_profit[1]), "\n")

cat("\n✓ Sub-category analysis complete!\n")