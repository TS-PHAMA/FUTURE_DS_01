# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)

# Category Analysis with improved structure
category_analysis <- sales %>%
  group_by(category) %>%
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    transactions = n(),
    avg_order_value = round(total_sales / transactions, 2),
    .groups = 'drop'
  ) %>%
  mutate(
    sales_pct = round(total_sales / sum(total_sales) * 100, 2),
    profit_pct = round(total_profit / sum(total_profit) * 100, 2),
    profit_margin = round(total_profit / total_sales * 100, 2),
    quantity_pct = round(total_quantity / sum(total_quantity) * 100, 2)
  ) %>%
  arrange(desc(total_sales))

# Print summary statistics
cat("\n==================== CATEGORY PERFORMANCE SUMMARY ====================\n")
print(category_analysis, width = 200)
cat("\n")

# Create formatted report
category_report <- category_analysis %>%
  mutate(
    total_sales = dollar(total_sales, accuracy = 0.01, big.mark = ","),
    total_profit = dollar(total_profit, accuracy = 0.01, big.mark = ","),
    avg_order_value = dollar(avg_order_value, accuracy = 0.01, big.mark = ","),
    sales_pct = paste0(sprintf("%.1f", sales_pct), "%"),
    profit_pct = paste0(sprintf("%.1f", profit_pct), "%"),
    profit_margin = paste0(sprintf("%.1f", profit_margin), "%"),
    quantity_pct = paste0(sprintf("%.1f", quantity_pct), "%")
  )

# Print formatted report
cat("\n==================== FORMATTED CATEGORY REPORT ====================\n")
print(category_report, width = 200)

# Ensure output directories exist
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Save report to CSV
write.csv(category_report, "outputs/tables/category_report.csv", row.names = FALSE)

# Define a professional color palette
category_colors <- c(
  "Technology" = "#2E86AB",   # Deep blue
  "Furniture" = "#A23B72",     # Rich burgundy
  "Office Supplies" = "#F18F01" # Warm gold
)

# If categories differ, use a gradient approach
if(!all(unique(category_analysis$category) %in% names(category_colors))) {
  category_colors <- setNames(
    viridis::viridis(nrow(category_analysis), option = "D"),
    category_analysis$category
  )
}

# Enhanced Sales Plot
category_sales_plot <- ggplot(category_analysis, 
                              aes(x = reorder(category, total_sales), 
                                  y = total_sales,
                                  fill = category)) +
  geom_bar(stat = "identity", 
           width = 0.7,
           alpha = 0.9,
           color = "gray30",
           size = 0.3) +
  geom_text(aes(label = dollar(round(total_sales/1000, 1), suffix = "K")),
            hjust = -0.1,
            size = 4,
            fontface = "bold",
            color = "gray20") +
  scale_fill_manual(values = category_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Total Sales by Product Category",
    subtitle = paste("Analysis Period:", min(sales$date), "to", max(sales$date)),
    x = NULL,
    y = "Total Sales (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    plot.caption = element_text(size = 9, color = "#95a5a6", hjust = 1),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

# Enhanced Profit Plot
category_profit_plot <- ggplot(category_analysis, 
                               aes(x = reorder(category, total_profit), 
                                   y = total_profit,
                                   fill = category)) +
  geom_bar(stat = "identity", 
           width = 0.7,
           alpha = 0.9,
           color = "gray30",
           size = 0.3) +
  geom_text(aes(label = paste0(dollar(round(total_profit/1000, 1), suffix = "K"),
                               " (", profit_margin, "%)")),
            hjust = -0.1,
            size = 4,
            fontface = "bold",
            color = "gray20") +
  scale_fill_manual(values = category_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Total Profit by Product Category",
    subtitle = paste("Overall Profit Margin:", 
                     sprintf("%.1f%%", sum(category_analysis$total_profit) / 
                               sum(category_analysis$total_sales) * 100)),
    x = NULL,
    y = "Total Profit (USD)",
    caption = "Source: Business Sales Performance Analytics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    plot.caption = element_text(size = 9, color = "#95a5a6", hjust = 1),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

# Combined Performance Dashboard Plot
category_dashboard <- ggplot(category_analysis) +
  # Sales bars
  geom_bar(aes(x = reorder(category, total_sales), 
               y = total_sales, 
               fill = "Sales"),
           stat = "identity",
           width = 0.35,
           position = position_nudge(y = 0),
           alpha = 0.8) +
  # Profit bars
  geom_bar(aes(x = reorder(category, total_sales), 
               y = total_profit, 
               fill = "Profit"),
           stat = "identity",
           width = 0.35,
           position = position_nudge(y = 0),
           alpha = 0.9) +
  scale_fill_manual(values = c("Sales" = "#3498db", "Profit" = "#2ecc71"),
                    name = "Metric") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Category Performance: Sales vs Profit",
    subtitle = "Comparing revenue and profitability across categories",
    x = NULL,
    y = "Amount (USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save all plots
ggsave("outputs/figures/category_sales.png", 
       category_sales_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

ggsave("outputs/figures/category_profit.png", 
       category_profit_plot, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

ggsave("outputs/figures/category_dashboard.png", 
       category_dashboard, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Print completion message
cat("\n✓ Analysis complete! Files saved to outputs/tables/ and outputs/figures/\n")
