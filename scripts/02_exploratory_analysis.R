# ==================================================
# FUTURE_DS_01
# Exploratory Data Analysis - KPI Calculations
# ==================================================

library(tidyverse)
library(lubridate)
library(scales)

# Load cleaned data
sales <- read.csv("data/cleaned/Superstore_clean.csv")
sales$order_date <- as.Date(sales$order_date)

# ==================================================
# KPI 1: Total Sales
# ==================================================
total_sales <- sum(sales$sales, na.rm = TRUE)

# ==================================================
# KPI 2: Total Profit
# ==================================================
total_profit <- sum(sales$profit, na.rm = TRUE)

# ==================================================
# KPI 3: Profit Margin
# ==================================================
profit_margin <- (total_profit / total_sales) * 100

# ==================================================
# KPI 4: Number of Orders
# ==================================================
total_orders <- n_distinct(sales$order_id)

# ==================================================
# KPI 5: Number of Customers
# ==================================================
total_customers <- n_distinct(sales$customer_id)

# ==================================================
# KPI 6: Average Order Value
# ==================================================
average_order_value <- total_sales / total_orders

# ==================================================
# Print KPIs
# ==================================================
cat("\n==================== KEY PERFORMANCE INDICATORS ====================\n")
cat("Total Sales:        ", dollar(total_sales, big.mark = ","), "\n")
cat("Total Profit:       ", dollar(total_profit, big.mark = ","), "\n")
cat("Profit Margin:      ", round(profit_margin, 2), "%\n")
cat("Total Orders:       ", comma(total_orders), "\n")
cat("Total Customers:    ", comma(total_customers), "\n")
cat("Average Order Value:", dollar(round(average_order_value, 2), big.mark = ","), "\n")

# ==================================================
# Save KPI Table
# ==================================================
kpi_table <- data.frame(
  Metric = c("Total Sales", "Total Profit", "Profit Margin",
             "Total Orders", "Total Customers", "Average Order Value"),
  Value = c(
    total_sales,
    total_profit,
    profit_margin,
    total_orders,
    total_customers,
    average_order_value
  )
)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(kpi_table, "outputs/tables/kpi_summary.csv", row.names = FALSE)

cat("\nKPI summary saved to outputs/tables/kpi_summary.csv\n")