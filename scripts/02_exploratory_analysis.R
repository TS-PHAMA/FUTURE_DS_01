# ==========================================================
# FUTURE_DS_01
# Exploratory Data Analysis
# ==========================================================

library(tidyverse)
library(lubridate)

# Load cleaned data

sales <- read.csv(
  "data/cleaned/Superstore_clean.csv"
)

sales$order_date <- as.Date(sales$order_date)

# ==========================================================
# KPI 1: Total Sales
# ==========================================================

total_sales <- sum(sales$sales)

# ==========================================================
# KPI 2: Total Profit
# ==========================================================

total_profit <- sum(sales$profit)

# ==========================================================
# KPI 3: Profit Margin
# ==========================================================

profit_margin <- (total_profit / total_sales) * 100

# ==========================================================
# KPI 4: Number of Orders
# ==========================================================

total_orders <- n_distinct(sales$order_id)

# ==========================================================
# KPI 5: Number of Customers
# ==========================================================

total_customers <- n_distinct(sales$customer_id)

# ==========================================================
# KPI 6: Average Order Value
# ==========================================================

average_order_value <- total_sales / total_orders

# ==========================================================
# Print KPIs
# ==========================================================

cat("\n")
cat("Total Sales: ", round(total_sales,2), "\n")
cat("Total Profit: ", round(total_profit,2), "\n")
cat("Profit Margin: ", round(profit_margin,2), "%\n")
cat("Total Orders: ", total_orders, "\n")
cat("Total Customers: ", total_customers, "\n")
cat("Average Order Value: ", round(average_order_value,2), "\n")

kpi_table <- data.frame(
  Metric = c(
    "Total Sales",
    "Total Profit",
    "Profit Margin",
    "Total Orders",
    "Total Customers",
    "Average Order Value"
  ),
  Value = c(
    total_sales,
    total_profit,
    profit_margin,
    total_orders,
    total_customers,
    average_order_value
  )
)

dir.create("outputs/tables",
           recursive = TRUE,
           showWarnings = FALSE)

write.csv(
  kpi_table,
  "outputs/tables/kpi_summary.csv",
  row.names = FALSE
)
