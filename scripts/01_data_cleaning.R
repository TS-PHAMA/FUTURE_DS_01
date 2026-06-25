# ==========================================================
# FUTURE_DS_01
# Business Sales Performance Analytics
# Data Cleaning Script
# ==========================================================

# Load Libraries
library(tidyverse)
library(lubridate)
library(janitor)

# ==========================================================
# Import Data
# ==========================================================

sales <- read.csv(
  "data/raw/Sample - Superstore.csv")

# ==========================================================
# Initial Inspection
# ==========================================================

head(sales)

str(sales)

summary(sales)

dim(sales)

# ==========================================================
# Clean Column Names
# ==========================================================

sales <- clean_names(sales)

names(sales)

# ==========================================================
# Missing Values
# ==========================================================

missing_values <- colSums(is.na(sales))

missing_values

# ==========================================================
# Duplicate Records
# ==========================================================

duplicates <- sum(duplicated(sales))

duplicates

# ==========================================================
# Convert Dates
# ==========================================================

sales$order_date <- mdy(sales$order_date)

sales$ship_date <- mdy(sales$ship_date)

# Verify

str(sales$order_date)

# ==========================================================
# Save Clean Dataset
# ==========================================================

dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)

write.csv(
  sales,
  "data/cleaned/superstore_clean.csv",
  row.names = FALSE
)

# ==========================================================
# Data Quality Summary
# ==========================================================

cat("\n")
cat("Rows:", nrow(sales), "\n")
cat("Columns:", ncol(sales), "\n")
cat("Missing Values:", sum(missing_values), "\n")
cat("Duplicates:", duplicates, "\n")
