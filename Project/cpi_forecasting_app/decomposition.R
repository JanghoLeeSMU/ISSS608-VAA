pacman::p_load(tidyverse, shiny, readxl)

data <- read_excel("data.xlsx", sheet = "Raw")

cpi_cols <- grep("^CPI", names(data), value = TRUE)

cpi_cols
length(cpi_cols)

decomp_list <- list()
table1_list <- list()
table2_list <- list()

for (col in cpi_cols) {
  
  # 1. Create time series object for the CPI column
  ts_data <- ts(data[[col]], start = c(2020,1), frequency = 12)
  
  # 2. Perform additive decomposition
  decomp <- decompose(ts_data, type = "additive")
  
  decomp_list[[col]] <- decomp
  
  # ---------------------------------------------------
  # Create Table 1: Observed Series Summary
  # ---------------------------------------------------
  
  start_value <- ts_data[1]
  end_value <- tail(ts_data,1)
  
  total_increase <- end_value - start_value
  increase_rate <- (total_increase/start_value)*100
  
  table1_list[[col]] <- data.frame(
    CPI = col,
    Metric = c(
      "Start CPI (2020-01)",
      "End CPI (2025-12)",
      "Total Increase",
      "Increase Rate"
    ),
    Value = c(
      round(start_value,3),
      round(end_value,3),
      round(total_increase,3),
      paste0(round(increase_rate,1),"%")
    )
  )
  
  # ---------------------------------------------------
  # Extract decomposition components
  # ---------------------------------------------------
  
  trend_vals <- na.omit(as.numeric(decomp$trend))
  seasonal_vals <- na.omit(as.numeric(decomp$seasonal))
  residual_vals <- na.omit(as.numeric(decomp$random))
  
  trend_start <- head(trend_vals,1)
  trend_end <- tail(trend_vals,1)
  trend_change <- trend_end - trend_start
  
  seasonal_min <- min(seasonal_vals)
  seasonal_max <- max(seasonal_vals)
  seasonal_range <- seasonal_max - seasonal_min
  
  residual_min <- min(residual_vals)
  residual_max <- max(residual_vals)
  residual_range <- residual_max - residual_min
  
  # ---------------------------------------------------
  # Calculate ratios used for interpretation rules
  # ---------------------------------------------------
  
  series_mean <- mean(ts_data, na.rm = TRUE)
  
  trend_ratio <- abs(trend_change) / series_mean
  seasonal_ratio <- seasonal_range / series_mean
  residual_ratio <- residual_range / series_mean

  # ---------------------------------------------------
  # Generate rule-based interpretations
  # ---------------------------------------------------
  
  trend_interp <- if (trend_ratio > 0.10) {
    "Strong upward trend over the sample period."
  } else if (trend_ratio > 0.03) {
    "Moderate upward trend."
  } else {
    "Relatively stable long-term trend."
  }
  
  seasonal_interp <- if (seasonal_ratio > 0.02) {
    "Noticeable seasonal variation."
  } else if (seasonal_ratio > 0.01) {
    "Mild seasonal pattern."
  } else {
    "Very weak seasonality."
  }
  
  residual_interp <- if (residual_ratio > 0.03) {
    "Relatively high irregular fluctuations."
  } else if (residual_ratio > 0.01) {
    "Moderate irregular variation."
  } else {
    "Series is relatively stable with small random noise."
  }
  
  # ---------------------------------------------------
  # Create Table 2: Decomposition Summary
  # ---------------------------------------------------
  
  table2_list[[col]] <- data.frame(
    CPI = col,
    Component = c("Trend","Seasonality","Residual"),
    Characteristics = c(
      paste0("Trend change = ", round(trend_change,3)),
      paste0("Seasonal range = ", round(seasonal_range,3)),
      paste0("Residual range = ", round(residual_range,3))
    ),
    Interpretation = c(
      trend_interp,
      seasonal_interp,
      residual_interp
    )
  )
  
}

table1_list[["CPI: Food"]]
table2_list[["CPI: Food"]]
plot(decomp_list[[""]])

table1_list[["CPI: Housing & Utilities"]]
table2_list[["CPI: Housing & Utilities"]]
plot(decomp_list[["CPI: Housing & Utilities"]])

all_table1 <- do.call(rbind, table1_list)
all_table2 <- do.call(rbind, table2_list)

all_table1
all_table2

write.csv(all_table1, "all_table1.csv", row.names = FALSE)
write.csv(all_table2, "all_table2.csv", row.names = FALSE)

