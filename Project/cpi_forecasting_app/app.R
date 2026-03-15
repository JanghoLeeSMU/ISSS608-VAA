pacman::p_load(tidyverse, shiny, readxl, DT)

# plot.decomposed.ts hardcodes the main title, so use a wrapper that omits it
plot_decomp <- function(x, ...) {
  xx <- x$x
  if (is.null(xx))
    xx <- with(x, if (type == "additive") random + trend + seasonal
                  else random * trend * seasonal)
  plot(cbind(observed = xx, trend = x$trend, seasonal = x$seasonal, random = x$random),
       main = "", ...)
}

# ── Data prep ──────────────────────────────────────────────────────────────────
data     <- read_excel("data.xlsx", sheet = "Raw")
cpi_cols <- grep("^CPI", names(data), value = TRUE)

# Precompute time series
ts_list <- setNames(
  lapply(cpi_cols, function(col) ts(data[[col]], start = c(2020, 1), frequency = 12)),
  cpi_cols
)

# Precompute Table 1 (independent of decomp type)
table1_list <- lapply(cpi_cols, function(col) {
  ts_data        <- ts_list[[col]]
  start_value    <- ts_data[1]
  end_value      <- tail(ts_data, 1)
  total_increase <- end_value - start_value
  increase_rate  <- (total_increase / start_value) * 100
  data.frame(
    Metric = c("Start CPI (2020-01)", "End CPI (2025-12)", "Total Increase", "Increase Rate"),
    Value  = c(round(start_value, 3), round(end_value, 3),
               round(total_increase, 3), paste0(round(increase_rate, 1), "%"))
  )
})
names(table1_list) <- cpi_cols

# ── Category hierarchy ─────────────────────────────────────────────────────────
parent_cats <- c(
  "CPI: Food",
  "CPI: Clothing and Footwear (C&F)",
  "CPI: Housing & Utilities",
  "CPI: Household Durables & Services (HDS)",
  "CPI: Health",
  "CPI: Transport",
  "CPI: Information & Communication (InfoComm)",
  "CPI: Recreation, Sport & Culture (RSC)",
  "CPI: Education",
  "CPI: Miscellaneous Goods & Services (MG&S)",
  "CPI: All Items Less Accommodation",
  "CPI: All Items Less Imputed Rentals For Housing"
)
parent_cats <- parent_cats[parent_cats %in% cpi_cols]

children_map <- list(
  "CPI: Food" = c(
    "CPI: Food",
    cpi_cols[cpi_cols != "CPI: Food" &
               (grepl("^CPI: Food", cpi_cols) | grepl("^CPI: (FFBSS|FBSS):", cpi_cols))]
  ),
  "CPI: Clothing and Footwear (C&F)" = c(
    "CPI: Clothing and Footwear (C&F)",
    cpi_cols[grepl("^CPI: C&F:", cpi_cols)]
  ),
  "CPI: Housing & Utilities" = c(
    "CPI: Housing & Utilities",
    cpi_cols[grepl("^CPI: Housing & Utilities:", cpi_cols)]
  ),
  "CPI: Household Durables & Services (HDS)" = c(
    "CPI: Household Durables & Services (HDS)",
    cpi_cols[grepl("^CPI: HDS:", cpi_cols)]
  ),
  "CPI: Health" = c(
    "CPI: Health",
    cpi_cols[grepl("^CPI: Health:", cpi_cols)]
  ),
  "CPI: Transport" = c(
    "CPI: Transport",
    cpi_cols[grepl("^CPI: Transport:", cpi_cols)]
  ),
  "CPI: Information & Communication (InfoComm)" = c(
    "CPI: Information & Communication (InfoComm)",
    cpi_cols[grepl("^CPI: InfoComm:", cpi_cols)]
  ),
  "CPI: Recreation, Sport & Culture (RSC)" = c(
    "CPI: Recreation, Sport & Culture (RSC)",
    cpi_cols[grepl("^CPI: RSC:", cpi_cols)]
  ),
  "CPI: Education" = c(
    "CPI: Education",
    cpi_cols[grepl("^CPI: Education:", cpi_cols)]
  ),
  "CPI: Miscellaneous Goods & Services (MG&S)" = c(
    "CPI: Miscellaneous Goods & Services (MG&S)",
    cpi_cols[grepl("^CPI: MG&S:", cpi_cols)]
  ),
  "CPI: All Items Less Accommodation"                = "CPI: All Items Less Accommodation",
  "CPI: All Items Less Imputed Rentals For Housing"  = "CPI: All Items Less Imputed Rentals For Housing"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f8f9fa; font-family: 'Segoe UI', sans-serif; }
    .main-title { font-size: 22px; font-weight: 700; color: #2c3e50;
                  margin: 20px 0 4px 0; }
    .sub-title  { font-size: 13px; color: #7f8c8d; margin-bottom: 20px; }
    .card       { background: white; border-radius: 8px;
                  box-shadow: 0 1px 4px rgba(0,0,0,.08);
                  padding: 20px; margin-bottom: 18px; }
    .section-label { font-size: 13px; font-weight: 600; color: #555;
                     text-transform: uppercase; letter-spacing: .5px;
                     margin-bottom: 10px; }
    .selectize-input { border-radius: 6px !important; }
  "))),

  div(class = "container-fluid", style = "max-width:1200px; margin:auto;",
    div(class = "main-title", "CPI Decomposition Explorer"),
    div(class = "sub-title",  "Singapore CPI · 2020–2025"),

    div(class = "card",
      fluidRow(
        column(4,
          selectInput("top_cat", "Category (Top-level)",
                      choices  = parent_cats,
                      selected = parent_cats[1],
                      width    = "100%")
        ),
        column(4,
          selectInput("sub_cat", "Sub-category",
                      choices  = children_map[[parent_cats[1]]],
                      selected = children_map[[parent_cats[1]]][1],
                      width    = "100%")
        ),
        column(4,
          selectInput("decomp_type", "Decomposition Type",
                      choices  = c("Additive" = "additive", "Multiplicative" = "multiplicative"),
                      selected = "additive",
                      width    = "100%")
        )
      )
    ),

    div(class = "card",
      div(class = "section-label", "Decomposition Plot"),
      plotOutput("decomp_plot", height = "450px")
    ),

    fluidRow(
      column(4,
        div(class = "card",
          div(class = "section-label", "Observed Series Summary"),
          DTOutput("table1")
        )
      ),
      column(8,
        div(class = "card",
          div(class = "section-label", "Component Summary"),
          DTOutput("table2")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Update sub-category choices when top-level changes
  observeEvent(input$top_cat, {
    choices <- children_map[[input$top_cat]]
    updateSelectInput(session, "sub_cat", choices = choices, selected = choices[1])
  })

  # Reactive decomposition (depends on category + decomp type)
  decomp_result <- reactive({
    req(input$sub_cat, input$decomp_type)
    decompose(ts_list[[input$sub_cat]], type = input$decomp_type)
  })

  # Reactive Table 2 (depends on decomp result)
  table2_reactive <- reactive({
    decomp      <- decomp_result()
    ts_data     <- ts_list[[input$sub_cat]]
    series_mean <- mean(ts_data, na.rm = TRUE)

    trend_vals    <- na.omit(as.numeric(decomp$trend))
    seasonal_vals <- na.omit(as.numeric(decomp$seasonal))
    residual_vals <- na.omit(as.numeric(decomp$random))

    trend_change   <- tail(trend_vals, 1) - head(trend_vals, 1)
    seasonal_range <- max(seasonal_vals) - min(seasonal_vals)
    residual_range <- max(residual_vals) - min(residual_vals)

    trend_ratio <- abs(trend_change) / series_mean

    is_mult <- input$decomp_type == "multiplicative"

    # Trend: same for both (in original units)
    trend_interp <- if (trend_ratio > 0.10) "Strong upward trend over the sample period." else
      if (trend_ratio > 0.03) "Moderate upward trend." else "Relatively stable long-term trend."

    if (is_mult) {
      # Multiplicative: seasonal/residual are multipliers ~1.0; range = peak-to-trough % swing
      seasonal_interp <- if (seasonal_range > 0.05) "Noticeable seasonal variation (>5% swing)." else
        if (seasonal_range > 0.02) "Mild seasonal pattern (2–5% swing)." else "Very weak seasonality (<2% swing)."
      residual_interp <- if (residual_range > 0.05) "Relatively high irregular fluctuations (>5% range)." else
        if (residual_range > 0.02) "Moderate irregular variation (2–5% range)." else
          "Series is relatively stable with small random noise (<2%)."
      seasonal_char <- paste0("Seasonal range = ", round(seasonal_range * 100, 2), "%")
      residual_char <- paste0("Residual range = ", round(residual_range * 100, 2), "%")
    } else {
      # Additive: normalise by mean to get relative ratios
      seasonal_ratio <- seasonal_range / series_mean
      residual_ratio <- residual_range / series_mean
      seasonal_interp <- if (seasonal_ratio > 0.02) "Noticeable seasonal variation." else
        if (seasonal_ratio > 0.01) "Mild seasonal pattern." else "Very weak seasonality."
      residual_interp <- if (residual_ratio > 0.03) "Relatively high irregular fluctuations." else
        if (residual_ratio > 0.01) "Moderate irregular variation." else
          "Series is relatively stable with small random noise."
      seasonal_char <- paste0("Seasonal range = ", round(seasonal_range, 3))
      residual_char <- paste0("Residual range = ", round(residual_range, 3))
    }

    data.frame(
      Component       = c("Trend", "Seasonality", "Residual"),
      Characteristics = c(
        paste0("Trend change = ", round(trend_change, 3)),
        seasonal_char,
        residual_char
      ),
      Interpretation  = c(trend_interp, seasonal_interp, residual_interp)
    )
  })

  output$decomp_plot <- renderPlot({
    decomp    <- decomp_result()
    type_label <- if (input$decomp_type == "additive") "Additive" else "Multiplicative"
    par(bg = "white", oma = c(0, 0, 2, 0))
    plot_decomp(decomp, col = "#2980b9")
    mtext(paste0(input$sub_cat, "  [", type_label, "]"),
          side = 3, outer = TRUE, line = 0.5, cex = 1.5, col = "#2c3e50", font = 2)
  }, bg = "white")

  dt_opts <- list(dom = "t", ordering = FALSE, pageLength = 20,
                  columnDefs = list(list(className = "dt-left", targets = "_all")))

  output$table1 <- renderDT({
    req(input$sub_cat)
    datatable(table1_list[[input$sub_cat]], rownames = FALSE, options = dt_opts)
  })

  output$table2 <- renderDT({
    datatable(table2_reactive(), rownames = FALSE, options = dt_opts)
  })
}

shinyApp(ui, server)
