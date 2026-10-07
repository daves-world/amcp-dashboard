# =============================================================================
# AMCP Ltd - Executive Performance Dashboard
# R Shiny replica of a 6-page Power BI report (Akan Manufacturing & Consumer
# Products Ltd). KPIs are grouped by strategic dimension and tracked against
# targets for 2024-2026.
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)

# ---- Data -------------------------------------------------------------------
monthly <- read.csv("data/monthly_data.csv", check.names = FALSE, stringsAsFactors = FALSE)
monthly$Date <- as.Date(monthly$Date)

kpi <- read.csv("data/kpi_targets.csv", check.names = FALSE, stringsAsFactors = FALSE)

rev_prod <- read.csv("data/revenue_by_product.csv", check.names = FALSE, stringsAsFactors = FALSE)
rev_prod$Date <- as.Date(rev_prod$Date)

kpi_dict <- read.csv("data/kpi_dictionary.csv", check.names = FALSE, stringsAsFactors = FALSE)

# ---- Look & feel ------------------------------------------------------------
status_levels <- c("On Target", "Watch", "Attention", "Critical", "N/A (context metric)")
status_cols   <- c("On Target"  = "#00C821", "Watch" = "#F2C811", "Attention" = "#FD7E14",
                   "Critical"   = "#B90000", "N/A (context metric)" = "#A6A6A6")
status_text   <- c("white", "black", "white", "white", "white")
line_pal      <- c("#118DFF", "#12239E", "#E66C37")

theme <- bs_theme(version = 5, primary = "#0B2545", "body-bg" = "#F4F6FA",
                  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                                              "Helvetica Neue", "Arial", "sans-serif"))

# ---- Formatting helpers -----------------------------------------------------
fmt_num <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_pct <- function(x) paste0(fmt_num(x, 1), "%")
fmt_compact <- function(x) {
  if (is.na(x)) return("—")
  a <- abs(x)
  if (a >= 1e9) paste0(fmt_num(x / 1e9, 2), "B")
  else if (a >= 1e6) paste0(fmt_num(x / 1e6, 1), "M")
  else if (a >= 1e4) paste0(fmt_num(x / 1e3, 1), "K")
  else fmt_num(x, 1)
}
# Table values span GHS millions to 0.0003 intensities, so format per value
fmt_kpi <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) "—"
    else if (abs(v) >= 1000) formatC(v, format = "f", digits = 0, big.mark = ",")
    else format(signif(v, 4), scientific = FALSE, drop0trailing = TRUE)
  }, character(1))
}

# ---- Reusable UI / plot builders --------------------------------------------
make_cards <- function(items) {
  boxes <- lapply(items, function(i) value_box(title = i$t, value = i$v, theme = "primary", height = "110px"))
  do.call(layout_column_wrap, c(list(width = "170px", fill = FALSE), unname(boxes)))
}

plot_card <- function(id, height = "340px") {
  card(full_screen = TRUE, plotlyOutput(id, height = height))
}

table_card <- function(id, title = NULL) {
  card(full_screen = TRUE, if (!is.null(title)) card_header(title), DTOutput(id))
}

style_plot <- function(p, title, ytitle = NULL) {
  p |>
    layout(title = list(text = title, x = 0.01, font = list(size = 15)),
           xaxis = list(title = ""), yaxis = list(title = if (is.null(ytitle)) "" else ytitle),
           legend = list(orientation = "h", y = -0.2),
           margin = list(t = 50, b = 40), hovermode = "x unified") |>
    config(displayModeBar = FALSE)
}

line_chart <- function(df, cols, title, ytitle = NULL) {
  p <- plot_ly()
  for (i in seq_along(cols)) {
    p <- add_trace(p, x = df$Date, y = df[[cols[i]]], type = "scatter", mode = "lines+markers",
                   name = cols[i], line = list(color = line_pal[i], width = 2.5),
                   marker = list(color = line_pal[i], size = 5))
  }
  style_plot(p, title, ytitle) |> layout(showlegend = length(cols) > 1)
}

status_donut <- function(dimension, title) {
  d <- kpi |>
    filter(Dimension == dimension) |>
    count(Status) |>
    mutate(Status = factor(Status, levels = status_levels)) |>
    arrange(Status)
  plot_ly(d, labels = ~Status, values = ~n, type = "pie", hole = 0.55, sort = FALSE,
          marker = list(colors = unname(status_cols[as.character(d$Status)])),
          textinfo = "value", hovertemplate = "%{label}: %{value} KPIs<extra></extra>") |>
    layout(title = list(text = title, x = 0.01, font = list(size = 15)),
           legend = list(orientation = "h", y = -0.1), margin = list(t = 50)) |>
    config(displayModeBar = FALSE)
}

kpi_table <- function(dimension) {
  d <- kpi |>
    filter(Dimension == dimension) |>
    transmute(KPI,
              `2024` = fmt_kpi(`2024 Actual`),
              `2025` = fmt_kpi(`2025 Actual`),
              `2026` = fmt_kpi(`2026 Actual`),
              Target = fmt_kpi(Target),
              `Ach. %` = round(`Achievement %`, 1),
              Status)
  datatable(d, rownames = FALSE, class = "compact stripe",
            options = list(dom = "t", paging = FALSE, scrollY = "300px", scrollX = TRUE, scrollCollapse = TRUE,
                           columnDefs = list(list(className = "dt-right", targets = 1:5)))) |>
    formatStyle("Status",
                backgroundColor = styleEqual(status_levels, unname(status_cols)),
                color = styleEqual(status_levels, status_text), fontWeight = "bold")
}

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "AMCP Ltd | Executive Performance Dashboard",
  theme = theme, fillable = FALSE, bg = "#0B2545", inverse = TRUE,
  header = tags$style(HTML("
    table.dataTable { font-size: 0.8rem; }
    table.dataTable th, table.dataTable td { padding: 4px 8px !important; white-space: nowrap; }
    .value-box-title { font-size: 0.8rem; }
    .value-box-value { font-size: 1.6rem; }
  ")),
  sidebar = sidebar(
    width = 230,
    checkboxGroupInput("years", "Year", choices = 2024:2026, selected = 2024:2026),
    helpText("Filters the monthly charts and KPI cards. KPI scorecard tables always show all years against target."),
    hr(),
    tags$small(class = "text-muted",
               "Akan Manufacturing & Consumer Products Ltd (AMCP). Monthly KPIs, Jan 2024 - Dec 2026.")
  ),

  nav_panel("Financial", div(class = "py-2",
    uiOutput("fin_cards"),
    layout_columns(col_widths = 12, plot_card("fin_status", "280px"), plot_card("fin_growth", "320px")),
    layout_columns(col_widths = c(6, 6), plot_card("fin_product"), plot_card("fin_npm"))
  )),

  nav_panel("Operations", div(class = "py-2",
    uiOutput("ops_cards"),
    layout_columns(col_widths = c(6, 6),
      plot_card("ops_downtime"), plot_card("ops_donut"),
      plot_card("ops_scatter"),  plot_card("ops_ontime"))
  )),

  nav_panel("Customer & Market", div(class = "py-2",
    uiOutput("cus_cards"),
    layout_columns(col_widths = c(6, 6),
      plot_card("cus_complaints"), plot_card("cus_donut"),
      plot_card("cus_share"),      table_card("cus_table", "KPI scorecard (actuals by year vs target)"))
  )),

  nav_panel("Sustainability", div(class = "py-2",
    uiOutput("sus_cards"),
    layout_columns(col_widths = c(6, 6),
      plot_card("sus_ghg"),       plot_card("sus_donut"),
      plot_card("sus_training"),  table_card("sus_table", "KPI scorecard (actuals by year vs target)"))
  )),

  nav_panel("People", div(class = "py-2",
    uiOutput("ppl_cards"),
    layout_columns(col_widths = c(6, 6),
      plot_card("ppl_absent"),    plot_card("ppl_donut"),
      plot_card("ppl_training"),  table_card("ppl_table", "KPI scorecard (actuals by year vs target)"))
  )),

  nav_panel("Governance & Risk", div(class = "py-2",
    uiOutput("gov_cards"),
    layout_columns(col_widths = c(6, 6),
      plot_card("gov_risk"),      plot_card("gov_donut"),
      plot_card("gov_privacy"),   table_card("gov_table", "KPI scorecard (actuals by year vs target)"))
  )),

  nav_panel("KPI Dictionary", div(class = "py-2",
    card(full_screen = TRUE, card_header("KPI definitions, direction and strategic objective"),
         DTOutput("dict_table"))
  ))
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  md <- reactive({
    validate(need(length(input$years) > 0, "Select at least one year in the sidebar."))
    monthly |> filter(Year %in% as.integer(input$years))
  })
  rp <- reactive(rev_prod |> filter(Year %in% as.integer(input$years)))

  avg <- function(d, col) mean(d[[col]], na.rm = TRUE)

  # ---- Financial
  output$fin_cards <- renderUI({
    d <- md()
    make_cards(list(
      list(t = "Revenue (GHS)",          v = fmt_compact(sum(d$Revenue))),
      list(t = "Avg Revenue Growth",     v = fmt_pct(avg(d, "Revenue Growth"))),
      list(t = "Avg Gross Profit Margin", v = fmt_pct(avg(d, "Gross Profit Margin"))),
      list(t = "Avg Working Capital Ratio", v = fmt_num(avg(d, "Working Capital Ratio"), 2)),
      list(t = "Avg Net Profit Margin",  v = fmt_pct(avg(d, "Net Profit Margin"))),
      list(t = "Avg EBITDA Margin",      v = fmt_pct(avg(d, "EBITDA Margin")))
    ))
  })
  output$fin_status <- renderPlotly({
    d <- kpi |> filter(Dimension == "Financial Performance") |>
      mutate(Status = factor(Status, levels = status_levels))
    plot_ly(d, x = ~KPI, y = ~`Achievement %`, color = ~Status, colors = status_cols, type = "bar",
            hovertemplate = "%{x}<br>Achievement: %{y:.1f}%<extra></extra>") |>
      layout(title = list(text = "Health Status of Financial KPIs (achievement vs 2026 target, %)", x = 0.01,
                          font = list(size = 15)),
             xaxis = list(title = "", categoryorder = "array", categoryarray = d$KPI),
             yaxis = list(title = "Achievement %"), legend = list(orientation = "h", y = -0.25),
             shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 100, y1 = 100,
                                line = list(dash = "dash", color = "#555", width = 1))),
             margin = list(t = 50)) |>
      config(displayModeBar = FALSE)
  })
  output$fin_growth <- renderPlotly(line_chart(md(), "Revenue Growth", "Monthly Revenue Growth (%)", "%"))
  output$fin_product <- renderPlotly({
    d <- rp() |> group_by(category = `Product Category`) |>
      summarise(revenue = sum(`Revenue (GHS)`), .groups = "drop") |> arrange(desc(revenue))
    plot_ly(x = factor(d$category, levels = d$category), y = d$revenue, type = "bar",
            marker = list(color = line_pal[1]),
            hovertemplate = "%{x}<br>GHS %{y:,.0f}<extra></extra>") |>
      style_plot("Revenue by Product Category (GHS)", "GHS") |> layout(hovermode = "closest")
  })
  output$fin_npm <- renderPlotly(line_chart(md(), "Net Profit Margin", "Net Profit Margin (%)", "%"))

  # ---- Operations
  output$ops_cards <- renderUI({
    d <- md()
    make_cards(list(
      list(t = "Avg Capacity Utilisation", v = fmt_pct(avg(d, "Capacity Utilisation"))),
      list(t = "Avg Production Volume (units)", v = fmt_compact(avg(d, "Production Volume"))),
      list(t = "Avg Production Yield",     v = fmt_pct(avg(d, "Production Yield"))),
      list(t = "Avg Defect Rate",          v = fmt_pct(avg(d, "Defect Rate"))),
      list(t = "Avg Downtime (hours)",     v = fmt_num(avg(d, "Production Downtime"), 1)),
      list(t = "Avg OEE",                  v = fmt_pct(avg(d, "Overall Equipment Effectiveness (OEE)")))
    ))
  })
  output$ops_downtime <- renderPlotly(line_chart(md(), "Production Downtime", "Production Downtime (hours)", "hours"))
  output$ops_donut    <- renderPlotly(status_donut("Operational Performance", "Health Status of Operational KPIs"))
  output$ops_scatter  <- renderPlotly({
    d <- md()
    plot_ly(d, x = ~`Defect Rate`, y = ~`Production Yield`, color = factor(d$Year), colors = line_pal,
            type = "scatter", mode = "markers", marker = list(size = 10, opacity = 0.85),
            text = format(d$Date, "%b %Y"),
            hovertemplate = "%{text}<br>Defect rate: %{x:.1f}%<br>Yield: %{y:.1f}%<extra></extra>") |>
      layout(title = list(text = "Production Yield vs Defect Rate", x = 0.01, font = list(size = 15)),
             xaxis = list(title = "Defect Rate (%)"), yaxis = list(title = "Production Yield (%)"),
             legend = list(orientation = "h", y = -0.2), margin = list(t = 50)) |>
      config(displayModeBar = FALSE)
  })
  output$ops_ontime <- renderPlotly(line_chart(md(), "On-Time Production", "On-Time Production (%)", "%"))

  # ---- Customer & Market
  output$cus_cards <- renderUI({
    d <- md()
    make_cards(list(
      list(t = "Total Complaints",        v = fmt_num(sum(d$`Customer Complaints`), 0)),
      list(t = "Avg Customer Satisfaction", v = fmt_num(avg(d, "Customer Satisfaction"), 1)),
      list(t = "Avg Customer Retention",  v = fmt_pct(avg(d, "Customer Retention"))),
      list(t = "Avg Net Promoter Score",  v = fmt_num(avg(d, "Net Promoter Score"), 1)),
      list(t = "Avg Market Share",        v = fmt_pct(avg(d, "Market Share"))),
      list(t = "Avg On-Time Delivery",    v = fmt_pct(avg(d, "On-Time Delivery")))
    ))
  })
  output$cus_complaints <- renderPlotly(line_chart(md(), "Customer Complaints", "Customer Complaints (count)", "count"))
  output$cus_donut <- renderPlotly(status_donut("Customer & Market", "Health Status of Customer & Market KPIs"))
  output$cus_share <- renderPlotly(line_chart(md(), "Market Share", "Market Share (%)", "%"))
  output$cus_table <- renderDT(kpi_table("Customer & Market"))

  # ---- Sustainability
  output$sus_cards <- renderUI({
    d <- md()
    make_cards(list(
      list(t = "Scope 1 GHG (tCO2e)",     v = fmt_compact(sum(d$`Scope 1 GHG Emissions`))),
      list(t = "Scope 2 GHG (tCO2e)",     v = fmt_compact(sum(d$`Scope 2 GHG Emissions`))),
      list(t = "Energy Consumption (MWh)", v = fmt_compact(sum(d$`Energy Consumption`))),
      list(t = "Avg Renewable Energy Share", v = fmt_pct(avg(d, "Renewable Energy Share"))),
      list(t = "Water Consumption (m³)",  v = fmt_compact(sum(d$`Water Consumption`))),
      list(t = "Avg Waste Recycling Rate", v = fmt_pct(avg(d, "Waste Recycling Rate")))
    ))
  })
  output$sus_ghg <- renderPlotly(
    line_chart(md(), c("Scope 1 GHG Emissions", "Scope 2 GHG Emissions"), "GHG Emissions (tCO2e)", "tCO2e"))
  output$sus_donut <- renderPlotly(status_donut("Sustainability", "Health Status of Sustainability KPIs"))
  output$sus_training <- renderPlotly(line_chart(md(), "Employee Training Hours", "Employee Training Hours", "hours"))
  output$sus_table <- renderDT(kpi_table("Sustainability"))

  # ---- People & Human Capital
  output$ppl_cards <- renderUI({
    d <- md()
    make_cards(list(
      list(t = "Avg Total Employees",     v = fmt_num(avg(d, "Total Employees"), 0)),
      list(t = "Avg Training Hrs / Employee", v = fmt_num(avg(d, "Training Hours per Employee"), 1)),
      list(t = "Avg Employee Turnover",   v = fmt_pct(avg(d, "Employee Turnover"))),
      list(t = "Avg Employee Engagement", v = fmt_pct(avg(d, "Employee Engagement"))),
      list(t = "Avg Female Representation", v = fmt_pct(avg(d, "Female Workforce Representation"))),
      list(t = "Avg Productivity (units/employee)", v = fmt_num(avg(d, "Employee Productivity"), 0))
    ))
  })
  output$ppl_absent <- renderPlotly(line_chart(md(), "Employee Absenteeism", "Employee Absenteeism (%)", "%"))
  output$ppl_donut <- renderPlotly(status_donut("People & Human Capital", "Health Status of People & Human Capital KPIs"))
  output$ppl_training <- renderPlotly(line_chart(md(), "Training Hours per Employee", "Training Hours per Employee", "hours"))
  output$ppl_table <- renderDT(kpi_table("People & Human Capital"))

  # ---- Governance & Risk
  output$gov_cards <- renderUI({
    d <- md()
    make_cards(list(
      list(t = "Data Privacy Incidents",  v = fmt_num(sum(d$`Data Privacy Incidents`), 0)),
      list(t = "Avg Board Attendance",    v = fmt_pct(avg(d, "Board Meeting Attendance"))),
      list(t = "Avg Ethics Training Completion", v = fmt_pct(avg(d, "Ethics Training Completion"))),
      list(t = "Avg Supplier Compliance", v = fmt_pct(avg(d, "Supplier Compliance Rate"))),
      list(t = "Avg Regulatory Compliance", v = fmt_pct(avg(d, "Regulatory Compliance Rate"))),
      list(t = "Issues Closed on Time",   v = fmt_num(sum(d$`Issues Closed on Time (count)`), 0))
    ))
  })
  output$gov_risk <- renderPlotly(line_chart(md(), "High-Risk Issues Outstanding", "High-Risk Issues Outstanding", "count"))
  output$gov_donut <- renderPlotly(status_donut("Governance & Risk", "Health Status of Governance & Risk KPIs"))
  output$gov_privacy <- renderPlotly(line_chart(md(), "Data Privacy Incidents", "Data Privacy Incidents", "count"))
  output$gov_table <- renderDT(kpi_table("Governance & Risk"))

  # ---- KPI dictionary
  output$dict_table <- renderDT(
    datatable(kpi_dict, rownames = FALSE, filter = "top", class = "compact stripe",
              options = list(pageLength = 15, scrollX = TRUE))
  )
}

shinyApp(ui, server)
