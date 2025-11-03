# ================================================
# EXECUTIVE DASHBOARD - FINAL, TABBED & VISUAL
# ================================================

library(shiny)
library(dplyr)
library(readxl)
library(DT)
library(ggplot2)

# ==================== DATA PREPARATION ====================
sbd <- read_excel("Part3_casestudy_salesbonus.xlsx")

sbd <- sbd %>%
  mutate(
    time = time - 1,
    bonus_period = ifelse(time == 0, "Before Bonus", "After Bonus"),
    type_clean = case_when(
      type == "." ~ "Others",
      type == "accessory" ~ "Accessories",
      type == "electronic device" ~ "Devices",
      type == "insurance + device" ~ "Insurance",
      TRUE ~ type
    )
  )

# Program-level metrics
program_metrics <- sbd %>%
  group_by(bonus_period) %>%
  summarise(
    avg_margin = mean(sales_profitmargin),
    sales_volume = n(),
    .groups = 'drop'
  )

# Product mix before and after
product_mix <- sbd %>%
  group_by(bonus_period, type_clean) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(bonus_period) %>%
  mutate(share = count / sum(count) * 100) %>%
  ungroup()

# Store performance
store_metrics <- sbd %>%
  group_by(store, bonus_period) %>%
  summarise(
    avg_margin = mean(sales_profitmargin),
    sales_count = n(),
    .groups = 'drop'
  ) %>%
  mutate(store_label = paste0("Store ", store))

# ==================== UI ====================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', sans-serif; background-color: #f5f7fa; }
      .kpi-box { background: white; padding: 18px; border-radius: 8px; text-align: center;
                 box-shadow: 0 1px 4px rgba(0,0,0,0.06); }
      .kpi-value { font-size: 32px; font-weight: 700; color: #2c3e50; }
      .kpi-label { font-size: 13px; color: #7f8c8d; }
      .interpretation-box { background: #fffbe6; border-left: 4px solid #f39c12; padding: 14px;
                            border-radius: 6px; margin-top: 18px; font-size: 14px; }
      .chart-container { background: white; padding: 18px; border-radius: 8px;
                         box-shadow: 0 1px 4px rgba(0,0,0,0.06); }
    "))
  ),
  
  titlePanel("Sales Bonus Program: Performance Review"),
  
  tabsetPanel(
    type = "tabs",
    
    # --- Tab 1: KPIs ---
    tabPanel("Program KPIs",
             br(),
             fluidRow(
               column(6, div(class = "kpi-box",
                             div(class = "kpi-value", textOutput("kpi_margin_value")),
                             div(class = "kpi-label", "Average Profit Margin (After Bonus)")
               )),
               column(6, div(class = "kpi-box",
                             div(class = "kpi-value", textOutput("kpi_volume_value")),
                             div(class = "kpi-label", "Total Sales Volume (After Bonus)")
               ))
             ),
             div(class = "interpretation-box",
                 h4("Findings"),
                 uiOutput("auto_interpretation")
             )
    ),
    
    # --- Tab 2: Charts ---
    tabPanel("Performance Charts",
             br(),
             fluidRow(
               column(6,
                      div(class = "chart-container",
                          h4("Profit Margin: Before vs. After"),
                          plotOutput("margin_chart")
                      )
               ),
               column(6,
                      div(class = "chart-container",
                          h4("Sales Volume: Before vs. After"),
                          plotOutput("volume_chart")
                      )
               )
             )
    ),
    
    # --- Tab 3: Product Mix ---
    tabPanel("Product Mix",
             br(),
             div(class = "chart-container",
                 h4("Product Mix Share: Before vs. After"),
                 plotOutput("product_mix_chart")
             )
    ),
    
    # --- Tab 4: Data Tables ---
    tabPanel("Store Performance",
             br(),
             h4("Store Metrics"),
             DTOutput("store_table")
    )
  )
)

# ==================== SERVER ====================
server <- function(input, output, session) {
  
  # KPI values
  output$kpi_margin_value <- renderText({
    margin_after <- program_metrics$avg_margin[program_metrics$bonus_period == "After Bonus"]
    paste0(round(margin_after, 2), "%")
  })
  
  output$kpi_volume_value <- renderText({
    format(program_metrics$sales_volume[program_metrics$bonus_period == "After Bonus"], big.mark = ",")
  })
  
  # Automated Interpretation
  output$auto_interpretation <- renderUI({
    before <- program_metrics %>% filter(bonus_period == "Before Bonus")
    after  <- program_metrics %>% filter(bonus_period == "After Bonus")
    margin_change <- after$avg_margin - before$avg_margin
    volume_change <- after$sales_volume - before$sales_volume
    margin_pct <- (margin_change / before$avg_margin) * 100
    volume_pct <- (volume_change / before$sales_volume) * 100
    
    HTML(paste0(
      "The bonus program has delivered measurable results:<br>",
      "• <strong>Profit Margin</strong> increased from ", round(before$avg_margin, 2), "% to ", 
      round(after$avg_margin, 2), "% (+", round(margin_pct, 1), "% improvement, +",
      round(margin_change, 2), "pp).<br>",
      "• <strong>Sales Volume</strong> grew from ", format(before$sales_volume, big.mark = ","), 
      " to ", format(after$sales_volume, big.mark = ","), " transactions (+", round(volume_pct, 1), "% growth)."
    ))
  })
  
  # Profit Margin Chart
  output$margin_chart <- renderPlot({
    ggplot(program_metrics, aes(x = bonus_period, y = avg_margin, fill = bonus_period)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(avg_margin, 2), "%")), vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Before Bonus" = "#95a5a6", "After Bonus" = "#27ae60")) +
      ylim(0, max(program_metrics$avg_margin) * 1.15) +
      labs(y = "Average Profit Margin (%)", x = "") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(size = 12))
  })
  
  # Sales Volume Chart
  output$volume_chart <- renderPlot({
    ggplot(program_metrics, aes(x = bonus_period, y = sales_volume, fill = bonus_period)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = format(sales_volume, big.mark = ",")), vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Before Bonus" = "#95a5a6", "After Bonus" = "#27ae60")) +
      ylim(0, max(program_metrics$sales_volume) * 1.15) +
      labs(y = "Total Sales Volume", x = "") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(size = 12))
  })
  
  # Product Mix Chart (Before vs After)
  output$product_mix_chart <- renderPlot({
    ggplot(product_mix, aes(x = bonus_period, y = share, fill = type_clean)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(share, 1), "%")), position = position_stack(vjust = 0.5), size = 4) +
      scale_fill_manual(values = c("Accessories" = "#e74c3c", "Devices" = "#3498db", 
                                   "Insurance" = "#9b59b6", "Others" = "#95a5a6")) +
      labs(y = "Share (%)", x = "", fill = "Product Type") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(size = 12), legend.position = "bottom")
  })
  
  # Store Performance Table
  output$store_table <- renderDT({
    # Create wide format: Store | Before Margin | After Margin | Change | Before Volume | After Volume | Volume Change
    store_before <- store_metrics %>%
      filter(bonus_period == "Before Bonus") %>%
      arrange(store)
    
    store_after <- store_metrics %>%
      filter(bonus_period == "After Bonus") %>%
      arrange(store)
    
    dt <- data.frame(
      Store = store_before$store_label,
      Before_Margin_Pct = round(store_before$avg_margin, 2),
      After_Margin_Pct = round(store_after$avg_margin, 2),
      Margin_Change_Pct = round(store_after$avg_margin - store_before$avg_margin, 2),
      Before_Volume = store_before$sales_count,
      After_Volume = store_after$sales_count,
      Volume_Change = store_after$sales_count - store_before$sales_count
    )
    
    # Color-coding function for DT
    datatable(dt, options = list(pageLength = 15, dom = 'tip')) %>%
      formatRound(columns = c(2, 3, 4), digits = 2) %>%
      formatStyle(
        'After_Margin_Pct',
        backgroundColor = styleInterval(
          c(12, 14, 16),
          c('#fee2e2', '#fef3c7', '#d1fae5', '#dcfce7')
        )
      ) %>%
      formatStyle(
        'Margin_Change_Pct',
        backgroundColor = styleInterval(
          c(-0.01, 0.01, 1),
          c('#fee2e2', '#fef3c7', '#d1fae5', '#dcfce7')
        )
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
