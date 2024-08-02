library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(shinycssloaders)
library(plm)
library(corrplot)

# Load the data
data <- read_csv("Book2.csv", show_col_types = FALSE)

# Rename the columns for easier reference
colnames(data) <- c("Year", "Country", "GDP_Growth", "Trade_GDP", "Population_Growth", 
                    "Capital_GDP", "Labor_Force", "Economic_Freedom")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Economic Growth and Trade Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualization", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(4, selectInput(inputId = "Year", label = "Year", choices = unique(data$Year), selected = unique(data$Year)[1])),
                column(4, selectInput(inputId = "Metric1", label = "Indicators", choices = colnames(data)[3:length(colnames(data))], selected = colnames(data)[3])),
                column(4, selectInput(inputId = "Metric2", label = "Indicators", choices = colnames(data)[3:length(colnames(data))], selected = colnames(data)[4]))
              ),
              fluidRow(
                box(title = "Economic Analysis by Country", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, plotOutput("metricChart", height = "600px") %>% withSpinner(color = "#3c8dbc"))
              )
      ),
      tabItem(tabName = "analysis",
              h2("Analysis Section"),
              p("This section provides the results of a fixed-effects regression analysis and a correlation matrix."),
              fluidRow(
                column(12, 
                       box(title = "Fixed Effects Regression Analysis", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, 
                           actionButton("run_analysis", "Run Fixed Effects Regression"),
                           verbatimTextOutput("regression_results") %>% withSpinner(color = "#3c8dbc")
                       )
                )
              ),
              fluidRow(
                column(12, 
                       box(title = "Correlation Matrix", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, 
                           actionButton("run_correlation", "Generate Correlation Matrix"),
                           plotOutput("correlation_matrix") %>% withSpinner(color = "#3c8dbc")
                       )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive data filtered by selected year
  year_filtered_data <- reactive({
    req(input$Year)
    data %>% filter(Year == input$Year)
  })
  
  output$metricChart <- renderPlot({
    req(year_filtered_data())
    
    # Gather data into long format for ggplot
    plot_data <- year_filtered_data() %>%
      pivot_longer(cols = c(input$Metric1, input$Metric2), names_to = "Metric", values_to = "Value")
    
    # Define dynamic colors for the metrics
    all_metrics <- colnames(data)[3:length(colnames(data))]
    colors <- rainbow(length(all_metrics))
    metric_colors <- setNames(colors, all_metrics)
    
    ggplot(plot_data, aes(x = Country, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      scale_fill_manual(values = metric_colors) +
      labs(title = paste("Trend Analysis:", input$Metric1, "vs", input$Metric2),
           x = "Country",
           y = "Value",
           fill = "Metric") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold")
      )
  })
  
  # Reactive expression to run the fixed-effects regression
  regression_results <- eventReactive(input$run_analysis, {
    pdata <- pdata.frame(data, index = c("Country", "Year"))
    fixed_model <- plm(GDP_Growth ~ Trade_GDP + Population_Growth + Capital_GDP + Labor_Force + Economic_Freedom, 
                       data = pdata, model = "within")
    summary(fixed_model)
  })
  
  # Output the regression results
  output$regression_results <- renderPrint({
    regression_results()
  })
  
  # Reactive expression to generate the correlation matrix
  correlation_matrix <- eventReactive(input$run_correlation, {
    numeric_data <- data %>% select(GDP_Growth, Trade_GDP, Population_Growth, Capital_GDP, Labor_Force, Economic_Freedom)
    cor(numeric_data, use = "complete.obs")
  })
  
  # Output the correlation matrix plot
  output$correlation_matrix <- renderPlot({
    corr_matrix <- correlation_matrix()
    corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
  })
}

# Run the application
shinyApp(ui = ui, server = server)