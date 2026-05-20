library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

df <- read_csv(file.path("data", "fdi_gdp_sample.csv"), show_col_types = FALSE) %>%
  mutate(fdi_gdp_ratio = fdi_billion_usd / gdp_billion_usd * 100)

ui <- fluidPage(
  titlePanel("Canadian FDI GDP Time Series Modeling"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Year range", min(df$year), max(df$year), c(min(df$year), max(df$year))),
      checkboxGroupInput("series", "Series", c("FDI" = "fdi_billion_usd", "GDP" = "gdp_billion_usd"), selected = c("fdi_billion_usd", "gdp_billion_usd"))
    ),
    mainPanel(
      plotOutput("trend_plot"),
      plotOutput("ratio_plot"),
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    df %>% filter(year >= input$year_range[1], year <= input$year_range[2])
  })

  output$trend_plot <- renderPlot({
    plot_df <- filtered() %>%
      select(year, all_of(input$series)) %>%
      tidyr::pivot_longer(-year, names_to = "series", values_to = "value")
    ggplot(plot_df, aes(year, value, color = series)) +
      geom_line(linewidth = 1.1) +
      geom_point() +
      theme_minimal() +
      labs(title = "FDI and GDP Trend", x = NULL, y = "USD billions")
  })

  output$ratio_plot <- renderPlot({
    ggplot(filtered(), aes(year, fdi_gdp_ratio, fill = top_sector)) +
      geom_col() +
      theme_minimal() +
      labs(title = "FDI as Share of GDP", x = NULL, y = "Percent")
  })

  output$table <- renderDT({
    datatable(filtered(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
