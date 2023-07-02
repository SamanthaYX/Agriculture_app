
# load packages
library(shiny)
library(readr)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(plotly) 
library(scales)
#library (lubridate)

# load the data
final_data <- read_csv("data/final_data.csv")

# Create list of named values for the input selection
vars <- c(
  "Annual value of agriculture ($US dollar)" = "annualvalue_agriculture_current_us",
  "Employment in agriculture (%)" = "employment_in_agriculture_percent")

# Create list of country names
countries <- final_data |>
  filter(year == max(year)) |>
  pull(country) |>
  sort()

# UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Annual Value and Employment(%) of Agriculture App"),
  helpText("This app is allow user to compare the Annual Value ($) and Employment(%) of Agriculture
              from 2011 - 2021 year in both line chart and bar chart. User also could change different 
              year to compare the data"),
  
  sidebarLayout(
    sidebarPanel(
      
      tags$h3("Indicator 1"),
      selectInput("indicator1", "Indicator:", vars),  
      selectInput("country1", "Country:", countries),
      
      tags$h3("Indicator 2"),
      selectInput("indicator2", "Indicator:", vars),  
      selectInput("country2", "Country:", countries),
      
      ###########################     
      sliderTextInput(
        "lineRange1",
        "Indicator 1: Line Chart 1 Date:",
        grid = TRUE,
        choices =2011:2021,
        selected = c(2011, 2021),
        width = "100%"
      ),
      
      sliderTextInput(
        "barRange1",
        "Indicator 1: Bar Chart Date:",
        grid = TRUE,
        choices =2011:2021,
        selected = c(2011, 2021),
        width = "100%"
      ),
      
      ###########################
      sliderTextInput(
        "lineRange2",
        "Indicator 2: Line Chart 1 Date:",
        grid = TRUE,
        choices =2011:2021,
        selected = c(2011, 2021),
        width = "100%"
      ),
      
      sliderTextInput(
        "barRange2",
        "Indicator 2: Bar Chart 2 Date:",
        grid = TRUE,
        choices =2011:2021,
        selected = c(2011, 2021),
        width = "100%"
      )),
    
    
    mainPanel(
      fluidRow(
        column(6,plotlyOutput("lineChart1")),
        column(6,plotlyOutput("barChart1"))),
      
      fluidRow(
        column(6,plotlyOutput("lineChart2")),
        column(6,plotlyOutput("barChart2"),
               helpText("The first row is indicator 1, The second row is indicator 2.
                Select an indicator and country, choose a date range and view the trend. 
                 The data for this app comes from the World bank dataset.")
        ),)
    )))


# Define server logic required to draw a line and bar chart
server <- function(input, output) {
  
  # select and filter based on selection
  ctry_indicator1 <- reactive({
    final_data |>
      filter(country == input$country1) |>
      select(year, input$indicator1)     
  })
  
  lineplot <- reactive({
    ctry_indicator1() |>
      filter(year %in% (input$lineRange1[1]:input$lineRange1[2])) 
  })
  
  barplot <- reactive({
    ctry_indicator1() |>
      filter(year %in% (input$barRange1[1]:input$barRange1[2])) 
  })
  
  ######################2
  ctry_indicator2 <- reactive({
    final_data |>
      filter(country == input$country2) |>
      select(year, input$indicator2)     
  })
  
  lineplot2 <- reactive({
    ctry_indicator2() |>
      filter(year %in% (input$lineRange2[1]:input$lineRange2[2])) 
  })
  
  barplot2 <- reactive({
    ctry_indicator2() |>
      filter(year %in% (input$barRange2[1]:input$barRange2[2])) 
  })
  
  ##################
  
  # Render line chart 1
  output$lineChart1 <- renderPlotly({
    vi_data <- ggplot(lineplot(), aes(x = year, y = get(input$indicator1))) +
      geom_line(color = "steelblue", size = 0.75) +  
      labs(
        x = "", 
        y = names(vars[which(vars == input$indicator1)])
      ) +                           
      theme_minimal() +       
      scale_y_continuous(labels = scales::comma) 
    
    ggplotly(vi_data) 
  })
  
  # Render bar chart 1
  output$barChart1 <- renderPlotly({
    vi_data <- ggplot(barplot(), aes(x = year, y = get(input$indicator1), fill = factor(year))) +
      geom_bar(stat = "identity", color = "steelblue", size = 0.5) +  
      labs(
        x = "", 
        y = names(vars[which(vars == input$indicator1)]),
      ) + 
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      guides(fill = FALSE)
    
    ggplotly(vi_data) 
  })
  
  ##################
  
  # Render line chart 2
  output$lineChart2 <- renderPlotly({
    vi_data <- ggplot(lineplot2(), aes(x = year, y = get(input$indicator2))) +
      geom_line(color = "steelblue", size = 0.75) +  
      labs(
        x = "", 
        y = names(vars[which(vars == input$indicator2)])
      ) +                           
      theme_minimal() +       
      scale_y_continuous(labels = scales::comma) 
    
    ggplotly(vi_data) 
  })
  
  # Render Bar chart 2  
  output$barChart2 <- renderPlotly({
    vi_data <- ggplot(barplot2(), aes(x = year, y = get(input$indicator2), fill = factor(year))) +
      geom_bar(stat = "identity", 
               color = "steelblue", 
               size = 0.5) +  
      labs(
        x = "", 
        y = names(vars[which(vars == input$indicator2)]),
      ) + 
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      guides(fill = FALSE)  
    
    ggplotly(vi_data) 
  })
} 

# Run the application 
shinyApp(ui = ui, server = server)
