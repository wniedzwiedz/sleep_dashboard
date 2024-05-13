library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #c5d4d9;
      }
    "))
  ),
  
  # Header
  fluidRow(
    column(
      width = 4,
      headerPanel(title = "What affects sleep quality?")
    ),
    column(
      width = 2,
      selectInput("occupation_filter", "Exclude Occupation:",
                  choices = unique(heatmap_data$Occupation),
                  multiple = TRUE)
    ),
    column(
      width = 2,
      selectInput("disorder_filter", "Exclude Sleep Disorder:",
                  choices = unique(owad_data1$Sleep.Disorder),
                  multiple = TRUE)
    ),
    column(
      width = 1,
      sliderInput("quality_filter", "Sleep Quality",
                  min = 0, max = 10, value = c(0, 10),
                  step = 1)
    ),
    column(
      width = 2,
      sliderInput("age_filter", "Age",
                  min = min(owad_data1$Age), max = max(owad_data1$Age), value = c(min(owad_data1$Age), max(owad_data1$Age)),
                  step = 1)
    ),
    column(
      width = 1,
      checkboxInput("show_trend", "Show trends?", value = TRUE)
    )
  ),
  
  # Main content area
  mainPanel(width = 12,
            column(5,
                   plotOutput("linePlot", height = "200px"),
                   br(),
                   plotOutput("scatterPlot", height = "430px")
            ),
            column(3,
                   plotOutput("barPlot", height = "650px")
            ),
            column(4,
                   plotOutput("heatMap", height = "650px")
            )
  )
)
