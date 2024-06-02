library(shiny)

fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e9f1f5;
        font-family: Arial, sans-serif;
      }
      .header-title {
        font-size: 30px;
        font-weight: bold;
        color: #4f6d7a;
        margin-top: 20px;
        margin-left: 20px;
        
      }
      .filter-title {
      font-weight: bold;
        margin-top: 10px;
      }
      
      .select-input {
      font-size: 10px;
      padding: 0px;
      height: 110px;
      }
      
      .slider-input, .action-button {
      margin-top: 10px;
      }

      .plot-output {
        border: 1px solid #ccc;
        background-color: #fff;
        padding: 10px;
        border-radius: 5px;
      }

    "))
  ),
  
  # Header
  fluidRow(
    column(
      width = 3,
      
      div(class = "header-title", "What affects sleep quality?")
    ),
    column(
      width = 3,
      div(class = "filter-title", "Selected occupations:"),
      div(class = "select-input", selectInput("occupation_filter", "",
                                              choices = unique(owad_data$Occupation),
                                              multiple = TRUE,selected = unique(owad_data$Occupation)))
    ),
    column(
      width = 2,
      div(class = "filter-title", "Selected disorders:"),
      div(class = "select-input", selectInput("disorder_filter", "",
                                              choices = unique(owad_data$Sleep.Disorder),
                                              multiple = TRUE, selected = unique(owad_data$Sleep.Disorder)))
    ),
    column(
      width = 1,
      div(class = "slider-input", sliderInput("quality_filter", "Sleep Quality",
                                              min = 0, max = 10, value = c(0, 10),
                                              step = 1))
    ),
    column(
      width = 2,
      div(class = "slider-input", sliderInput("age_filter", "Age",
                                              min = min(owad_data$Age), max = max(owad_data$Age), value = c(min(owad_data$Age), max(owad_data$Age)),
                                              step = 1))
    ),
    column(
      width = 1,
      div(class = "action-button", actionButton("reset_filters", "Reset Filters")),
      div(class = "checkbox-input", checkboxInput("show_trend", "Show trends?", value = TRUE))
    )
  ),
  
  # Main content area
  mainPanel(width = 12,
            fluidRow(
              column(width = 5,
                     div(class = "plot-output", plotOutput("linePlot", height = "30vh", brush = brushOpts(id = "linePlot_brush"), click = "linePlot_click")),
                     br(),
                     div(class = "plot-output", plotOutput("scatterPlot", height = "40vh", brush = brushOpts(id = "scatterPlot_brush"), click = "scatterPlot_click"))
              ),
              column(width = 3,
                     div(class = "plot-output", plotOutput("barPlot", height = "75vh", click = "barPlot_click", brush = brushOpts(id = "barPlot_brush")))
              ),
              column(width = 4,
                     div(class = "plot-output", plotOutput("heatMap", height = "75vh", click = "heatMap_click", brush = brushOpts(id = "heatMap_brush")))
              )
            )
  )
)
