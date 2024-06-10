library(shiny)

fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e9f1f5;
        font-family: Arial, sans-serif;
      }
      .header-title {
        font-size: 36px;
        font-weight: bold;
        color: #4f6d7a;
        margin-top: 10px;
        margin-left: 20px;
        text-shadow: 2px 2px 2px #ccc;
        text-align: center;
      }
      .filter-title {
        font-weight: bold;
        margin-top: 10px;
      }
      .select-input {
        font-size: 11px;
      }
      

      .slider-input {
        padding-top: 10px;
      }
      .action-button {
        font-size: 14px;
        font-weight: bold;
        color: #4f6d7a;
        background-color: #d8e3e9;
        border: none;
        border-radius: 5px;
        text-align: center;
        text-decoration: none;
        transition-duration: 0.4s;
        box-shadow: 2px 2px 5px #888888;
      }

      .action-button:hover {
        background-color: #bed6e1;
      }

      .action-button:active {
        background-color: #a3c9d3;
      }

      .plot-output {
        border: 1px solid #ccc;
        background-color: #fff;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 2px 2px 5px #888888;
        transition: background-color 0.3s;
      }
      
      .plot-output:hover {
        background-color: #f0f0f0;
      }
      
      .slider-input {
        margin-top: 10px;
        text-align: center;
      }
      .slider-input .irs-bar {
        background-color: #4f6d7a;
        border-radius: 10px;
      }
      .slider-input .irs-handle {
        background-color: #fff;
        border: 2px solid #4f6d7a;
      }
    "))
  ),
  
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
                                              multiple = TRUE, selected = unique(owad_data$Occupation)),
          inline = TRUE)
    ),
    column(
      width = 1,
      div(class = "filter-title", "Selected disorders:"),
      div(class = "select-input", checkboxGroupInput("disorder_filter", "",
                                                     choices = unique(owad_data$Sleep.Disorder),
                                                     selected = unique(owad_data$Sleep.Disorder)))
    ),
    column(
      width = 2,
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
      div(class = "action-button", actionButton("reset_filters", "Reset")),
      div(class = "checkbox-input", checkboxInput("show_trend", "Show trends?", value = TRUE))
    )
  ),
  
  mainPanel(width = 12,
            fluidRow(
              column(width = 5,
                     div(class = "plot-output", plotOutput("linePlot", height = "20vh", brush = brushOpts(id = "linePlot_brush"), click = "linePlot_click")),
                     br(),
                     div(class = "plot-output", plotOutput("scatterPlot", height = "47vh", brush = brushOpts(id = "scatterPlot_brush"), click = "scatterPlot_click"))
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
