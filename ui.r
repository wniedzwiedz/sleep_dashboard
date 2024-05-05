#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("What affects sleep quality?"),
    

    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width=2,
          selectInput("disorder_filter",
                      "Exclude Sleep Disorder:",
                      choices = unique(owad_data1$Sleep.Disorder),
                      multiple = TRUE),
          selectInput("occupation_filter", "Exclude Occupation:",
                      choices = unique(heatmap_data$Occupation),
                      multiple = TRUE),
          checkboxInput("show_trend","Show trends?",value=TRUE)
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(width=10,
          fluidRow(column(6, plotOutput("linePlot")),
                   column(6, plotOutput("scatterPlot"))),
          
          fluidRow(column(6, plotOutput("barPlot")),
                   column(6, plotOutput("heatMap")))
        )
    )
)
