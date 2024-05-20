library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {
  
  # Reactive expression to filter data based on input
  filtered_owad_data <- reactive({
    owad_data %>%
      filter(!Occupation %in% input$occupation_filter,
             !Sleep.Disorder %in% input$disorder_filter,
             Age >= input$age_filter[1] & Age <= input$age_filter[2],
             Quality.of.Sleep >= input$quality_filter[1] & Quality.of.Sleep <= input$quality_filter[2])
  })
  
  # Reactive expression to create line plot data
  line_plot_data <- reactive({
    req(filtered_owad_data())
    filtered_owad_data() %>%
      group_by(Age) %>%
      summarise(Quality.of.Sleep = mean(Quality.of.Sleep),
                Observations = n(), .groups = 'drop')
  })
  
  # Reactive expression to create bar plot data
  bar_plot_data <- reactive({
    filtered_owad_data() %>%
      group_by(Occupation) %>%
      summarise(Quality.of.Sleep = round(mean(Quality.of.Sleep), 2), .groups = 'drop') %>%
      mutate(max_quality_of_sleep = 10.00) %>%
      mutate(Occupation = as.factor(Occupation))
  })
  
  # Reactive expression to create scatter plot data
  owad_data_counts <- reactive({
    filtered_owad_data() %>%
      count(Physical.Activity.Level, Quality.of.Sleep, name = "Count")
  })
  
  scatter_data <- reactive({
    left_join(filtered_owad_data(), owad_data_counts(), by = c("Physical.Activity.Level", "Quality.of.Sleep"))
  })
  
  # Reactive expression to create heatmap data
  heatmap_data <- reactive({
    filtered_owad_data() %>%
      count(Occupation, Quality.of.Sleep) %>%
      complete(Occupation, Quality.of.Sleep, fill = list(n = 0))
  })
  
  # Render line plot
  output$linePlot <- renderPlot({
    line_plot_data() %>% 
      ggplot(aes(x = Age, y = Quality.of.Sleep, color = Quality.of.Sleep)) +
      geom_line() +
      scale_colour_gradient(low = "red", high = "forestgreen") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Render bar plot
  output$barPlot <- renderPlot({
    bar_plot_data() %>% 
      ggplot(aes(x = Occupation, y = Quality.of.Sleep, fill = Occupation)) +
      geom_col(width = 0.61) +
      geom_text(aes(label = sprintf("%.2f", Quality.of.Sleep)), hjust = -0.2, size = 3.5) +
      geom_col(width = 0.61, aes(y = max_quality_of_sleep), alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank()) +
      labs(y = "Quality of Sleep") +
      scale_x_discrete(limits = rev) +
      scale_y_continuous(expand = expansion(add = c(0, 0)))
  })
  
  # Render scatter plot
  output$scatterPlot <- renderPlot({
    plot <- scatter_data() %>% 
      ggplot(aes(x = Physical.Activity.Level, y = Quality.of.Sleep, color = Sleep.Disorder, alpha = Count)) +
      geom_point() +
      facet_wrap(~Sleep.Disorder, scales = "free", ncol = 1) +
      labs(x = "Physical Activity Level", y = "Sleep Quality") +
      scale_alpha_continuous(range = c(0.1, 1)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    if (input$show_trend) {
      plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1))
    }
    
    plot
  })
  
  # Render heatmap
  output$heatMap <- renderPlot({
    heatmap_data() %>%
      ggplot(aes(x = factor(Quality.of.Sleep), y = factor(Occupation, levels = sort(unique(Occupation), decreasing = TRUE)), fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(x = "Sleep Quality", y = "Occupation", fill = "Count") +
      theme(panel.grid = element_blank())
  })
  
  # Update age slider based on brushed points on the line plot
  observeEvent(input$linePlot_brush, {
    brushed <- brushedPoints(line_plot_data(), input$linePlot_brush)
    if (nrow(brushed) > 0) {
      min_age <- min(brushed$Age, na.rm = TRUE)
      max_age <- max(brushed$Age, na.rm = TRUE)
      updateSliderInput(session, "age_filter", value = c(min_age, max_age))
      
    }
  })
  
}