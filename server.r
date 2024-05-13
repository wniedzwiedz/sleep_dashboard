library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  filtered_owad_data <- reactive({
    filtered_data <- owad_data %>%
      filter(!Occupation %in% input$occupation_filter,
             !Sleep.Disorder %in% input$disorder_filter,
             Age >= input$age_filter[1] & Age <= input$age_filter[2],
             Quality.of.Sleep >= input$quality_filter[1] & Quality.of.Sleep <= input$quality_filter[2])
    
    return(filtered_data)
  })
  
  line_plot_data <- reactive({
    req(filtered_owad_data())
    
    filtered_data <- filtered_owad_data()
    
    filtered_data %>%
      group_by(Age) %>%
      summarise(Quality.of.Sleep = mean(Quality.of.Sleep),
                Observations = n())
  })
  
  bar_plot_data <- reactive({
    filtered_owad_data() %>%
      group_by(Occupation) %>%
      summarise(Quality.of.Sleep = round(mean(Quality.of.Sleep), 2)) %>%
      mutate(max_quality_of_sleep = 10.00) %>%
      mutate_at("Occupation", as.factor)
  })
  
  owad_data_counts <- reactive({
    filtered_owad_data() %>%
    count(Physical.Activity.Level, Quality.of.Sleep, name = "Count")
  })
  
  scatter_data <- reactive({
    left_join(filtered_owad_data(), owad_data_counts(), by = c("Physical.Activity.Level", "Quality.of.Sleep"))
  })
  
  heatmap_data <- reactive({
    filtered_owad_data() %>%
      count(Occupation, Quality.of.Sleep) %>%
      complete(Occupation, Quality.of.Sleep, fill = list(n = 0))
  })
  
  
  
  
  output$linePlot <- renderPlot({
    line_plot_data() %>% 
      ggplot(aes(x = Age, y = Quality.of.Sleep, color = Quality.of.Sleep)) +
      geom_line() +
      scale_colour_gradient(low = "red", high = "forestgreen") +
      theme_minimal()+
      theme(legend.position = "none")
  })
  
  
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
      scale_y_discrete(expand = expansion(add=c(0,0)))
  })
  
  output$scatterPlot <- renderPlot({
    plot <- scatter_data() %>% ggplot(aes(x = Physical.Activity.Level, y = Quality.of.Sleep, color = Sleep.Disorder, alpha = Count)) +
      geom_point() +
      facet_wrap(~Sleep.Disorder, scales = "free", ncol = 1) +
      labs(x = "Physical Activity Level", y = "Sleep Quality") +
      scale_alpha_continuous(range = c(0.1, 1)) +  # Adjust the range of opacity
      theme_minimal()+
      theme(legend.position = "none")
  
  
  if (input$show_trend) {
    plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1))   # Add a linear trend line
  }
  
  plot
  })
  
  output$heatMap <- renderPlot({
    heatmap_data() %>%
      ggplot(aes(x = factor(Quality.of.Sleep), y = factor(Occupation, levels = sort(unique(Occupation), decreasing = TRUE)), fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(x = "Sleep Quality", y = "Occupation", fill = "Count") +
      theme(panel.grid = element_blank()) 
  })
}
