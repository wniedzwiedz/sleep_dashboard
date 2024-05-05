library(tidyverse)

owad_data <- read.csv("./Sleep_health_and_lifestyle_dataset.csv") %>% arrange(Occupation)

#line_plot_data <- owad_data %>% 
#  group_by(Age) %>% 
#  summarise(Quality.of.Sleep = mean(Quality.of.Sleep))


