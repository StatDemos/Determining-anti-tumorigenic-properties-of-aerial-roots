# Load libraries
library(tidyverse)

# Load data set
Anti_inflammatory_diclo <- readxl::read_xlsx("Datasets/Anti-inflammatory Diclo.xlsx")

# Visualization
Anti_inflammatory_diclo %>%
  ggplot(aes(x=concentration, y=inhibition)) +
  geom_point() +
  geom_smooth()

# Regression model
# 1st order
Anti_inf_diclo_regression_model1 <- lm(Anti_inflammatory_diclo$inhibition ~ Anti_inflammatory_diclo$concentration)
summary(Anti_inf_diclo_regression_model1)
