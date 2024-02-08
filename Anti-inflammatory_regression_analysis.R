# Load libraries
library(tidyverse)

# Load data set
Anti_inflammatory <- readxl::read_xlsx("Datasets/Anti-inflammatory.xlsx")

# Visualization
Anti_inflammatory %>%
  ggplot(aes(x=concentration, y=inhibition)) +
  geom_point() +
  geom_smooth()

# Regression model
# 1st order
Anti_inf_regression_model1 <- lm(Anti_inflammatory$inhibition ~ Anti_inflammatory$concentration)
summary(Anti_inf_regression_model1)
