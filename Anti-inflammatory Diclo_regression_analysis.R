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
Anti_inf_diclo_regression_model1 <- lm(inhibition ~ concentration, Anti_inflammatory_diclo)
summary(Anti_inf_diclo_regression_model1)

# Fitted regression line
convalues <- seq(0, 1500, 50)
predictedcounts <- predict(Anti_inf_diclo_regression_model1, list(concentration=convalues))

plot(Anti_inflammatory_diclo$concentration, Anti_inflammatory_diclo$inhibition,
     pch=16, 
     xlab = "concentration", ylab = "inhibition", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
