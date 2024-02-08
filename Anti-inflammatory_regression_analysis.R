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
Anti_inf_regression_model1 <- lm(inhibition ~ concentration, data = Anti_inflammatory)
summary(Anti_inf_regression_model1)

# Fitted regression line
convalues <- seq(0, 4500, 100)
predictedcounts <- predict(Anti_inf_regression_model1, list(concentration=convalues))

plot(Anti_inflammatory$concentration, Anti_inflammatory$inhibition,
     pch=16, 
     xlab = "concentration", ylab = "inhibition", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)

