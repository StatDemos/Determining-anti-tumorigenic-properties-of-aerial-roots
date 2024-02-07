# Load libraries
library(tidyverse)

# Load data set
DPPH <- readxl::read_xlsx("Datasets/DPPH.xlsx")

# Visualization
DPPH %>%
  ggplot(aes(x=concentration, y=`scavenging activity`)) +
  geom_point() +
  geom_smooth()

# Regression model
# 1st order
DPPH_regression_model1 <- lm(DPPH$`scavenging activity` ~ DPPH$concentration)
summary(DPPH_regression_model1)


# 2nd order
DPPH$concentration2 <- DPPH$concentration*DPPH$concentration

DPPH_regression_model2 <- lm(DPPH$`scavenging activity` ~ DPPH$concentration + DPPH$concentration2)
summary(DPPH_regression_model2)
