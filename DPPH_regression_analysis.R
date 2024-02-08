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
DPPH_regression_model1 <- lm(`scavenging activity` ~ concentration, data = DPPH)
summary(DPPH_regression_model1)


# 2nd order
DPPH$concentration2 <- DPPH$concentration*DPPH$concentration

DPPH_regression_model2 <- lm(`scavenging activity` ~ concentration + concentration2, data = DPPH)
summary(DPPH_regression_model2)

# Fitted regression line
convalues <- seq(0, 400, 10)
convalues2 <- convalues^2
predictedcounts <- predict(DPPH_regression_model2, list(concentration=convalues, concentration2 = convalues2))

plot(DPPH$concentration, DPPH$`scavenging activity`,
     pch=16, 
     xlab = "concentration", ylab = "scavenging activity", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
