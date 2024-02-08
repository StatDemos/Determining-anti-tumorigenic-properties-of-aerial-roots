# Load libraries
library(tidyverse)
library(broom)
library(rootSolve)

# Load data set
DPPH <- readxl::read_xlsx("Datasets/DPPH.xlsx")

# Visualization
DPPH %>%
  ggplot(aes(x=concentration, y=`scavenging activity`)) +
  geom_point() +
  geom_smooth()

DPPH %>%
  ggplot(aes(x=concentration, y=`scavenging activity`)) +
  geom_point() +
  labs(title = "Scatter plot of concentration vs scavenging activity") +
  xlab("concentration (µg/ml)")

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
     xlab = "concentration (µg/ml)", ylab = "scavenging activity",
     main = "Fitted regression line",
     cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h=50, col="red")


# Residual Analysis 
model_fitresid <- augment(DPPH_regression_model2)

# residual vs. fitted
ggplot(model_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1) + 
  labs(title = "Residual plot against the fitted values", x = "Fitted values", 
       y = "Residuals")

# histogram of residuals
ggplot(model_fitresid, aes(x = .std.resid)) + geom_histogram(aes(y = ..density..), color = "black",
                                                             fill = "white") +
  geom_density(alpha = 0.5, fill = "purple") + ggtitle("Histogram of residuals") +
  xlab("Residuals") + ylab("Density")

# QQ plot
ggplot(model_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# shapiro wilk
shapiro.test(model_fitresid$.std.resid)


# Calculation of IC50 value

x = seq(0,400,by=0.01)

f = function(x){
  -0.1596 + 0.5471*x - 0.001*x^2 - 50
}

plot(x, f(x), type = 'l')
abline(h=0, col="blue")

IC50 = uniroot.all(f, c(0, 200))
IC50
# 116.4831

# graphing the roots
points(x = IC50, y = rep(0, length(IC50)), col = "red", pch = 16, cex = 1.5)


# Plot the IC50 value in the graph
plot(DPPH$concentration, DPPH$`scavenging activity`,
     pch=16, ylim = c(0,100),
     xlab = "concentration (µg/ml)", ylab = "scavenging activity", 
     main = "Fitted regression line and IC50 value",
     cex.lab = 1.3, 
     col = "black")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)









