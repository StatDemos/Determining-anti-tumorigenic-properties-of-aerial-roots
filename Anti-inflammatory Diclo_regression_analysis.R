# Load libraries
library(tidyverse)
library(broom)
library(rootSolve)

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


# Residual Analysis 
model_fitresid <- augment(Anti_inf_diclo_regression_model1)

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

f = function(x){
  7.1451 + 0.0455*x - 50
}

x = seq(0,1500,by=0.01)
plot(x, f(x), type = 'l')
abline(h=0, col="blue")

IC50 = uniroot.all(f, c(600, 1200))
IC50
# 941.8659

# graphing the IC50
# Plot the IC50 value in the graph

plot(Anti_inflammatory_diclo$concentration, Anti_inflammatory_diclo$inhibition,
     pch=16, 
     xlab = "concentration", ylab = "inhibition", cex.lab = 1.3, 
     col = "black")
lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)


