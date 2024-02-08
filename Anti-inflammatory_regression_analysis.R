# Load libraries
library(tidyverse)
library(broom)
library(rootSolve)

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
abline(h=50, col="red")


# Residual Analysis 
model_fitresid <- augment(Anti_inf_regression_model1)

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
x = seq(0,5000,by=0.01)

f = function(x){
  -7.7062 + 0.0162*x - 50
}

plot(x, f(x), type = 'l')
abline(h=0, col="blue")

IC50 = uniroot.all(f, c(3000, 4000))
IC50
# 3562.111

# graphing the roots
points(x = roots, y = rep(0, length(roots)), col = "red", pch = 16, cex = 1.5)

# Plot the IC50 value in the graph
plot(Anti_inflammatory$concentration, Anti_inflammatory$inhibition,
     pch=16, ylim = c(0,100),
     xlab = "concentration", ylab = "scavenging activity", cex.lab = 1.3, 
     col = "black")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)




