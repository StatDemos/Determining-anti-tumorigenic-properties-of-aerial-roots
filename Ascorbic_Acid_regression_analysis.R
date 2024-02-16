# Loading libraries
library(readxl)
library(tidyverse)
library(broom)
library(rootSolve)


# Loading dataset
DPPH_Ascorbic_Acid <- read_excel("Datasets/DPPH Ascorbic Acid.xlsx")


# Scatter plot
DPPH_Ascorbic_Acid %>% ggplot(aes(x = concentration, 
           y = `scavenging activity`)) + geom_point() + 
  labs(title = "Scatter plot of concentration vs scavenging activity", 
       x = "concentration(µg/ml)")
           # + geom_smooth()


# Regression model

######################## ORDER 1 ###########################

# 
# AA_regression_model1 <-  lm(`scavenging activity` ~ concentration,
#                             data = DPPH_Ascorbic_Acid)
# summary(AA_regression_model1)

# 
# convalues <- seq(0, 100, 5)
# predictedcounts <- predict(AA_regression_model1,
#                            list(concentration=convalues))
# 
# plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
#      pch=16,
#      xlab = "concentration", ylab = "scavenging activity", cex.lab = 1.3,
#      col = "blue")
# 
# lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)


######################## ORDER 2 ###########################


DPPH_Ascorbic_Acid$concentration2 <- DPPH_Ascorbic_Acid$concentration^2
AA_regression_model2 <-  lm(`scavenging activity` ~ concentration + concentration2,
                            data = DPPH_Ascorbic_Acid)
summary(AA_regression_model2)

convalues <- seq(0, 100, 5)
predictedcounts <- predict(AA_regression_model2,
                           list(concentration=convalues, concentration2=convalues^2))
plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
     pch=16, ylim = c(0,100),
     xlab = "concentration", ylab = "scavenging activity", cex.lab = 1.3,
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)


######################## ORDER 3 - BEST MODEL ###########################


# DPPH_Ascorbic_Acid$concentration2 <- DPPH_Ascorbic_Acid$concentration^2
# DPPH_Ascorbic_Acid$concentration3 <- DPPH_Ascorbic_Acid$concentration^3
# AA_regression_model3 <-  lm(`scavenging activity` ~ concentration + concentration2 +
#                               concentration3,
#                             data = DPPH_Ascorbic_Acid)
# summary(AA_regression_model3)
# 
# convalues <- seq(0, 100, 5)
# predictedcounts <- predict(AA_regression_model3,
#                            list(concentration=convalues, concentration2=convalues^2,
#                                 concentration3=convalues^3))
# plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
#      pch=16, ylim = c(0,100), main = "Fitted regression line",
#      xlab = "concentration(µg/ml)", ylab = "scavenging activity", cex.lab = 1.3, 
#      col = "blue") 
# 
# 
# lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)

#####################################################################
# Best Model = Order 3
#####################################################################


###################### Residuals Analysis ###########################

model3_fitresid <- augment(AA_regression_model3)

# residual vs. fitted
ggplot(model3_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1) + 
  labs(title = "Residual plot against the fitted values", x = "Fitted values", 
       y = "Residuals")

# histogram of residuals
ggplot(model3_fitresid, aes(x = .std.resid)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(alpha = 0.5, fill = "purple") + ggtitle("Histogram of residuals") +
  xlab("Residuals") + ylab("Density")

# QQ plot
ggplot(model3_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# shapiro wilk
shapiro.test(model3_fitresid$.std.resid)

###################################################################################
# scavenging activity = 4.301 + 5.436concentration - 0.1094concentration^2 + 0.0006273concentration^3
###################################################################################

###################### Calculating IC50 ########################### 

# 0 = 4.301 - 50 + 5.436concentration - 0.1094concentration^2 + 0.0006273concentration^3
# 0 = - 45.699 + 5.436concentration - 0.1094concentration^2 + 0.0006273concentration^3


# writing the function
f2 = function(concentration) {
  5.436*concentration - 0.1094*concentration^2 + 0.0006273*concentration^3 - 45.699
}

# finding range of the roots

concentration = 0:100
plot(concentration, f2(concentration), type = 'l')
# adding a horizontal line at y = 0
abline(h = 0, col = 'blue')

# finding IC50 value
IC50 <- uniroot.all(f2, c(0, 100))
IC50

# Plot the IC50 value in the graph
plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
     pch=16, ylim = c(0,100), main = "Fitted regression line and IC50 value",
     xlab = "concentration(µg/ml)", ylab = "scavenging activity", cex.lab = 1.3, 
     col = "black")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)



#####################################################################

# Calculating IC50 for second order model


# 0 = 16.285 - 50 + 2.52 concentration - 0.019 concentration^2 
# 0 = -33.715 + 2.52 concentration - 0.019 concentration^2 


# writing the function
f2 = function(concentration) {
  2.52*concentration - 0.019*concentration^2 - 33.715
}

# finding range of the roots

concentration = 0:100
plot(concentration, f2(concentration), type = 'l')
# adding a horizontal line at y = 0
abline(h = 0, col = 'blue')

# finding IC50 value
IC50 <- uniroot.all(f2, c(0, 100))
IC50

# Plot the IC50 value in the graph
plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
     pch=16, ylim = c(0,100), main = "Fitted regression line and IC50 value",
     xlab = "concentration(µg/ml)", ylab = "scavenging activity", cex.lab = 1.3, 
     col = "black")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)








