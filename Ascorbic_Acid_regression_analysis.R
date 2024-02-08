# Loading libraries
library(readxl)
library(tidyverse)
library(broom)

# Loading dataset
DPPH_Ascorbic_Acid <- read_excel("Datasets/DPPH Ascorbic Acid.xlsx")
View(DPPH_Ascorbic_Acid)


# Scatter plot
DPPH_Ascorbic_Acid %>% ggplot(aes(x = concentration, 
           y = `scavenging activity`)) + geom_point() + geom_smooth()


# Regression model

######################## ORDER 1 ###########################


AA_regression_model1 <-  lm(`scavenging activity` ~ concentration,
                            data = DPPH_Ascorbic_Acid)
summary(AA_regression_model1)


convalues <- seq(0, 100, 5)
predictedcounts <- predict(AA_regression_model1,
                           list(concentration=convalues))

plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
     pch=16, 
     xlab = "concentration", ylab = "scavenging activity", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)


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


######################## ORDER 3 ###########################


DPPH_Ascorbic_Acid$concentration2 <- DPPH_Ascorbic_Acid$concentration^2
DPPH_Ascorbic_Acid$concentration3 <- DPPH_Ascorbic_Acid$concentration^3
AA_regression_model3 <-  lm(`scavenging activity` ~ concentration + concentration2 +
                              concentration3,
                            data = DPPH_Ascorbic_Acid)
summary(AA_regression_model3)

convalues <- seq(0, 100, 5)
predictedcounts <- predict(AA_regression_model3,
                           list(concentration=convalues, concentration2=convalues^2,
                                concentration3=convalues^3))
plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
     pch=16, ylim = c(0,100),
     xlab = "concentration", ylab = "scavenging activity", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)

#####################################################################
# Best Model = Order 3
#####################################################################


###################### Residuals Analysis ###########################

model3_fitresid <- augment(AA_regression_model3)

# residual vs. fitted
ggplot(model3_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1)

# histogram of residuals
ggplot(model3_fitresid, aes(x = .std.resid)) + geom_histogram(color = "white") +
  ggtitle("Histogram of residuals")

# QQ plot
ggplot(model3_fitresid, aes(sample = .std.resid)) + stat_qq() + stat_qq_line() +
ggtitle("QQ plot of residuals")

# shapiro wilk
shapiro.test(model3_fitresid$.std.resid)
