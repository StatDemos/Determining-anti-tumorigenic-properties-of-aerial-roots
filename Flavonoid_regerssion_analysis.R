# load packages

library(readxl)
library(tidyverse)
library(broom)

# read data set

Flav <- read_excel("Datasets/Flavonoid Content.xlsx")
View(Flav)

# scatterplot

ggplot(Flav, aes(x= concentration, y= absorption_blank_diff)) + 
  geom_point() + geom_smooth()

# regression model

Flav_model = lm(absorption_blank_diff~concentration, data = Flav)
summary(Flav_model)


predictedcounts <- predict(Flav_model,
                           list(concentration=Flav$concentration))
                                
plot(Flav$concentration, Flav$absorption_blank_diff,
     pch=16, 
     xlab = "concentration", ylab = "absorption", cex.lab = 1.3, 
     col = "blue")

lines(Flav$concentration, predictedcounts, col = "darkgreen", lwd = 3)

#####################################################################
# Best Model = Order 1
#####################################################################


###################### Residuals Analysis ###########################

model1_fitresid <- augment(Flav_model)

# residual vs. fitted
ggplot(model1_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1)

# histogram of residuals
ggplot(model1_fitresid, aes(x = .std.resid)) + geom_histogram(color = "white") +
  ggtitle("Histogram of residuals")

# QQ plot
ggplot(model1_fitresid, aes(sample = .std.resid)) + stat_qq() + stat_qq_line() +
  ggtitle("QQ plot of residuals")

# shapiro wilk
shapiro.test(model1_fitresid$.std.resid)
