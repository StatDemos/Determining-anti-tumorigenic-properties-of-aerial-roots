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

convalues <- seq(0,800,25)
predictedcounts <- predict(Flav_model,
                           list(concentration=convalues))
                                
plot(Flav$concentration, Flav$absorption_blank_diff,
     pch=16, 
     xlab = "concentration", ylab = "absorption", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)

#####################################################################
# Best Model = Order 1
#####################################################################


###################### Residuals Analysis ###########################

model1_fitresid <- augment(Flav_model)

# residual vs. fitted
ggplot(model1_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1) + 
  labs(title = "Residual plot against the fitted values", x = "Fitted values", 
       y = "Residuals")

# histogram of residuals
ggplot(model1_fitresid, aes(x = .std.resid)) + geom_histogram(aes(y = ..density..), color = "black",
                                                                fill = "white") +
  geom_density(alpha = 0.5, fill = "purple") + ggtitle("Histogram of residuals") +
  xlab("Residuals") + ylab("Density")

# QQ plot
ggplot(model1_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# shapiro wilk
shapiro.test(model1_fitresid$.std.resid)


