# load packages

library(readxl)
library(tidyverse)
library(broom)

# read data set

Phenol <- read_excel("Datasets/Phenol Content.xlsx")
View(Phenol)

# scatterplot

ggplot(Phenol, aes(x= concentration, y= absorption_blank_diff)) + 
  geom_point() + geom_smooth()

# regression model

# linear model - the best model 
Phenol_model1 = lm(absorption_blank_diff~concentration, data = Phenol)
summary(Phenol_model1)

# quadratic model
Phenol$concentration2 <- Phenol$concentration*Phenol$concentration

phenol_model2 <- lm(absorption_blank_diff~concentration + concentration2 , data=Phenol)
summary(phenol_model2)

##################################################################################

convalues <- seq(0,65,0.5)
predictedcounts <- predict(Phenol_model1,
                           list(concentration=convalues))

plot(Phenol$concentration, Phenol$absorption_blank_diff,
     pch=16, 
     xlab = "concentration", ylab = "absorption", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)

#####################################################################
# Best Model = Order 1
#####################################################################


###################### Residuals Analysis ###########################

model1_fitresid <- augment(phenol_model1)

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
shapiro.test(model1_fitresid$.resid)


