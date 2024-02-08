# load packages

library(readxl)
library(tidyverse)
library(stats)
library(dplyr)
library(rootSolve)
library(broom)

# read data set

Brine_shrimp <- read_excel("Datasets/Brine shrimp.xlsx")
View(Brine_shrimp)


# scatter plot

ggplot(Brine_shrimp, aes(x= concentration, y= `% mortality`)) + 
  geom_point() + geom_smooth()


# regression model

# linear model

Brine_shrimp_model1 = lm(`% mortality`~concentration, data = Brine_shrimp)
summary(Brine_shrimp_model1)


# quadratic model
Brine_shrimp$concentration2 <- Brine_shrimp$concentration*Brine_shrimp$concentration

Brine_shrimp_model2 <- lm(`% mortality`~ concentration + concentration2, data = Brine_shrimp)
summary(Brine_shrimp_model2)

# third order
Brine_shrimp$concentration3 <- Brine_shrimp$concentration*Brine_shrimp$concentration*Brine_shrimp$concentration

Brine_shrimp_model3 <- lm(`% mortality`~ concentration + concentration2 + concentration3, data = Brine_shrimp)
summary(Brine_shrimp_model3)


####################################################################################################

# Fitted regression line 2nd order
convalues <- seq(0, 1000, 10)
predictedcounts <- predict(Brine_shrimp_model2, list(concentration=convalues, concentration2=convalues^2))

plot(Brine_shrimp$concentration, Brine_shrimp$`% mortality`,
     pch=16, 
     xlab = "concentration", ylab = "mortality rate", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h=50, col="red")


# Fitted regression line 3rd order
convalues <- seq(0, 1000, 10)
predictedcounts <- predict(Brine_shrimp_model3, list(concentration=convalues, concentration2=convalues^2, concentration3=convalues^3 ))

plot(Brine_shrimp$concentration, Brine_shrimp$`% mortality`,
     pch=16, 
     xlab = "concentration", ylab = "mortality rate", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h=50, col="red")


#####################################################################

# filter data below 60 mortality rate

df <- Brine_shrimp %>% filter(`% mortality` < 60) 
View(df)

# scatter plot

ggplot(df, aes(x= concentration, y= `% mortality`)) + 
  geom_point() + geom_smooth()

# fitted regression 
# linear model

Brine_shrimp_model_df1 = lm(`% mortality`~concentration, data = df)
summary(Brine_shrimp_model_df1)

# quadratic model - best model
df$concentration2 <- df$concentration*df$concentration

Brine_shrimp_model_df2 <- lm(`% mortality`~ concentration + concentration2, data = df)
summary(Brine_shrimp_model_df2)

# third order
df$concentration3 <- df$concentration*df$concentration*df$concentration

Brine_shrimp_model_df3 <- lm(`% mortality`~ concentration + concentration2 + concentration3, data = df)
summary(Brine_shrimp_model_df3)


# fitted regression line using df
# for 2nd order

convalues <- seq(0,1000,10)
predictedcounts <- predict(Brine_shrimp_model_df2, 
                           list(concentration=convalues, concentration2=convalues^2))

plot(df$concentration, df$`% mortality`,
     pch=16, 
     xlab = "concentration", ylab = "absorption", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)


# for 3rd order

# Fitted regression line 3rd order
convalues <- seq(0, 1000, 10)
predictedcounts <- predict(Brine_shrimp_model_df3, list(concentration=convalues, concentration2=convalues^2, 
                                                        concentration3=convalues^3))

plot(df$concentration, df$`% mortality`,
     pch=16, 
     xlab = "concentration", ylab = "mortality rate", cex.lab = 1.3, 
     col = "blue")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h=50, col="red")


###########################################################

# Calculation of IC50 value

x = seq(0,1000,by=0.01)

f = function(x){
 13.27 + 0.06581*x - 0.00004522*x^2 - 50
}

plot(x, f(x), type = 'l')
abline(h=0, col="blue")

roots = uniroot.all(f, c(0, 200))
roots
# 116.4831

# graphing the roots
points(x = roots, y = rep(0, length(roots)), col = "red", pch = 16, cex = 1.5)


###################### Residuals Analysis ###########################

model2_fitresid <- augment(Brine_shrimp_model_df2)

# residual vs. fitted
ggplot(model2_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1) + 
  labs(title = "Residual plot against the fitted values", x = "Fitted values", 
       y = "Residuals")

# histogram of residuals
ggplot(model2_fitresid, aes(x = .std.resid)) + geom_histogram(aes(y = ..density..), color = "black",
                                                              fill = "white") +
  geom_density(alpha = 0.5, fill = "purple") + ggtitle("Histogram of residuals") +
  xlab("Residuals") + ylab("Density")

# QQ plot
ggplot(model2_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# shapiro wilk
shapiro.test(model2_fitresid$.std.resid)
