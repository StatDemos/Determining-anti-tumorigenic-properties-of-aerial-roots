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


#####################################################################

# filter standard data

df <- Brine_shrimp %>% filter(plant == "standard") 
View(df)

# scatter plot

ggplot(df, aes(x= concentration, y= `% mortality`)) + 
  geom_point() + geom_smooth()

# fitted regression 
# linear model

Brine_shrimp_model_df1 = lm(`% mortality`~concentration, data = df)
summary(Brine_shrimp_model_df1)

# quadratic model 
df$concentration2 <- df$concentration*df$concentration

Brine_shrimp_model_df2 <- lm(`% mortality`~ concentration + concentration2, data = df)
summary(Brine_shrimp_model_df2)

# third order - best model
df$concentration3 <- df$concentration*df$concentration*df$concentration

Brine_shrimp_model_df3 <- lm(`% mortality`~ concentration + concentration2 + concentration3, data = df)
summary(Brine_shrimp_model_df3)

# fitted regression line using df

# Fitted regression line 3rd order
convalues <- seq(0, 1000, 10)
predictedcounts <- predict(Brine_shrimp_model_df3, list(concentration=convalues, concentration2=convalues^2, 
                                                        concentration3=convalues^3))

plot(df$concentration, df$`% mortality`,
     pch=16, 
     xlab = "concentration(µg/ml)", ylab = "mortality rate", cex.lab = 1.3, 
     col = "blue" , main = "Fitted regression line")


lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)



###########################################################

# Calculation of IC50 value

x = seq(0,1000,by=0.01)

f = function(x){
 -0.7188 + 0.4209*x - 0.0008792*x^2 + 0.000000558*x^3 - 50
}

plot(x, f(x), type = 'l')
abline(h=0, col="blue")

IC50 = uniroot.all(f, c(0, 1000))
IC50
# 181.1943

# graphing the roots
points(x = IC50, y = rep(0, length(roots)), col = "red", pch = 16, cex = 1.5)

# Plot the IC50 value in the graph
plot(df$concentration, df$`% mortality`,
     pch=16, ylim = c(0,100),
     xlab = "concentration(µg/ml)", ylab = "mortality rate", main = "Fitted regression line and IC50 value",
     cex.lab = 1.3, 
     col = "black")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)

#########################################################################################
# for plants

# filter plant data

df2 <- Brine_shrimp %>% filter(plant != "standard") 
View(df2)

# scatter plot

ggplot(df2, aes(x= concentration, y= `% mortality`)) + 
  geom_point() + geom_smooth()

# fitted regression 
# linear model

Brine_shrimp_model_df12 = lm(`% mortality`~concentration, data = df2)
summary(Brine_shrimp_model_df12)

# quadratic model 
df2$concentration2 <- df2$concentration*df2$concentration

Brine_shrimp_model_df22 <- lm(`% mortality`~ concentration + concentration22, data = df2)
summary(Brine_shrimp_model_df22)

# third order - best model
df2$concentration3 <- df2$concentration*df2$concentration*df2$concentration

Brine_shrimp_model_df32 <- lm(`% mortality`~ concentration + concentration2 + concentration3, data = df2)
summary(Brine_shrimp_model_df32)

# fitted regression line using df



# Fitted regression line 3rd order
convalues <- seq(0, 1000, 10)
predictedcounts <- predict(Brine_shrimp_model_df32, list(concentration=convalues, concentration2=convalues^2, 
                                                         concentration3=convalues^3))


plot(df2$concentration, df2$`% mortality`,
     pch=16, 
     xlab = "concentration(µg/ml)", ylab = "mortality rate", cex.lab = 1.3, 
     col = "blue", main = "Fitted regression line")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)


###########################################################

# Calculation of IC50 value

x = seq(0,1500)

f = function(x){
  11.58 + 0.06967*x - 0.00009572*x^2 + 0.00000004956*x^3 - 50
}

plot(x, f(x), type = 'l')
abline(h=0, col="blue")

IC50 = uniroot.all(f, c(0, 1500))
IC50
# 1310.027

# graphing the roots
points(x = IC50, y = rep(0, length(roots)), col = "red", pch = 16, cex = 1.5)

# Plot the IC50 value in the graph
plot(df2$concentration, df2$`% mortality`,
     pch=16, ylim = c(0,100),
     xlab = "concentration(µg/ml)", ylab = "mortality rate", main = "Fitted regression line and IC50 value",
     cex.lab = 1.3, 
     col = "black")

lines(convalues, predictedcounts, col = "darkgreen", lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)
