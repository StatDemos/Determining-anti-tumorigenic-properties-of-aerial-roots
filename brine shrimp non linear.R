library(readxl)
library(tidyverse)
library(stats)
library(dplyr)
library(rootSolve)
library(broom)

# read data set

Brine_shrimp <- read_excel("Datasets/Brine shrimp.xlsx")
View(Brine_shrimp)

# filter standard data

df <- Brine_shrimp %>% filter(plant == "standard") 
View(df)

# scatter plot

ggplot(df, aes(x= concentration, y= `% mortality`)) + 
  geom_point() 

# filter standard data

df <- Brine_shrimp %>% filter(plant == "standard") 
View(df)

# scatter plot

ggplot(df, aes(x= concentration, y= `% mortality`)) + 
  geom_point() 

library(drc)
library(ggplot2)
MM.model <- drm(`% mortality`~ concentration, data=df, fct=MM.2())
mmdf <- data.frame(concentration=seq(0,max(df$concentration),length.out=100))
mmdf$`% mortality` <- predict(MM.model, newdata=mmdf)
ggplot(df, aes(x = concentration  , y = `% mortality`)) +
  theme_bw() +
  xlab("Concentration [mM]") +
  ylab("Speed [dE/min]") +
  ggtitle("Techvidvan Michaelis-Menten kinetics") +
  geom_point(alpha = 0.5) +
  geom_line(data = mmdf, 
            aes(x = concentration, y = `% mortality`), 
            colour = "green")
summary(MM.model)

###################################################################


mm.model.nls <- nls(`% mortality`~Vm*concentration/(K+concentration), data=df, 
                    start = list(K=max(df$`% mortality`)/2, 
                                 Vm=max(df$`% mortality`)))
summary(mm.model.nls)

sse <- mm.model.nls$m$deviance()
sse

sse <- mm.model.nls$m$deviance()
null <- lm(`% mortality`~1, df)
sst <- data.frame(summary.aov(null)[[1]])$Sum.Sq
percent_variation_explained = 100*(sst-sse)/sst
percent_variation_explained

#########################################################################

plot(df$concentration, df$`% mortality`, xlab="concentration (µg/ml)", ylab = "mortality rate")
lines(df$concentration, 
      predict(mm.model.nls, df),col=2)

# Calculation of IC50 value

x = seq(0,1000,by=0.01)

f = function(x){
  (101.2*x)/(234.8+x)- 50
}

plot(x, f(x), type = 'l')
abline(h=0, col="blue")

IC50 = uniroot.all(f, c(0, 1000))
IC50
# 229.2696

# graphing the roots
points(x = IC50, y = rep(0, length(roots)), col = "red", pch = 16, cex = 1.5)

# Plot the IC50 value in the graph
plot(df$concentration, df$`% mortality`,
     pch=16, ylim = c(0,100),
     xlab = "concentration(µg/ml)", ylab = "mortality rate", main = "Fitted regression line and IC50 value",
     cex.lab = 1.3, 
     col = "black")
lines(df$concentration, 
      predict(mm.model.nls, df),col= "darkgreen",lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)

