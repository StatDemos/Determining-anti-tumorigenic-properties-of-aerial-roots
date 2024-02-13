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

mm.model.nls <- nls(`% mortality~Vm*concentration/(K+concentration), data=df, 
                    start = list(K=max(df$`% mortality`)/2, 
                                 Vm=max(df$`% mortality`)))
summary(mm.model.nls)