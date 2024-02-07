# load packages

library(readxl)
library(tidyverse)

# read data set

Flav <- read_excel("Datasets/Flavonoid Content.xlsx")
View(Flav)

# scatterplot

ggplot(Flav, aes(x= concentration, y= absorption_blank_diff)) + 
  geom_point() + geom_smooth()

# regression model

Flav_model = lm(absorption_blank_diff~concentration, data = Flav)
summary(Flav_model)

