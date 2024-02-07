# load packages

library(readxl)
library(tidyverse)

# read data set

Phenol <- read_excel("Datasets/Phenol Content.xlsx")
View(Phenol)

# scatterplot

ggplot(Phenol, aes(x= concentration, y= absorption_blank_diff)) + 
  geom_point() + geom_smooth()

# regression model

# linear model
Phenol_model1 = lm(absorption_blank_diff~concentration, data = Phenol)
summary(Phenol_model)

# quadratic model
Phenol$concentration2 <- Phenol$concentration*Phenol$concentration

phenol_model2 <- lm(Phenol$absorption_blank_diff~Phenol$concentration + Phenol$concentration2)
summary(phenol_model2)
