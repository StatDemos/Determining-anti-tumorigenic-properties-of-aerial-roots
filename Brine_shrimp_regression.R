# load packages

library(readxl)
library(tidyverse)
library(stats)
library(dplyr)

# read data set

Brine_shrimp <- read_excel("Datasets/Brine shrimp.xlsx")
View(Brine_shrimp)
colnames(Brine_shrimp)

# filter data

df <- Brine_shrimp %>% filter(`% mortality`>25)
view(df)

# scatterplot

ggplot(Brine_shrimp, aes(x= concentration, y= `% mortality`)) + 
  geom_point() + geom_smooth()


ggplot(df, aes(x=concentration, y= `% mortality`)) + 
  geom_point() + geom_smooth()

# regression model

# linear model

Brine_shrimp_model1 = lm(`% mortality`~concentration, data = Brine_shrimp)
summary(Brine_shrimp_model1)

# for df
Brine_shrimp_model_df1 = lm(`% mortality`~concentration, data = df)
summary(Brine_shrimp_model_df1)


# quadratic model
Brine_shrimp$concentration2 <- Brine_shrimp$concentration*Brine_shrimp$concentration

Brine_shrimp_model2 <- lm(Brine_shrimp$`% mortality`~ Brine_shrimp$concentration + Brine_shrimp$concentration2)
summary(Brine_shrimp_model2)

# for df - this is the best model
df$concentration2 <-df$concentration*df$concentration

Brine_shrimp_model_df2 <- lm(df$`% mortality`~ df$concentration + df$concentration2)
summary(Brine_shrimp_model_df2)



# third order
Brine_shrimp$concentration3 <- Brine_shrimp$concentration*Brine_shrimp$concentration*Brine_shrimp$concentration

Brine_shrimp_model3 <- lm(Brine_shrimp$`% mortality`~ Brine_shrimp$concentration + Brine_shrimp$concentration2 + Brine_shrimp$concentration3)
summary(Brine_shrimp_model3)

# for df
df$concentration2 <- df$concentration*df$concentration

df$concentration3 <- df$concentration*df$concentration*df$concentration

Brine_shrimp_model_df3 <- lm(df$`% mortality`~ df$concentration + df$concentration2 + df$concentration3)
summary(Brine_shrimp_model_df3)
