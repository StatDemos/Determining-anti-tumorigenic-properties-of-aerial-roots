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

Brine_shrimp_model_df2 <- lm(`% mortality`~ concentration + concentration2, data=df)
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


####################################################################################################

#####################################################################
# Best Model = Order 2 with filtering data
#####################################################################


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

