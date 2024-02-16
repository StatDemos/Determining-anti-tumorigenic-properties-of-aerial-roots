# Loading libraries
library(readxl)
library(tidyverse)
library(broom)
library(rootSolve)
library(rootSolve)

# Loading data set
DPPH_Ascorbic_Acid <- read_excel("Datasets/DPPH Ascorbic Acid.xlsx")


# Scatter plot
DPPH_Ascorbic_Acid %>% ggplot(aes(x = concentration, 
                                  y = `scavenging activity`)) + geom_point() + 
  labs(title = "Scatter plot of concentration vs scavenging activity", 
       x = "concentration(µg/ml)") 

# fit non linear model - correct model
mm.model.nls <- nls(`scavenging activity`~Vm*concentration/(K+concentration), data=DPPH_Ascorbic_Acid, 
                    start = list(K=max(DPPH_Ascorbic_Acid$`scavenging activity`)/2, 
                                 Vm=max(DPPH_Ascorbic_Acid$`scavenging activity`)))
summary(mm.model.nls)

# calculate R-sq. value
sse <- mm.model.nls$m$deviance()
sse

sse <- mm.model.nls$m$deviance()
null <- lm(`scavenging activity`~1, DPPH_Ascorbic_Acid)
sst <- data.frame(summary.aov(null)[[1]])$Sum.Sq
percent_variation_explained = 100*(sst-sse)/sst
percent_variation_explained

# 97.02988
#########################################################################

plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`, xlab="concentration (µg/ml)", ylab = "scavenging activity",col = "blue",pch = 16 )
lines(DPPH_Ascorbic_Acid$concentration, 
      predict(mm.model.nls, DPPH_Ascorbic_Acid),col= "darkgreen",lwd = 3, main = "Fitted regression line")

# Calculation of IC50 value

x = seq(0,100,by=0.01)

f = function(x){
  (92.633*x)/(7.709+x)- 50
}

plot(x, f(x), type = 'l')
abline(h=0, col="blue")

IC50 = uniroot.all(f, c(0, 100))
IC50
# 9.041127

# graphing the roots
#points(x = IC50, y = rep(0, length(roots)), col = "red", pch = 16, cex = 1.5)

# Plot the IC50 value in the graph
plot(DPPH_Ascorbic_Acid$concentration, DPPH_Ascorbic_Acid$`scavenging activity`,
     pch=16,
     xlab = "concentration(µg/ml)", ylab = "scavenging activity", main = "Fitted regression line and IC50 value",
     cex.lab = 1.3, 
     col = "black")
lines(DPPH_Ascorbic_Acid$concentration, 
      predict(mm.model.nls),col= "darkgreen",lwd = 3)
abline(h = 50, col = 'blue')
points(x = IC50, y = 50, col = "red", pch = 16)


