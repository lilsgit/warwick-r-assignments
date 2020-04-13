######################################################################################################################
##                                                       PART 1                                                     ##
######################################################################################################################

install.packages("carData")
library("carData")

f1 <- lm(income ~ 1, Prestige) # a: only has one intercept 
f2 <- lm(income ~ education, Prestige) # b: has one independent variable - education 
f3 <- lm(income ~ prestige, Prestige) # c: has one independent variable - prestige
f4 <- lm(income ~ 0 + prestige, Prestige) # d: has one independent variable - prestige, with no intercept
f5 <- lm(income ~ prestige - 1, Prestige) # e: has one independent variable - prestige, with no intercept, same as f4
f6 <- lm(income ~ education + prestige, Prestige) # f: has two independent variables - education, prestige
f7 <- lm(income ~ education:prestige, Prestige) # g: has one independent variable - interaction between education and prestige
f8 <- lm(income ~ 0 + education:prestige, Prestige) # h: has one independent variable - interaction between education and prestige, with no intercept
f9 <- lm(income ~ education*prestige, Prestige) # i: has three independent variables - education, prestige and interaction between education and prestige
f10 <- lm(income ~ 0 + education*prestige, Prestige) # j: has three independent variables - education, prestige and interaction between education and prestige, with no intercept

######################################################################################################################
##                                                       PART 2                                                     ##
######################################################################################################################

install.packages("tidyverse")
install.packages("cowplot")
devtools::install_github('thomasp85/gganimate')
library("tidyverse")
library("cowplot")  
library("gganimate") 

d <- read.csv("/Users/tianyi/Desktop/COURSES/Term 1/Methods and Analysis in Behavioural Science/Homework 3/distress.csv")

#1. Visualize the interrelation of the three variables, distress, self.blame, and circumstance.blame.
######################################################################################################################

head(d)
#self.blame circumstance.blame distress
#1          1                  3       19
#2          3                  3       25
#3          1                  2       14
#4          3                  3       30
#5          2                  3       20
#6          1                  4       26

#Three methods of presenting the interrelation of the three variables: 
#Method 1
pairs(cor(d))
#Method 2
install.packages("corrplot")
library("corrplot")
corrplot(cor(d))
#Method 3
install.packages("mvtnorm")
install.packages("GGally")
install.packages("emmeans")
library("mvtnorm")
library("GGally")
library("emmeans")
GGally::ggscatmat(d, columns = c(1, 2, 3))
ggsave("interrelation.pdf")

#2. Construct three regression models for predicting distress from self blame, distress from circumstance blame, and distress from both self blame and circumstance blame.
######################################################################################################################

m1 <- lm(distress ~ self.blame, d)
summary(m1) 
#Call:
#  lm(formula = distress ~ self.blame, data = d)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8.7104 -2.7104  0.2653  3.3383 11.3870 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  20.5156     0.8963  22.889  < 2e-16 ***
#  self.blame    2.0487     0.3566   5.745 1.08e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.329 on 96 degrees of freedom
#Multiple R-squared:  0.2558,	Adjusted R-squared:  0.2481 
#F-statistic:    33 on 1 and 96 DF,  p-value: 1.082e-07

m2 <- lm(distress ~ circumstance.blame, d)
summary(m2) 
#Call:
#  lm(formula = distress ~ circumstance.blame, data = d)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11.3674  -3.7092   0.1178   2.9965  11.6326 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         26.3969     1.1886  22.209   <2e-16 ***
#  circumstance.blame  -0.5148     0.3998  -1.287    0.201    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.975 on 96 degrees of freedom
#Multiple R-squared:  0.01697,	Adjusted R-squared:  0.006733 
#F-statistic: 1.658 on 1 and 96 DF,  p-value: 0.201

m3 <- lm(distress ~ self.blame + circumstance.blame, d)
summary(m3)
#Call:
#  lm(formula = distress ~ self.blame + circumstance.blame, data = d)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9.7332 -2.5422  0.1266  2.2611  8.2053 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         23.6659     0.9498  24.917  < 2e-16 ***
#  self.blame           3.0615     0.3554   8.614 1.51e-13 ***
#  circumstance.blame  -1.9943     0.3467  -5.752 1.07e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.747 on 95 degrees of freedom
#Multiple R-squared:  0.448,	Adjusted R-squared:  0.4364 
#F-statistic: 38.56 on 2 and 95 DF,  p-value: 5.503e-13

#3. Why are the coefficients in the single-predictor models smaller in magnitude than the coefficients in the model with both predictors?
######################################################################################################################

#Because the model with both predictors explains more of the dependent variable than the single-predictor models. From the Multiple R-squared we can also confrim that (the Multiple R-squared of the model with both predictors is 0.448, which is much more lager than the ones of the single-predictor models). 

#To make sure the model does not have multicollinearity problem, we need to controlling:
m3_1 <- lm(distress ~ self.blame + circumstance.blame, d)
summary(m3_1)$coefficients
m3_2 <- lm(self.blame ~ circumstance.blame, d)
d$resid_a <- residuals(m3_2)
summary(lm(distress ~ 0 + resid_a, d))$coefficients
m3_3 <-lm(distress ~ self.blame + circumstance.blame, d)
summary(m3_3)$coefficients

#4. Construct one regression model for predicting distress from, self blame, circumstance blame, and their interaction. What does this interaction tell you? Can you visualize it?
######################################################################################################################

m4 <- lm(distress ~ self.blame * circumstance.blame, d)
summary(m4) 
#Call:
#  lm(formula = distress ~ self.blame * circumstance.blame, data = d)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8.7888 -2.2905  0.1533  2.3357  8.2112 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    20.4228     1.9477  10.486  < 2e-16 ***
#  self.blame                      4.7119     0.9370   5.029 2.36e-06 ***
#  circumstance.blame             -0.8122     0.7102  -1.144   0.2557    
#self.blame:circumstance.blame  -0.5337     0.2810  -1.899   0.0606 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.697 on 94 degrees of freedom
#Multiple R-squared:  0.4684,	Adjusted R-squared:  0.4515 
#F-statistic: 27.61 on 3 and 94 DF,  p-value: 6.811e-13

#From this model we can observe that the effect of circumstance.blame is based on the variable self.blame. However, after scaling the variables, the result has changed, there is no interaction effect between self.blame and circumstance.blame. 

m5 <- lm(distress ~ scale(self.blame) * scale(circumstance.blame), d)
summary(m5) 
#Call:
#  lm(formula = distress ~ scale(self.blame) * scale(circumstance.blame), 
#     data = d)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8.7888 -2.2905  0.1533  2.3357  8.2112 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                  25.4177     0.4307  59.016  < 2e-16 ***
#  scale(self.blame)                             4.0348     0.4536   8.895 4.11e-14 ***
#  scale(circumstance.blame)                    -2.5054     0.4322  -5.797 8.98e-08 ***
#  scale(self.blame):scale(circumstance.blame)  -0.8310     0.4375  -1.899   0.0606 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.697 on 94 degrees of freedom
#Multiple R-squared:  0.4684,	Adjusted R-squared:  0.4515 
#F-statistic: 27.61 on 3 and 94 DF,  p-value: 6.811e-13

d <- d %>% 
  mutate(self.blame_cat = cut(self.blame, 3, labels = c("low", "medium", "high")),
         circumstance.blame_cat = cut(circumstance.blame, 3, labels = c("low", "medium", "high")))

p1 <- ggplot(d, aes(x = self.blame, y = distress, 
                     color = circumstance.blame_cat, shape = circumstance.blame_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "bottom")

p2 <- ggplot(d, aes(x = circumstance.blame, y = distress, 
              color = self.blame_cat, shape = self.blame_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "bottom")

install.packages("Rmisc")
library("Rmisc")
ggsave("interaction model.pdf", multiplot(p1, p2))
