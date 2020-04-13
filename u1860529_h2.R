############################         Student Number: 1860529         ##############################

###################################################################################################
#1. Load the data into R and transform the character variables into factors.
###################################################################################################

install.packages("tidyverse")
library("tidyverse")

#I use "******" in order to not show my name here. 
don <- read_csv("/Users/******/Desktop/COURSES/Term 1/Methods and Analysis in Behavioural Science/Homework 4/donations.csv") %>% 
  as.tibble()
str(don)
#Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	120 obs. of  4 variables:
#$ id      : int  1 2 3 4 5 6 7 8 9 10 ...
#$ nudge   : chr  "information" "information" "information" "information" ...
#$ eyes    : chr  "yes" "yes" "yes" "yes" ...
#$ donation: int  10 9 3 7 11 12 4 8 2 9 ...

don_1 <- don %>%
  mutate(nudge = factor(nudge, levels = c("information", "personal", "matching"), 
                        labels = c("information", "personal", "matching")),
         eyes = factor(eyes, levels = c("no", "yes"), 
                       labels = c("no", "yes")))
str(don_1)
#Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	120 obs. of  4 variables:
#$ id      : int  1 2 3 4 5 6 7 8 9 10 ...
#$ nudge   : Factor w/ 3 levels "information",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ eyes    : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
#$ donation: int  10 9 3 7 11 12 4 8 2 9 ...

###################################################################################################
#2. Fit a model of the data involving both factors and their interaction using lm(). Make sure the parameter estimate represent the overall or average effects.
###################################################################################################

install.packages("afex")
library("afex")
afex::set_sum_contrasts()

fit_2 <-lm(donation ~ nudge * eyes, don_1)
summary(fit_2)
#Call:
#lm(formula = donation ~ nudge * eyes, data = don_1)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-9.200 -3.300  0.350  3.025 10.400 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   15.0083     0.4103  36.581   <2e-16 ***
#nudge1        -6.0083     0.5802 -10.355   <2e-16 ***
#nudge2        -0.1583     0.5802  -0.273    0.785    
#eyes1         -0.1417     0.4103  -0.345    0.731    
#nudge1:eyes1  -0.5583     0.5802  -0.962    0.338    
#nudge2:eyes1  -5.6083     0.5802  -9.666   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.494 on 114 degrees of freedom
#Multiple R-squared:  0.7144,	Adjusted R-squared:  0.7019 
#F-statistic: 57.03 on 5 and 114 DF,  p-value: < 2.2e-16

###################################################################################################
#3. Use car::Anova() to test the model terms of this model. Use Type 3 sums of squares.
###################################################################################################

install.packages("car")
library("car")

fit_3 <- Anova(fit_2, type = 3)
fit_3
#Anova Table (Type III tests)
#
#Response: donation
#             Sum Sq  Df   F value Pr(>F)    
#(Intercept) 27030.0   1 1338.1483 <2e-16 ***
#nudge        2966.1   2   73.4203 <2e-16 ***
#eyes            2.4   1    0.1192 0.7305    
#nudge:eyes   2791.7   2   69.1034 <2e-16 ***
#  Residuals    2302.7 114                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###################################################################################################
#4. Use afex function aov_car() to fit the same model.
###################################################################################################

fit_4 <- aov_car(donation ~ nudge * eyes + Error(id), don_1)
fit_4
#Anova Table (Type 3 tests)
#
#Response: donation
#      Effect     df   MSE         F  ges p.value
#1      nudge 2, 114 20.20 73.42 ***  .56  <.0001
#2       eyes 1, 114 20.20      0.12 .001     .73
#3 nudge:eyes 2, 114 20.20 69.10 ***  .55  <.0001
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

###################################################################################################
#5. Use afex function aov_ez() to fit the same model.
###################################################################################################

fit_5 <- aov_ez(id = "id", dv = "donation", don_1, between = c("nudge", "eyes"))
fit_5
#Anova Table (Type 3 tests)
#
#Response: donation
#      Effect     df   MSE         F  ges p.value
#1      nudge 2, 114 20.20 73.42 ***  .56  <.0001
#2       eyes 1, 114 20.20      0.12 .001     .73
#3 nudge:eyes 2, 114 20.20 69.10 ***  .55  <.0001
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

###################################################################################################
#6. Which of the three effects are significant, which are not significant?
###################################################################################################

#From the previous three models we can see that the F-values and p-values for the independent variables are : F-values are: 73.42, 0.12, 69.10; p-values are: 0, 0.73, 0. Therefore, we can tell that coefficients of nudge variable and the interaction between "nudge" and "eyes" are significant. While the coefficient of eyes variable is not significant enough on a significance level of 95%. 

#Therefore, the effects of "nudge" and the interaction between "nudge" and "eyes" are significant, the effect of "eyes" itself is not significant. 

###################################################################################################
#7. Use afex_plot to inspect and understand the interaction.
###################################################################################################

install.packages("GGally")
library("GGally")
install.packages("ggpol")
library("ggpol")
install.packages("ggbeeswarm")
library("ggbeeswarm")
install.packages("cowplot")
library("cowplot")

theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom"))

#The interaction with "nudge" on the x-axis:
p1 <- afex_plot(fit_4, x = "nudge", trace = "eyes", error = "model", 
                mapping = c("linetype", "shape", "fill"),
                data_geom = geom_violin, 
                data_arg = list(width = 0.5))
p2 <- afex_plot(fit_4, x = "nudge", trace = "eyes", error = "model", dodge = 0.5,
                mapping = c("shape", "color"),
                data_geom = geom_beeswarm,
                data_arg = list(
                  dodge.width = 0.5,  
                  cex = 0.8))
p3 <- afex_plot(fit_4, x = "nudge", trace = "eyes", error = "model", 
                mapping = c("shape", "fill"),
                data_geom = geom_boxplot, 
                data_arg = list(width = 0.3))
p4 <- afex_plot(fit_4, "nudge", "eyes",
                mapping = list("fill"),
                data_geom= geom_boxjitter,
                data_arg= list(width = 0.7, 
                               jitter.width = 0.12, 
                               jitter.color = "darkgrey",
                               outlier.color = "darkgrey",
                               outlier.intersect = TRUE)) +
  geom_line(aes(group = 1), size = 1.5) +
  theme(legend.position= "bottom")
interaction_7_1 <- plot_grid(p1, p2, p3, p4, ncol = 2, labels = 1:4) 
interaction_7_1

#The interaction with "eyes" on the x-axis:
p5 <- afex_plot(fit_4, x = "eyes", trace = "nudge", error = "model", 
                mapping = c("linetype", "shape", "fill"),
                data_geom = geom_violin, 
                data_arg = list(width = 0.5))
p6 <- afex_plot(fit_4, x = "eyes", trace = "nudge", error = "model", dodge = 0.5,
                mapping = c("shape", "color"),
                data_geom = geom_beeswarm,
                data_arg = list(
                  dodge.width = 0.5,  
                  cex = 0.8))
p7 <- afex_plot(fit_4, x = "eyes", trace = "nudge", error = "model", 
                mapping = c("shape", "fill"),
                data_geom = geom_boxplot, 
                data_arg = list(width = 0.3))
p8 <- afex_plot(fit_4,  "eyes", "nudge",
                mapping = list("fill"),
                data_geom= geom_boxjitter,
                data_arg= list(width = 0.7, 
                               jitter.width = 0.12, 
                               jitter.color = "darkgrey",
                               outlier.color = "darkgrey",
                               outlier.intersect = TRUE)) +
  geom_line(aes(group = 1), size = 1.5) +
  theme(legend.position= "bottom")
interaction_7_2 <- plot_grid(p5, p6, p7, p8, ncol = 2, labels = 1:4) 
interaction_7_2

#From these two plot graphs, we can see the interaction between variables "nudge" and "eyes". The interaction between the information and personal factor by the eyes variable is ordinal; while the interaction between the personal and matching factor by the eyes variable is disordinal. 

#The interaction effect of the personal factor by eyes variable is positive; the interaction effect of information factor by eyes variable is slightly positive; the interaction effect of matching factor by eyes variable is negative. 

###################################################################################################
#8. Use emmeans to conduct follow-up tests of the interaction. Is there one particular design cell or cells that elicits the most donations in a statistically significant manner? Can you find an ordering among the cells or combination of cells?
###################################################################################################

install.packages("emmeans")
library("emmeans")

em_8_1 <- emmeans(fit_2, c("nudge", "eyes")) 
em_8_1
# nudge       eyes emmean       SE  df  lower.CL upper.CL
#information no     8.30 1.004977 114  6.309149 10.29085
#personal    no     9.10 1.004977 114  7.109149 11.09085
#matching    no    27.20 1.004977 114 25.209149 29.19085
#information yes    9.70 1.004977 114  7.709149 11.69085
#personal    yes   20.60 1.004977 114 18.609149 22.59085
#matching    yes   15.15 1.004977 114 13.159149 17.14085
#
#Confidence level used: 0.95 

#In order to see the combinations of the cells more clear, I reorder em_8 based on the descending order of the "emmean" of the factors.
em_8_2 <- em_8_1 %>% 
  as.tibble() %>%
  arrange(desc(emmean))
em_8_2
## A tibble: 6 x 7
#nudge       eyes  emmean    SE    df lower.CL upper.CL
#<fct>       <fct>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#1 matching    no     27.2   1.00   114    25.2      29.2
#2 personal    yes    20.6   1.00   114    18.6      22.6
#3 matching    yes    15.2   1.00   114    13.2      17.1
#4 information yes     9.70  1.00   114     7.71     11.7
#5 personal    no      9.1   1.00   114     7.11     11.1
#6 information no      8.30  1.00   114     6.31     10.3

plot(em_8_1, comparisons = TRUE, adjust = "holm")

pairs(contrast(em_8_1), adjust = "holm")
#contrast                                       estimate       SE  df t.ratio p.value
#information,no effect - personal,no effect        -0.80 1.421252 114  -0.563  1.0000
#information,no effect - matching,no effect       -18.90 1.421252 114 -13.298  <.0001
#information,no effect - information,yes effect    -1.40 1.421252 114  -0.985  0.9801
#information,no effect - personal,yes effect      -12.30 1.421252 114  -8.654  <.0001
#information,no effect - matching,yes effect       -6.85 1.421252 114  -4.820  <.0001
#personal,no effect - matching,no effect          -18.10 1.421252 114 -12.735  <.0001
#personal,no effect - information,yes effect       -0.60 1.421252 114  -0.422  1.0000
#personal,no effect - personal,yes effect         -11.50 1.421252 114  -8.091  <.0001
#personal,no effect - matching,yes effect          -6.05 1.421252 114  -4.257  0.0003
#matching,no effect - information,yes effect       17.50 1.421252 114  12.313  <.0001
#matching,no effect - personal,yes effect           6.60 1.421252 114   4.644  0.0001
#matching,no effect - matching,yes effect          12.05 1.421252 114   8.478  <.0001
#information,yes effect - personal,yes effect     -10.90 1.421252 114  -7.669  <.0001
#information,yes effect - matching,yes effect      -5.45 1.421252 114  -3.835  0.0010
#personal,yes effect - matching,yes effect          5.45 1.421252 114   3.835  0.0010
#
#P value adjustment: holm method for 15 tests 

#From the emmeans model and the plot above we can see that the mean of the matching-no effect is about 27.2, which is the largest among all. Moreover, the results of the contrast show that the matching-no effect is significant. Therefore, the cells of matching-no elicit the most donations in a statistically significant manner.  

interaction_8_1 <- list (
  matchno_personyes = c(0, 0, 1, 0, -1, 0),
  personyes_matchyes = c(0, 0, 0, 0, 1, -1),
  matchyes_inforyes = c(0, 0, 0, -1, 0, 1),
  inforyes_personno = c(0, -1, 0, 1, 0, 0),
  personno_inforno = c(-1, 1, 0, 0, 0, 0))
contrast(em_8_1, interaction_8_1, adjust = "holm")
#contrast           estimate       SE  df t.ratio p.value
#matchno_personyes      6.60 1.421252 114   4.644  <.0001
#personyes_matchyes     5.45 1.421252 114   3.835  0.0008
#matchyes_inforyes      5.45 1.421252 114   3.835  0.0008
#inforyes_personno      0.60 1.421252 114   0.422  1.0000
#personno_inforno       0.80 1.421252 114   0.563  1.0000
#
#P value adjustment: holm method for 5 tests  

#From the estimates and p-values of the tests above we can see that, the ordering among the cells or combination of cells is: matching-no > personal-yes > matching-yes > information-yes ≈ personal-no ≈ information-no (we cannot reject that there is no difference in the means of information-yes and personal-no, personal-no and information-no). 