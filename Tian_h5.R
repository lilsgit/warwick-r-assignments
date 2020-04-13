install.packages("tidyverse")
install.packages("readr")
library("tidyverse")
library(readr)

########
#Part 1#
########

#Using ANOVAs to get the results from the paper: 
pet1 <- read_csv("/Users/tianyi/Desktop/COURSES/Term 1/Methods and Analysis in Behavioural Science/Homework 5/pettibone12.csv") %>%
  mutate(subject = as.factor(subject),
         session = factor(session, levels = c("Short Delay", "Medium Delay", "Long Delay", "Extra Long"), 
                          labels = c("2s", "4s", "6s", "8s")), 
         sex = factor(sex, levels = c("Female", "Male"), 
                      labels = c("Female", "Male")), 
         condition = factor(condition, levels = c("ad", "c"), 
                            labels = c("symmetric_dominance", "compromise"))) %>%
  as.tibble()
str(pet1)
#Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	164 obs. of  6 variables:
#$ subject  : Factor w/ 164 levels "ad_1","ad_10",..: 1 45 80 5 9 14 18 22 27 31 ...
#$ session  : Factor w/ 4 levels "2s","4s","6s",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ age      : int  18 18 18 18 18 18 20 21 19 18 ...
#$ sex      : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 1 1 1 2 1 ...
#$ condition: Factor w/ 2 levels "symmetric_dominance",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ avgrt    : num  747 976 836 1098 955 ...

install.packages("afex")
library("afex")
afex::set_sum_contrasts()
fit1 <- lm(avgrt ~ condition * session, pet1)
summary(fit1)

install.packages("car")
library("car")
fit2 <- Anova(fit1, type = 3)
fit3 <- aov_car(avgrt ~ condition * session + Error(subject), pet1)
fit4 <- aov_ez(id = "subject", dv = "avgrt", pet1, between = c("condition", "session"))
summary(fit2)
summary(fit3)
summary(fit4)

install.packages("emmeans")
library("emmeans")
em1 <- emmeans(fit1, c("condition", "session"))
em2 <- em1 %>% 
  as.tibble() %>%
  arrange(desc(emmean))
em2
## A tibble: 8 x 7
#condition           session emmean    SE    df lower.CL upper.CL
#<fct>               <fct>    <dbl> <dbl> <dbl>    <dbl>    <dbl>
#1 compromise          2s       1201.  128.   156     947.    1454.
#2 symmetric_dominance 6s       1195.  132.   156     935.    1455.
#3 symmetric_dominance 2s       1062.  132.   156     802.    1322.
#4 symmetric_dominance 4s       1061.  132.   156     801.    1321.
#5 compromise          4s        875.  125.   156     627.    1123.
#6 compromise          8s        849.  132.   156     589.    1109.
#7 compromise          6s        754.  128.   156     500.    1007.
#8 symmetric_dominance 8s        711.  132.   156     451.     971.
plot(em1, comparisons = TRUE, adjust = "holm")

pairs(contrast(em1), adjust = "holm")

interaction1 <- list (
  c2_s6 = c(0, 1, 0, 0, -1, 0, 0, 0),
  s6_s2 = c(-1, 0, 0, 0, 1, 0, 0, 0),
  s2_s4 = c(1, 0, -1, 0, 0, 0, 0, 0),
  s4_c4 = c(0, 0, 1, -1, 0, 0, 0, 0),
  c4_c8 = c(0, 0, 0, 1, 0, 0, 0, -1), 
  c8_c6 = c(0, 0, 0, 0, 0, -1, 0, 1), 
  c6_s8 = c(0, 0, 0, 0, 0, 1, -1, 0))
contrast(em1, interaction1, adjust = "holm")
# contrast  estimate       SE  df t.ratio p.value
#c2_s6      5.54500 183.8402 156   0.030  1.0000
#s6_s2    133.23500 186.0687 156   0.716  1.0000
#s2_s4      0.86000 186.0687 156   0.005  1.0000
#s4_c4    185.97818 181.7907 156   1.023  1.0000
#c4_c8     26.15182 181.7907 156   0.144  1.0000
#c8_c6     95.25381 183.8402 156   0.518  1.0000
#c6_s8     42.69119 183.8402 156   0.232  1.0000
#
#P value adjustment: holm method for 7 tests 

interaction2 <- list (
  '2s_4s' = c(1/2, 1/2, -1/2, -1/2, 0, 0, 0, 0), 
  '2s_6s' = c(1/2, 1/2, 0, 0, -1/2, -1/2, 0, 0), 
  '2s_8s' = c(1/2, 1/2, 0, 0, 0, 0, -1/2, -1/2)
)
contrast(em1, interaction2, adjust = "holm")
#contrast estimate       SE  df t.ratio p.value
#2s_4s    163.2391 129.2721 156   1.263  0.4171
#2s_6s    156.8944 129.9947 156   1.207  0.4171
#2s_8s    351.3525 130.7849 156   2.686  0.0240
#
#P value adjustment: holm method for 3 tests 

#Testing residuals: 
pet1$residuals <- residuals(fit1)
library("ggplot2")
ggplot(pet1, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line()
ggplot(pet1, aes(residuals)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(pet1$residuals), sd = sd(pet1$residuals)), 
                size = 2, color = "blue") +
  geom_histogram(aes(y=..density..), binwidth = 25)

#Transforming data: 
pet1 <- pet1 %>%
  mutate(log_avgrt = log(avgrt))
fit1_1 <- lm(log_avgrt ~ condition * session, pet1)  
pet1$residuals1 <- residuals(fit1_1)
ggplot(pet1, aes(sample = residuals1)) +
  stat_qq() +
  stat_qq_line()

p1 <- ggplot(pet1, aes(residuals1)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(pet1$residuals1), sd = sd(pet1$residuals1)), 
                size = 2, color = "blue") +
  geom_histogram(aes(y=..density..), binwidth = 0.05)

pet1 <- pet1 %>%
  mutate(avgrt_inv = 1/avgrt)
fit1_2 <- lm(log_avgrt ~ condition * session, pet1)  
pet1$residuals2 <- residuals(fit1_2)
ggplot(pet1, aes(sample = residuals2)) +
  stat_qq() +
  stat_qq_line()
ggplot(pet1, aes(residuals2)) +
  geom_histogram(aes(y=..density..))

p2 <- ggplot(pet1, aes(residuals2)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(pet1$residuals2), sd = sd(pet1$residuals2)), 
                size = 2, color = "blue") +
  geom_histogram(aes(y=..density..), binwidth = 0.05)

install.packages("cowplot")
library("cowplot")
plot_grid(p1, p2)
#As we can see the graphs, these two transformations give the same result of the normality of the residuals. Often times, response times are analyzed on the level of seconds and not on the level of milliseconds (as given here)., but this does not have any influence on the results when considering the transformations. 

########
#Part 2#
########

#Task 1

rus1 <- read_csv("/Users/tianyi/Desktop/COURSES/Term 1/Methods and Analysis in Behavioural Science/Homework 5/russ_18_e3.csv") %>% 
  as.tibble() %>%
  mutate(gender = as.factor(gender),
         lineup_1 = as.factor(lineup_1),
         resp_1 = as.factor(resp_1),
         cresp_1 = as.factor(cresp_1),
         lineup_2 = as.factor(lineup_2),
         resp_2 = as.factor(resp_2),
         cresp_2 = as.factor(cresp_2),
         lineup_3 = as.factor(lineup_3),
         resp_3 = as.factor(resp_3),
         cresp_3 = as.factor(cresp_3),
         lineup_4 = as.factor(lineup_4),
         resp_4 = as.factor(resp_4),
         cresp_4 = as.factor(cresp_4),
         lineup_5 = as.factor(lineup_5),
         resp_5 = as.factor(resp_5),
         cresp_5 = as.factor(cresp_5),
         lineup_6 = as.factor(lineup_6),
         resp_6 = as.factor(resp_6),
         cresp_6 = as.factor(cresp_6)
  )
str(rus1)

rus2 <- rus1 %>%
  select(lineup_1, accuracy_1) %>%
  separate(lineup_1, c("saw", "target"), sep = " ") %>%
  group_by(saw, target) %>%
  summarise(ac1 = mean(accuracy_1)*100)

rus3 <- rus1 %>%
  select(lineup_2, accuracy_2) %>%
  separate(lineup_2, c("saw", "target"), sep = " ") %>%
  group_by(saw, target) %>%
  summarise(ac2 = mean(accuracy_2)*100)

rus4 <- rus1 %>%
  select(lineup_3, accuracy_3) %>%
  separate(lineup_3, c("saw", "target"), sep = " ") %>%
  group_by(saw, target) %>%
  summarise(ac3 = mean(accuracy_3)*100)

rus5 <- rus1 %>%
  select(lineup_4, accuracy_4) %>%
  separate(lineup_4, c("saw", "target"), sep = " ") %>%
  group_by(saw, target) %>%
  summarise(ac4 = mean(accuracy_4)*100)

rus6 <- rus1 %>%
  select(lineup_5, accuracy_5) %>%
  separate(lineup_5, c("saw", "target"), sep = " ") %>%
  group_by(saw, target) %>%
  summarise(ac5 = mean(accuracy_5)*100)

rus7 <- rus1 %>%
  select(lineup_6, accuracy_6) %>%
  separate(lineup_6, c("saw", "target"), sep = " ") %>%
  group_by(saw, target) %>%
  summarise(ac6 = mean(accuracy_6)*100)

rus8 <- rus2 %>%
  left_join(rus3) %>%
  left_join(rus4) %>%
  left_join(rus5) %>%
  left_join(rus6) %>%
  left_join(rus7) %>%
  rowwise() %>% 
  mutate(`% of obseration` = (ac1 + ac2 + ac3 + ac4 + ac5 + ac6)/6)

ggplot(rus8, aes(saw, `% of obseration`)) + 
  geom_bar(stat = "identity", size = 0.5) +  
  facet_grid(~ target) + 
  ggtitle("Individual lineups") + 
  xlab(" ") 

rus9 <- rus1 %>%
  select(accuracy_1, accuracy_2, accuracy_3, accuracy_4, accuracy_5, accuracy_6) %>% 
  distinct(accuracy_1, accuracy_2, accuracy_3, accuracy_4, accuracy_5, accuracy_6,.keep_all=TRUE) %>%
  rowwise() %>%
  mutate(l1 = mean(accuracy_1), 
         l2 = mean(accuracy_2),
         l3 = mean(accuracy_3),
         l4 = mean(accuracy_4),
         l5 = mean(accuracy_5),
         l6 = mean(accuracy_6)) %>%
  gather(l1, l2, l3, l4, l5, l6, key = "key", value = "value") %>%
  group_by(key) %>%
  summarise(`% of obseration` = mean(value))

rus9 <- base::unique(rus9)

ggplot(rus9, aes(key, `% of obseration`)) + 
  geom_bar(stat = "identity") +  
  ggtitle("Target") + 
  xlab(" ")   

#Task 2
rus10 <- rus1 %>%
  select(accuracy_1, accuracy_2, accuracy_3, accuracy_4, accuracy_5, accuracy_6, ot_hits, ot_cr, cfmt) %>%
  #rowwise() %>%
  mutate(acc = accuracy_1 + accuracy_2 + accuracy_3 + accuracy_4 + accuracy_5 + accuracy_6) %>%
  group_by(ot_hits, ot_cr) %>%
  distinct(ot_hits, ot_cr,.keep_all=TRUE) %>%
  ungroup() 

cor.test(rus10$acc, rus10$ot_hits)
cor.test(rus10$acc, rus10$ot_cr)
cor.test(rus10$acc, rus10$cfmt)

summary(lm(acc ~ ot_hits + ot_cr + cfmt, rus10))

rus11 <- rus10 %>%
  select(ot_hits:acc) %>%
  mutate(cfmt = cfmt/100) %>%
  gather(ot_hits, ot_cr, cfmt, key = "key", value = "value")

ggplot(rus11, aes(acc, value)) + 
  geom_dotplot(binaxis = "y", stackdir = "center") +
  geom_smooth(method = 'loess') +
  facet_grid(~ key)







  


