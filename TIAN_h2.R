#Part1. 
install.packages("tidyverse")
install.packages("dplyr")
library("tidyverse")
library(dplyr)

#1.Load in well-being-wide.csv using read_csv as wb1. 
wb1 <- read.csv("/Users/tianyi/Desktop/Homework 2/well-being-wide.csv")

#2.Create a new tidy version of wb1 called wb2. 
wb2 <- wb1 %>% 
  gather(`t1`, `t2`, `t3`, `t4`,`t5`, key = "time", value = "well_being")
wb2

#3.The mean well-being at each time point. 
v <- c("t1", "t2", "t3", "t4", "t5")
for (i in v) {
  mean_wb <- mean(wb2[wb2$time == i,]$`well_being`)
  print(mean_wb)
}
#The outputs above are equal to the values of the followings:
mean(wb2[wb2$time == "t1",]$`well_being`)
mean(wb2[wb2$time == "t2",]$`well_being`)
mean(wb2[wb2$time == "t3",]$`well_being`)
mean(wb2[wb2$time == "t4",]$`well_being`)
mean(wb2[wb2$time == "t5",]$`well_being`)

#4.The mean well-being at each time point, separated by sex. 
v <- c("t1", "t2", "t3", "t4", "t5")
u <- c("Male", "Female")
for (i in v) {
  for (j in u)
  {mean_wb_sex <- mean(wb2[wb2$time == i & wb2$sex == j,]$`well_being`)
       print(mean_wb_sex)}
}
#The outputs above are equal to the values of the followings:
mean(wb2[wb2$time == "t1"& wb2$sex == "Male",]$`well_being`)
mean(wb2[wb2$time == "t1"& wb2$sex == "Female",]$`well_being`)
mean(wb2[wb2$time == "t2"& wb2$sex == "Male",]$`well_being`)
mean(wb2[wb2$time == "t2"& wb2$sex == "Female",]$`well_being`)
mean(wb2[wb2$time == "t3"& wb2$sex == "Male",]$`well_being`)
mean(wb2[wb2$time == "t3"& wb2$sex == "Female",]$`well_being`)
mean(wb2[wb2$time == "t4"& wb2$sex == "Male",]$`well_being`)
mean(wb2[wb2$time == "t4"& wb2$sex == "Female",]$`well_being`)
mean(wb2[wb2$time == "t5"& wb2$sex == "Male",]$`well_being`)
mean(wb2[wb2$time == "t5"& wb2$sex == "Female",]$`well_being`)

#5.Create wb_pre, that only contains the data from time points t1 and t2. 
wb_pre <- wb2 %>%
  filter(time == "t1" | time == "t2")
#Change the date frame into tibble form:
as.tibble(wb_pre) 
wb_pre

#6.Add a new column well_being_z to wb2 that contains the by-sex standarized well being [standaridzed = (value - mean(value)) / sd(value)) ]. 
wb2_z <- wb2 %>%
  group_by(sex) %>% 
  mutate(well_being_z = (`well_being` - mean(`well_being`) / sd(`well_being`)))
wb2_z

#7. Add a new column pre_post to wb2
wb2_p <- wb2 %>%
  mutate(pre_post = ifelse(wb2$time == "t1" | wb2$time == "t2", "pre", "post"))
wb2_p

#8.Create a new tibble with the name av_wb, that has the average well-being as well as the average standarized well-being before the intervention (i.e., average of t1 and t2) and after the intervention (i.e., average of t3 to t5) per participant. This tibble should also contain the sex variable.
av_wb <- wb2_p %>%
  mutate(well_being_z = (`well_being` - mean(`well_being`)) / sd(`well_being`)) %>% #Get the average standarized well-being
  group_by(sub, pre_post, sex) %>% #Per participant
  summarise(av_well_being = mean(`well_being`), av_well_being_z = mean(well_being_z))
as.tibble(av_wb)
av_wb

#9. Calculate the difference between pre and post well-being value for each participant. Which participant has the largest and which has the smallest difference?
av_wb_d <- av_wb %>%
  group_by(sub) %>%
  summarise(differ_wb = max(`av_well_being`) - min(`av_well_being`)) 
av_wb_d
#Participant s6 has the largest difference, s5 has the smallest difference. 

#10. Calculate the difference between pre and post standardized well-being value for each participant. Which participant has the largest and which has the smallest difference?
av_wb_d_z <- av_wb %>%
  group_by(sub) %>%
  summarise(differ_wb_z = max(`av_well_being_z`) - min(`av_well_being_z`)) 
av_wb_d_z
#Participant s6 has the largest difference, s5 has the smallest difference. 


#Part2. 

#1.Write a function h1 which uses a for-loop and the explicit formulation h(x, n) = 1 + x + x2 + ...xn 
h1 <- function(x, n) {
  h1 = 0
  for (i in 0:n) 
    h1 =  h1 + (x^i)
    return(h1)
}
#Plug in the values from 5. to check if the function works:
h1(0.3, 55)  
h1(6.6, 8)
h1(1, 12)

#2.Write a function h2 which uses a while-loop and the explicit formulation h(x, n) = 1 + x + x2 + ...xn 
h2 <- function(x, n) {
  h2 = 0
  i = 0
  while (i <= n) {
    h2 = h2 + (x^i)
    i = i + 1
  }
  return(h2)
}
#Plug in the values from 5. to check if the function works:
h2(0.3, 55)
h2(6.6, 8)
h2(1, 12)

#3.Write a function h3 which uses no loop, but vectorised operations for the explicit formulation h(x, n) = 1 + x + x2 + ...xn
h3 <- function(x, n) {
  h3 = sum(x^(seq.int(0,n,1))) 
  return(h3)
}
#Plug in the values from 5. to check if the function works:
h3(0.3, 55)
h3(6.6, 8)
h3(1, 12)

#4.Write a function h4 which uses no loop and the geometric sequence h(x, n)
h4 <- function(x, n) {
  h4 <- ifelse(x != 1, (1 - x ^ (n+1)) / (1 - x), n + 1)
  return(h4)
}
#Plug in the values from 5. to check if the function works:
h4(0.3, 55)
h4(6.6, 8)
h4(1, 12)
#When x = 1, the value of h4 becomes n + 1.

#5.Check functions h1, h2, h3, and h4 against the values given in the table
# x     n    h(x, n)
# 0.3   55   1.428571
# 6.6   8    4243335.538178
# 1     12   13
#Use the values of x and n in the table to find the values of h1, h2, h3 and h4, I can get the values as the table shows.


#Part3.

#1.Write function f1 that implement the formula given in this table. This function should use a for-loop and if-statements
f1 <- function(x) {
  f1 <- vector("double", length(x))
  for (i in 1:length(x)) 
    f1[i] <- 
      if (x[i] <= 0) {
        -x[i]^3 
      } else if (x[i] >1) {
        sqrt(x[i])
      } else {
        x[i]^2
      }
  return(f1)
}

#2.Write function f2 that implements the formula given in the table of 2.6. This function should be vectorized
f2 <- function(x) {
  f2 = ifelse(x <= 0, (-x^3), ifelse(x > 1, sqrt(x), (x^2)))
  return(f2)
}

#3.Create a vector of x-values as shown in the following and use these x-values to create the following plot:
x.values <- seq(-2, 2, by = 0.1)
plot(x.values, f1(x.values), type = "l") 
plot(x.values, f2(x.values), type = "l")
#Use f1 or f2, I get the graph as the plots in the homework2 show.