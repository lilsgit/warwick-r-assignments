#Install all the necessary packages:
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

#Set the labels large enough to be readable: 
theme_set(theme_bw(base_size = 15))


####################################################################################################################
##                                              Task 1: Loading Data                                              ##
####################################################################################################################

#Here my name showed up in the code so I use white colour to blocked it. 
wbs_1 <-
  read_delim("/Users/tianyi/desktop/Assessment 1/well_being_survey.tab", delim = "\t") %>% 
  as.tibble()

#Get the file as below:
### A tibble: 2,048 x 25
#Casenew INDWGT MCZ_1 MCZ_2 MCZ_3 MCZ_4 MCZ_5 MCZ_7 MCZ_8 MCZ_9 MCZ_10 MCZ_11 MCZ_13 MCZ_17 QHealthr  RSEX AGEXr
#    <int>  <dbl> <int> <int> <int> <int> <int> <int> <int> <chr>  <int> <chr>  <chr>   <int>    <int> <int> <int>
#1      12 2.09e4     9     9     7     5     6     8     9 10         3 3      " "        10        2     2     2
#2      15 9.05e4     8     9     9     2    10     8    10 " "        7 5      10          9        2     1     3
#3      18 5.03e4     7     8     8     0    10     8     8 " "        4 8      5           8        2     1     3
#4      20 4.02e4     8    10    10     0     8     5     6 " "        5 " "    " "        10        3     2     5
#5      54 1.26e5     9     9     8     7     7     7     9 " "        7 9      6           8        1     2     3
#6      54 9.91e4     7     9     6     7     8     9     5 " "       10 10     " "         9        2     2     1
#7      67 4.25e4     9     9     8     1     9     6     9 9          9 8      8          10        2     2     2
#8      89 6.00e4    10    10    10     0     9     9    10 " "        9 9      10         10        1     1     3
#9      91 2.87e4    10    10     9     0    10     5    10 " "       10 " "    " "         8        4     2     6
#10     96 5.91e4     7     6     4     9    10     7     8 " "        8 7      7           8        2     1     4
## ... with 2,038 more rows, and 8 more variables: Martstat3r <int>, HighEd4r <int>, Ethnicity2r <int>,
##   DVILO3ar <int>, FtPtWkr <chr>, NSECAC3 <int>, GorA <int>, INDWGTr <dbl>


####################################################################################################################
##                                             Task 2: Preparing Data                                             ##
####################################################################################################################

wbs_1[wbs_1 == 98| wbs_1 == 99] <- NA 

wbs_2_1 <- wbs_1 %>%
  mutate(MCZ_9 = as.numeric(MCZ_9), 
         MCZ_11 = as.numeric(MCZ_11), 
         MCZ_13 = as.numeric(MCZ_13)) 

wbs_2_2 <- wbs_2_1 %>%
  select(MCZ_1:MCZ_17)

#Ensure that all main variables (i.e., MCZ...) only contain allowed values (i.e., integers from 0 to 10): 
all(wbs_2_2 >= 0 & wbs_2_2 <= 10, na.rm = TRUE) #=TRUE

#wbs_2_1 after data preparation: 
## A tibble: 2,048 x 25
#Casenew INDWGT MCZ_1 MCZ_2 MCZ_3 MCZ_4 MCZ_5 MCZ_7 MCZ_8 MCZ_9 MCZ_10 MCZ_11 MCZ_13 MCZ_17 QHealthr  RSEX AGEXr
#   <int>  <dbl> <int> <int> <int> <int> <int> <int> <int> <dbl>  <int>  <dbl>  <dbl>  <int>    <int> <int> <int>
#1      12 2.09e4     9     9     7     5     6     8     9    10      3      3     NA     10        2     2     2
#2      15 9.05e4     8     9     9     2    10     8    10    NA      7      5     10      9        2     1     3
#3      18 5.03e4     7     8     8     0    10     8     8    NA      4      8      5      8        2     1     3
#4      20 4.02e4     8    10    10     0     8     5     6    NA      5     NA     NA     10        3     2     5
#5      54 1.26e5     9     9     8     7     7     7     9    NA      7      9      6      8        1     2     3
#6      54 9.91e4     7     9     6     7     8     9     5    NA     10     10     NA      9        2     2     1
#7      67 4.25e4     9     9     8     1     9     6     9     9      9      8      8     10        2     2     2
#8      89 6.00e4    10    10    10     0     9     9    10    NA      9      9     10     10        1     1     3
#9      91 2.87e4    10    10     9     0    10     5    10    NA     10     NA     NA      8        4     2     6
#10     96 5.91e4     7     6     4     9    10     7     8    NA      8      7      7      8        2     1     4
## ... with 2,038 more rows, and 8 more variables: Martstat3r <int>, HighEd4r <int>, Ethnicity2r <int>,
##   DVILO3ar <int>, FtPtWkr <chr>, NSECAC3 <int>, GorA <int>, INDWGTr <dbl>

#Total number and proportion of the missing values in the dataset:
sum(is.na(wbs_2_1)) #=3333
mean(is.na(wbs_2_1)) #=0.06509766

#Proportion of missing values for each variable: 
wbs_2_3 <- wbs_2_1 %>%
  gather(key = "all variables", value = "values")  %>%
  group_by(`all variables`) %>%
  summarise(missing_value = mean(is.na(values))) %>%
  arrange(desc(`missing_value`))

#The results:
## A tibble: 25 x 2
#`all variables` missing_value
#   <chr>                   <dbl>
#1  MCZ_9                   0.769
#2  MCZ_13                  0.462
#3  MCZ_11                  0.348
#4  MCZ_2                   0.00684
#5  MCZ_5                   0.00586
#6  MCZ_3                   0.00391
#7  MCZ_1                   0.00293
#8  MCZ_10                  0.00293
#9  MCZ_17                  0.00293
#10 MCZ_4                   0.00244   
# ... with 15 more rows

#As we can see, the proportion of missing values for most variables is no larger than 0.01 (a very small proportion). 
#For MCZ_9, MCZ_11 and MCZ_13, the amount of missing values is slightly problematic, which might due to the design of the survey or the sample itself(systematical problem).


####################################################################################################################
##                                     Task 3: Univariate Exploratory Analysis                                    ##
####################################################################################################################

#Create a new dataset to use function facet_wrap() to combine all the plots:
wbs_3 <- wbs_2_1 %>%
  select(MCZ_1:MCZ_17) %>%
  gather(key = "variables", value = "values") 

ggplot(wbs_3, aes(x = values)) + 
  geom_histogram(aes(y =..density..), binwidth = 0.5, colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  facet_wrap(~ variables) + 
  labs(x = "Well-being", y = "Distribution") +
  ggtitle("Univariate exploratory analysis") 

ggsave("task 3.pdf")


####################################################################################################################
##                                          Task 4: Use Correct Data Types                                        ##
####################################################################################################################

#Transform (or mutate) all nominal variables into factors.
wbs_4 <- wbs_2_1 %>%
  transform(QHealthr = factor(QHealthr, c(1, 2, 3, 4, 5), c("very good", "good", "fair", "bad", "very bad")), 
            RSEX = factor(RSEX, c(1, 2), c("male", "female")), 
            AGEXr = factor(AGEXr, c(1, 2, 3, 4, 5, 6), c("16-24", "25-44", "45-54", "55-64", "65-74", "75+")), 
            Martstat3r = factor(Martstat3r, c(1, 2, 3), c("married", "single", "separated")), 
            HighEd4r = factor(HighEd4r, c(1, 2, 3, 4), c("degree", "below", "other", "none")), 
            Ethnicity2r = factor(Ethnicity2r, c(1, 2), c("white", "other")), 
            DVILO3ar = factor(DVILO3ar, c(1, 2, 3), c("employed", "unemployed", "inactive")), 
            FtPtWkr = factor(FtPtWkr, c(1, 2), c("full-time", "part-time")), 
            NSECAC3 = factor(NSECAC3, c(1, 2, 3, 4), c("higher", "intermediate", "lower", "no-class")), 
            GorA = factor(GorA, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), c("North East", "North West", "Yorkshire", "East Midlands", "West Midlands", "East England", "London", "South east", "South west", "Wales", "Scotland"))) 


####################################################################################################################
##                                         Task 5: Create Composite Scores                                        ##
####################################################################################################################

#Use (10 - MCZ_4) to recode the values of MCZ_4 in the following code: 
wbs_5_1 <- wbs_4 %>%
  rowwise() %>%
  mutate(personal_wb = mean(c(MCZ_1, MCZ_2, MCZ_3, 10 - MCZ_4)),  
         relation_wb = mean(c(MCZ_5)), 
         health_wb = mean(c(MCZ_7, MCZ_8)), 
         work_wb = mean(c(MCZ_10, MCZ_11, MCZ_13)))

#The proportion of missing values in these composite scores:
wbs_5_2 <- wbs_5_1 %>%
  gather(key = "all variables", value = "values")  %>%
  group_by(`all variables`) %>%
  summarise(missing_value = mean(is.na(values))) %>%
  arrange(desc(`missing_value`))

#The results: 
## A tibble: 29 x 2
#`all variables` missing_value
#   <chr>                   <dbl>
#1  MCZ_9                   0.769  
#2  work_wb                 0.511  
#3  MCZ_13                  0.464  
#4  MCZ_11                  0.358  
#5  FtPtWkr                 0.293  
#6  personal_wb             0.00781
#7  MCZ_2                   0.00684
#8  MCZ_5                   0.00586
#9  relation_wb             0.00586
#10 MCZ_3                   0.00391

#Calculate composite scores in a way that handles missing values on a case wise basis:
wbs_5_3 <- wbs_4 %>%
  rowwise() %>% 
  mutate(personal_wb = mean(c(MCZ_1, MCZ_2, MCZ_3, 10 - MCZ_4), na.rm = TRUE),  
         relation_wb = mean(c(MCZ_5), na.rm = TRUE), 
         health_wb = mean(c(MCZ_7, MCZ_8), na.rm = TRUE), 
         work_wb = mean(c(MCZ_10, MCZ_11, MCZ_13), na.rm = TRUE))

#It is appropriate to handle the missing values through the method above for most variables. 
#As we can see in the results of the proportion of missing values, apart from MCZ_9 and work_wb, the amount of missing value is not large much to affect the result of the variables (the largest one is below 0.5). 
#For variables MCZ_9 and work_wb, the amount of missing values is fairly large, so we cannot simply ignore the missing values in this case. 


####################################################################################################################
##                                      Task 6: Bivariate Exploratory Analysis                                    ##
####################################################################################################################

#After trying options for ggplot() to draw the graphs (I tried functions such as geom_dotplot, geom_col, geom_jitter, geom_boxplot and geom_violin), geom_boxplot and geom_violin seem to represent the data better than the other functions. #geom_boxplot can show the median and also the quartiles, geom_violin shows the density. Therefore I use these two for the plots. 

#Plots of the relationship between sex and well-being:
wbs_6_1 <- wbs_5_3 %>%
  select(RSEX, personal_wb:work_wb) %>%
  gather(personal_wb:work_wb, key = "variables", value = "well-being") 

ggplot(wbs_6_1, aes(x = RSEX, y = `well-being`)) + 
  geom_violin(scale = "area", aes(fill = RSEX)) +
  geom_boxplot(notch = FALSE, width = 0.2) +
  facet_wrap(~ variables) +
  stat_summary(fun.y = "mean", geom = "point", shape = 15, color = "grey") + 
  stat_summary(fun.y = "mean", geom = "line", color = "black", aes(group = 1)) + 
  labs(x = "Sex", y = "Well being") +
  scale_fill_brewer(palette = "Greys")

ggsave("sex.pdf")

#Plots of the relationship between marriage and well-being:
wbs_6_2 <- wbs_5_3 %>%
  select(Martstat3r, personal_wb:work_wb) %>%
  gather(personal_wb:work_wb, key = "variables", value = "well-being") 

ggplot(wbs_6_2, aes(x = Martstat3r, y = `well-being`)) + 
  geom_violin(scale = "area", aes(fill = Martstat3r)) +
  geom_boxplot(notch = FALSE, width = 0.2) +
  facet_wrap(~ variables) +
  stat_summary(fun.y = "mean", geom = "point", shape = 15, color = "grey") + 
  stat_summary(fun.y = "mean", geom = "line", color = "black", aes(group = 1)) + 
  labs(x = "Marriage", y = "Well being") +
  scale_fill_brewer(palette = "Greys")

ggsave("marriage.pdf")

#Plots of the relationship between education and well-being:
wbs_6_3 <- wbs_5_3 %>%
  select(HighEd4r, personal_wb:work_wb) %>%
  gather(personal_wb:work_wb, key = "variables", value = "well-being") 

ggplot(wbs_6_3, aes(x = HighEd4r, y = `well-being`)) + 
  geom_violin(scale = "area", aes(fill = HighEd4r)) +
  geom_boxplot(notch = FALSE, width = 0.2) +
  facet_wrap(~ variables) +
  stat_summary(fun.y = "mean", geom = "point", shape = 15, color = "grey") + 
  stat_summary(fun.y = "mean", geom = "line", color = "black", aes(group = 1)) + 
  labs(x = "Education", y = "Well being") +
  scale_fill_brewer(palette = "Greys")

ggsave("edu.pdf")

#Plots of the relationship between age and well-being:
wbs_6_4 <- wbs_5_3 %>%
  select(AGEXr, personal_wb:work_wb) %>%
  gather(personal_wb:work_wb, key = "variables", value = "well-being") 

ggplot(wbs_6_4, aes(x = AGEXr, y = `well-being`)) + 
  geom_violin(scale = "area", aes(fill = AGEXr)) +
  geom_boxplot(notch = FALSE, width = 0.2) +
  facet_wrap(~ variables) +
  stat_summary(fun.y = "mean", geom = "point", shape = 15, color = "grey") + 
  stat_summary(fun.y = "mean", geom = "line", color = "black", aes(group = 1)) + 
  labs(x = "Age", y = "Well being") +
  scale_fill_brewer(palette = "Greys")

ggsave("age.pdf")

#Plots of the relationship between employment and well-being:
wbs_6_5 <- wbs_5_3 %>%
  select(NSECAC3, personal_wb:work_wb) %>%
  gather(personal_wb:work_wb, key = "variables", value = "well-being") 

ggplot(wbs_6_5, aes(x = NSECAC3, y = `well-being`)) + 
  geom_violin(scale = "area", aes(fill = NSECAC3)) +
  geom_boxplot(notch = FALSE, width = 0.2) +
  facet_wrap(~ variables) +
  stat_summary(fun.y = "mean", geom = "point", shape = 15, color = "grey") + 
  stat_summary(fun.y = "mean", geom = "line", color = "black", aes(group = 1)) + 
  labs(x = "Employment effects", y = "Well being") +
  scale_fill_brewer(palette = "Greys")

ggsave("em.pdf")

