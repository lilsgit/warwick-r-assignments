#part2.1, creating variables x, a and b
x <- 12  
a <- 2
b <- 4

#part2.1, set the variable z
#1 
z <- x^(a^b)
#2
z <- (x^a)^b
#3
z <- 3*(x^3) + 2*(x^2) + 6*x + 1
#4
z <- z + 1

#part2.2, give R expressions that return the following vectors
#1
(M <- seq(1, 8, by = 1))
(N <- seq(7, 1, by = -1))
(R <- c(M, N))
#vector1 <- c(1:8, 7:1)
#2
(A <- rep(1, 1))
(B <- rep(2, 2))
(C <- rep(3, 3))
(D <- rep(4, 4))
(E <- rep(5, 5))
(R <- c(A, B, C, D, E))  
#vrep<-rep(seq(1:5),seq(1:5))
#vector2 <- rep(1:5, 1:5)

#part2.3, use R to produce a vector containing all integers from 1 to 100 that are not divisible by 2, 3, or 7
r <- 1:100
(R <- r[(r%%2 != 0) & (r%%3 != 0) & (r%%7 != 0)])

#part2.4
queue <- c("Steve", "Russell", "Alison", "Liam")
#1.Barry arrives
queue[5] <- "Barry"  
(R <- queue)
#2.Steve is served
queue[-1]
#3.Pam talks her way to the front with one item
queue[1] <- "Pam"
(R <- queue)
#4.Barry gets impatient and leaves
queue[-5]
#5.Alison gets impatient and leaves
queue <- queue[-5]
queue
which(queue == "Alison")
queue <- queue[-3]
queue
which(queue == "Russell")
(R <- queue)
#queue<-queue[-which(queue=="Alison")]

#part3
install.packages("carData")
library(carData)
#1.numbers of rows and columns Salaries has
ncol(Salaries) #=6
nrow(Salaries) #=397
#2.numbers of columns are numerical, numbers of columns contain factors
as.numeric(ncol(Salaries)) #should=3
as.factor(ncol(Salaries)) #should=3
str(Salaries) 
#3.create a new data.frame, salaries_a, that only contains the data from discipline = "A"
salaries_a = Salaries[Salaries$discipline == "A",]
nrow(salaries_a) #=181
str(discipline)  
#4.create a new data.frame, salaries_f, that only contains the data from the Female professor at a rank
salaries_f = Salaries[Salaries$sex == "Female" & Salaries$rank == "AssocProf",]
sum(salaries_f$discipline == "A")
sum(salaries_f$discipline == "B")
 number_a = salaries_f[salaries_f$discipline == "A",] #= 4
 number_b = salaries_f[salaries_f$discipline == "B",] #= 6
#5.the percentage of professors that started this year (yrs.service == 0)
#total_p = Salaries[Salaries$rank == "Prof",] #=266
mean(Salaries$yrs.service == 0)
 number_ps0 = Salaries[Salaries$yrs.service == 0,] #=11
 percentage_ps0 <- 11/397
#6.professors in discipline A are professor for more than 10 years
pro_10 = Salaries[ Salaries$ discipline == "A" & Salaries$yrs.service > 10,] #=119
sum(Salaries$ discipline == "A" & Salaries$yrs.service > 10)
#7.add a new column called luck to Salaries and fill it with random samples from the integers 1 to 6
luck <- round(runif(397,1,6))
Salaries <- cbind(Salaries,luck)
#Salaries$luck <- round(runif(397,1,6))
#Salaries$luck <- sample(1:6, nrow(Salaries), replace = TRUE)
#8.the Associate Professors (i.e., rank of AssocProf) got lucky (i.e., have a 6 in column luck)
sum(Salaries$rank == "AssocProf" & Salaries$luck == 6)
sum(Salaries$rank == "AssocProf" & Salaries$luck == 6)/sum(Salaries$rank == "AssocProf")
 number_ap = Salaries[Salaries$rank == "AssocProf",] #=64
 number_ap_luck = Salaries[Salaries$rank == "AssocProf" & Salaries$luck == 6,] #=8
 percentage_ap_luck =  8/64 
  