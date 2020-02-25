setwd()

#read file in R
college <- read.csv("College.csv", header = TRUE)

#read number of missing values
table(is.na(college))

#coloumn wise missing values
summary(college)

#row wise missing values
rowSums(is.na(college))

#remove rows with missing values
college <- na.omit(college)

#total number of students applications:
sum(college$Apps)

#total number of enrollments
sum(college$Enroll)

#average room and board costs 
mean(college$Room.Board) + mean(college$Books) + mean(college$Personal)

#Barplot Out-of-State Tuitio

barplot_data = c(mean(college$Outstate),mean(college$Outstate)*0.43)
barplot(barplot_data)

install.packages("corrplot")
library(corrplot)
data_subset <- college[-c(1,2)]
Matrix <- cor(data_subset)
corrplot(Matrix, method = "number")

#colleges full time enrollmnet greater than 90%
college$full_time_percent <- (college$F.Undergrad/(college$F.Undergrad + college$P.Undergrad))
nrow(college[college$full_time_percent > 0.9, ])

college$acceptance_rate <- (college$Enroll/college$Accept)
filter <- college[college$full_time_percent > 0.9,]
summary(filter$acceptance_rate)
filter_2 <- college[college$full_time_percent < 0.9,]
summary(filter_2$acceptance_rate)

#ggplot
college$perc.alumni <- as.factor(college$perc.alumni)
college$Grad.Rate <- as.factor(college$Grad.Rate)
head(college)
install.packages("ggplot")
library(ggplot)
# Basic scatter plot
ggplot(mtcars, aes(x=perc.alumni, y=Grad.Rate)) + geom_point() + geom_smooth(method=lm)





