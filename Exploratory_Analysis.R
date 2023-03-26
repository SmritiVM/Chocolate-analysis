#Creating data frame
library(readr)
chocolate <- read_csv("data/chocolate.csv")
View(chocolate)

chocolate <- data.frame(chocolate)

#Finding class of each column
library(tibble)
column_names = colnames(chocolate)
column_types = sapply(chocolate, class)
column_statistics = as_tibble(data.frame(column_names, column_types))
View(column_statistics)

summary(chocolate)

library(dplyr)
summary(select(chocolate, c(review_date, cocoa_percent, rating, counts_of_ingredients)))

#First plot
#1. Bar plot of the top 10 producers considering the number of ingredients used



#Second plot
#2. Scatter plot of changes in the ratings of chocolate bars (2006-2020)


#Third plot
#3. Violin plot of the distribution of ratings