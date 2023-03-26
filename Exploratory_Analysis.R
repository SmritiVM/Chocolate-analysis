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

library(ggplot2)

#Selecting top 10 companies
freq_Origin = as.data.frame(table(chocolate$company))
colnames(freq_Origin) = c("Company", "Frequency")

# The top 10 chocolate bar producers
top10s = dplyr::arrange(freq_Origin, desc(Frequency))[1:10,]
onlytops = dplyr::filter(chocolate, company %in% top10s[,1]) 

#First plot
#1. Bar plot of the top 10 producers considering the number of ingredients used



#Second plot
#2. Scatter plot of changes in the ratings of chocolate bars (2006-2020)

# Import required libraries
ggplot(onlytops, aes(x = review_date, y = rating, color = company)) + 
  geom_point()  + facet_wrap(~ company, nrow = 2) + geom_line() +theme_bw() +
  scale_x_continuous(breaks = seq(2005,2021,2), guide = guide_axis(angle = 90))


#Third plot
#3. Violin plot of the distribution of ratings
ggplot(onlytops, aes(x = company, y = rating)) + geom_violin() +
  geom_boxplot(width=0.1, fill="white") +theme_bw()
