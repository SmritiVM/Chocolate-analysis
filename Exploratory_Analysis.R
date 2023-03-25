##Chocolate Analysis
library(readr) #to read the csv file
chocolate <- read_csv("chocolate.csv")
View(chocolate)

#View column names
column_names = colnames(chocolate)
column_names

library(dplyr)
