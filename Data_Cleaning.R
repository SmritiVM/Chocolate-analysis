##Chocolate Analysis
library(readr) #to read the csv file
chocolate <- read_csv("data/raw_chocolate.csv")
View(chocolate)

#View column names
column_names = colnames(chocolate)
column_names

#Removing columns not being used
library(dplyr)
chocolate <- select(chocolate, -c(...1,ref,fourth_taste,specific_bean_origin_or_bar_name))

chocolate <- data.frame(chocolate)

#Modifying the columns
#Changing have/have not to 1/0

chocolate = data.frame(chocolate)
colnames(chocolate)[8] <- "bean"
colnames(chocolate)[10] <- "vanila"

#Columns to be changed: 9 to 15
bool_columns = colnames(chocolate[8:14])
bool_columns


for (column in bool_columns){
  have_not <- paste("have_not_", column, sep = "")
  chocolate[column] = replace(chocolate[column], chocolate[column] == have_not, 0);

  have <- paste("have_", column, sep = "")
  chocolate[column] <- replace(chocolate[column], chocolate[column] == have, 1)
}

colnames(chocolate)[10] <- "vanilla"

#Writing the cleaned data into a new file
write.csv(chocolate, "data/chocolate.csv")
