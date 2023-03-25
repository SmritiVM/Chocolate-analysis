##Chocolate Analysis
library(readr) #to read the csv file
chocolate <- read_csv("data/chocolate.csv")
View(chocolate)

#View column names
column_names = colnames(chocolate)
column_names

#Removing columns not being used
library(dplyr)
chocolate <- select(chocolate, -c(...1,ref,fourth_taste))

chocolate <- data.frame(chocolate)

#Modifying the columns
#Changing have/have not to 1/0

chocolate = data.frame(chocolate)
colnames(chocolate)[9] <- "bean"
colnames(chocolate)[11] <- "vanila"

#Columns to be changed: 9 to 15
bool_columns = colnames(chocolate[9:15])
bool_columns

<<<<<<< HEAD
chocolate$beans
for (x in 1:7){
  have_not <- paste("have_not_", bool_columns[x], sep = "")
  have <- paste("have_", bool_columns[x], sep = "")
  
  print(have_not)
  #print(x)
  #print(chocolate[x + 8])
  chocolate[x + 8] <- replace(chocolate[x + 8], c(have_not, have), c(0,1))
  
  #chocolate[column] <- replace(chocolate[column], have, 1)
  
}
=======
for (column in bool_columns){
  have_not <- paste("have_not_", column, sep = "")
  chocolate[column] = replace(chocolate[column], chocolate[column] == have_not, 0);

  have <- paste("have_", column, sep = "")
  chocolate[column] <- replace(chocolate[column], chocolate[column] == have, 1)
}
>>>>>>> 3623f9ba576790eff0c6337e909d8f2033a59fd6
