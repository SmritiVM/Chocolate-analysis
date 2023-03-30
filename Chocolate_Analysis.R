#Questions
#Selecting top 10 companies
freq_Origin = as.data.frame(table(chocolate$company))
colnames(freq_Origin) = c("Company", "Frequency")

# The top 10 chocolate bar producers
top10s = dplyr::arrange(freq_Origin, desc(Frequency))[1:10,]
onlytops = dplyr::filter(chocolate, company %in% top10s[,1])

View(onlytops)

#1. Multiple regression (model rating in terms of cocoa percent and count of ingredients)
library(dplyr)
Rating_data <- select(onlytops, c(company, rating, cocoa_percent, counts_of_ingredients))
Rating_data = Rating_data %>% group_by(company)  %>%
  summarise(avg_rating = mean(rating),
            avg_cocoa_percent = mean(cocoa_percent),
            avg_ingredient_count = round(mean(counts_of_ingredients)),
            .groups = 'drop')

RegModel = lm(Rating_data$avg_rating ~ Rating_data$avg_cocoa_percent + Rating_data$avg_ingredient_count)
RegModel

summary(RegModel)

library(scatterplot3d)
scatterplot3d(Rating_data$avg_rating ~ Rating_data$avg_cocoa_percent + Rating_data$avg_ingredient_count)

#2. Hypothesis testing (take top 10 companies producing chocolates as a sample and see if it's representative of the entire population) mean of the rating



#3. Hypothesis testing (take 2 countries, compare the average reviews) - check of one country produces objectively better chocolates 




#4. Multiple correlation (salt, sugar, cocoa_butter, vanilla) 