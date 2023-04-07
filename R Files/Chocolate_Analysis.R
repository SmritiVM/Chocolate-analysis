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
scatterplot3d(Rating_data$avg_rating ~ Rating_data$avg_cocoa_percent + Rating_data$avg_ingredient_count,
              xlab = "Cocoa percent",
              ylab = "Ingredient Count",
              zlab = "Rating")

#2. Hypothesis testing (take top 10 companies producing chocolates as a sample and see if it's representative of the entire population) mean of the rating



#3. Hypothesis testing (take 2 countries, compare the average reviews) - check of one country produces objectively better chocolates 
ratingmeans=aggregate(onlytops$rating, list(onlytops$company), FUN=mean)
ratingmeans
P1=(ratingmeans$Group.1[1])
m1=ratingmeans$x[1]
P2=(ratingmeans$Group.1[2])
m2=ratingmeans$x[2]
n1=sum(onlytops$company==P1)
n2=sum(onlytops$company==P2)
n2
ratingsd=aggregate(onlytops$rating, list(onlytops$company), FUN=sd)
sd1=ratingsd$x[1]
sd2=ratingsd$x[2]

#t test for 2 means
#H0 : Both companies have the same quality/popularity as the other
#H1 : The companies are not on the same level of quality/popularity
# testing at 5% level of significance
t= (m1-m2)/sqrt(((sd1*sd1)/n1)+((sd2*sd2)/n2))
t
cv=qt(0.975,(n1+n2-2))
cv
if(cv <=t){print("Accept Ho")} else{print("Reject Ho")}



#4. Multiple correlation (salt, sugar, cocoa_butter, vanilla) 
