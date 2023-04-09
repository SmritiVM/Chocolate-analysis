#Questions
#Selecting top 10 companies
freq_Origin = as.data.frame(table(chocolate$company))
colnames(freq_Origin) = c("Company", "Frequency")

# The top 10 chocolate bar producers
top10s = dplyr::arrange(freq_Origin, desc(Frequency))[1:10,]
onlytops = dplyr::filter(chocolate, company %in% top10s[,1])

View(onlytops)

#1. Find most common first taste and compare rating with other tastes

# find most common first taste
common_taste = names(which.max(table(chocolate$first_taste)))

# find the avg ratings
average_rating = 0
average_count = nrow(chocolate["rating"])
common_taste_rating = 0
common_taste_rating_count = 0

taste = chocolate["first_taste"]
rating = chocolate["rating"]

for (index in 1:nrow(chocolate["rating"])) {
  if (taste[[1]][[index]] == common_taste) {
    common_taste_rating = common_taste_rating + rating[[1]][[index]]
    common_taste_rating_count = common_taste_rating_count + 1
  }
  average_rating = average_rating + rating[[1]][[index]]
}

message("Average rating of chocolates is: ", average_rating / average_count)
message("Rating for chocolates with most popular taste is: ", common_taste_rating / common_taste_rating_count)

#2. Multiple regression (model rating in terms of cocoa percent and count of ingredients)
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

#3. Hypothesis testing (take top 10 companies producing chocolates as a sample and see if it's representative of the entire population) mean of the rating
sample_mean=0
sample_size=0
temp = data.frame(sort(table(chocolate$company), decreasing=TRUE)[1:10])
temp

soma = data.frame(chocolate[chocolate$company=='Soma' , ])
tempmean=mean(soma$rating)
n=nrow(soma)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

arete = data.frame(chocolate[chocolate$company=='Arete' , ])
tempmean=mean(arete$rating)
n=nrow(arete)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

fresco = data.frame(chocolate[chocolate$company=='Fresco' , ])
tempmean=mean(fresco$rating)
n=nrow(fresco)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

bonnat = data.frame(chocolate[chocolate$company=='Bonnat' , ])
tempmean=mean(bonnat$rating)
n=nrow(bonnat)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

pralus = data.frame(chocolate[chocolate$company=='Pralus' , ])
tempmean=mean(pralus$rating)
n=nrow(pralus)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

amorin = data.frame(chocolate[chocolate$company=='A. Morin' , ])
tempmean=mean(amorin$rating)
n=nrow(amorin)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

domori = data.frame(chocolate[chocolate$company=='Domori' , ])
tempmean=mean(domori$rating)
n=nrow(domori)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

guittard = data.frame(chocolate[chocolate$company=='Guittard' , ])
tempmean=mean(guittard$rating)
n=nrow(guittard)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

valrhona = data.frame(chocolate[chocolate$company=='Valrhona' , ])
tempmean=mean(valrhona$rating)
n=nrow(valrhona)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

zotter = data.frame(chocolate[chocolate$company=='Zotter' , ])
tempmean=mean(zotter$rating)
n=nrow(zotter)
sample_size=sample_size+n
sample_mean=sample_mean+tempmean

sample_mean=sample_mean/10
population_mean=mean(chocolate$rating)
sigma=sd(chocolate$rating)

z=(sample_mean-population_mean)/(sigma/sqrt(sample_size))

alpha=0.05
zhalfalpha=qnorm(1-(alpha/2))
c(-zhalfalpha,zhalfalpha)
pval=2*pnorm(z)

if(pval>alpha){print("Accept Null hypothesis")} else{print("Reject Null hypothesis")}


#4. Hypothesis testing (take 2 countries, compare the average reviews) - check of one country produces objectively better chocolates 
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

