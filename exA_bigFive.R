install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, data.table, tydir, formattable, kableExtra, magrittr)



bigFive <- read.csv(file = 'big_five_scores.csv', header=TRUE, sep=",")

# 172 ids had no countries attached so I excluded them

bigFive <- subset(bigFive, bigFive$country != "")

# correct the names of some countries
bigFive$country <- as.character(bigFive$country)


bigFive$country[bigFive$country == "Afghanista"] <- "Afghanistan"
bigFive$country[bigFive$country == "Cayman Isl"] <- "Cayman Islands"
bigFive$country[bigFive$country == "Czech Repu"] <- "Czechia"
bigFive$country[bigFive$country == "Dominican"] <- "Dominican Republic"
bigFive$country[bigFive$country == "El Salvado"] <- "El Salvador"
bigFive$country[bigFive$country == "New Zealan"] <- "New Zealand"
bigFive$country[bigFive$country == "Philippine"] <- "Philippines"
bigFive$country[bigFive$country == "Puerto Ric"] <- "Puerto Rico"
bigFive$country[bigFive$country == "Russian Fe"] <- "Russia"
bigFive$country[bigFive$country == "Saudi Arab"] <- "Saudi Arabia"
bigFive$country[bigFive$country == "South Afri"] <- "South Africa"
bigFive$country[bigFive$country == "South Kore"] <- "South Korea"
bigFive$country[bigFive$country == "Switzerlan"] <- "Switzerland"
bigFive$country[bigFive$country == "Trinidad a"] <- "Trinidad and Tobago"
bigFive$country[bigFive$country == "United Ara"] <- "United Arab Emirates"
bigFive$country[bigFive$country == "USA"] <- "US"
bigFive$country[bigFive$country == "Columbia"] <- "Colombia"
bigFive$country[bigFive$country == "UK"] <- "United Kingdom_All"
bigFive$country[bigFive$country == "China"] <- "China_All"
bigFive$country[bigFive$country == "Australia"] <- "Australia_All"
bigFive$country[bigFive$country == "France"] <- "France_All"
bigFive$country[bigFive$country == "Canada"] <- "Canada_All"
bigFive$country[bigFive$country == "Denmark"] <- "Denmark_All"
bigFive$country[bigFive$country == "Netherland"] <- "Netherlands_All"



#  count the number of appearances of all countries
countryCount <- as.data.frame(table(bigFive$country))[2:236,]
names(countryCount)[2] <- "Freq_BigFive"

# Calculate Means and add the count column to the dataset
bigFiveCountryMeans <- aggregate(bigFive[,5:9], list(bigFive$country), mean)

bigFiveCountryMeans <- merge(bigFiveCountryMeans, countryCount, by.x="Group.1", by.y="Var1")

names(bigFiveCountryMeans)[1] <- "country"

# exclude countries with less than 30 participants
bigFiveCountryMeans <- dplyr::filter(bigFiveCountryMeans, Freq >= 30)


# create agegroups and summarise over it 

agebreaks <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
agelabels <- c("10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")


bigFive <- bigFive %>%
  mutate(agegroup = case_when(age <= 20 ~ "10-20",
                              age > 20 & age <=30 ~ "20-30",
                              age > 30 & age <= 40 ~ "30-40",
                              age > 40 & age <= 50 ~ "40-50",
                              age > 50 & age <= 60 ~ "50-60",
                              age > 60 & age <= 70 ~ "60-70",
                              age > 70 & age <= 80 ~ "70-80",
                              age > 80 & age <= 90 ~ "80-90",
                              age > 90 & age <= 100 ~ "90-100"))

bigFiveAgeMeans <- bigFive %>%
  group_by(agegroup) %>%
  summarise_at(vars(agreeable_score, extraversion_score, openness_score, conscientiousness_score, neuroticism_score), 
               list(mean)) %>%
  mutate_if(is.numeric, round, 3)

bigFiveSexMeans <- bigFive %>%
  group_by(sex) %>%
  summarise_at(vars(agreeable_score, extraversion_score, openness_score, conscientiousness_score, neuroticism_score), 
               list(mean)) %>%
  mutate_if(is.numeric, round, 3)










