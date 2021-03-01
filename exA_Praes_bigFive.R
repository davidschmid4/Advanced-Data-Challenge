
install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, data.table, tydir, formattable, kableExtra, magrittr)
library(dplyr)
library(formattable)
library(ggplot2)
library(kableExtra)
library(magrittr)

# set custom color variables

customGreen = "#DeF7E9"

customGreen0 = "#71CA97"

customRed = "#ff7f7f"

# import big five dataset

bigFive <- read.csv(file = 'big_five_scores.csv', header=TRUE, sep=",")
print(unique(bigFive$country)) #236 unique countries

aus <- dplyr::filter(bigFive, bigFive$country == "Austria")

# 172 ids had no countries attached so I excluded them

bigFive <- subset(bigFive, bigFive$country != "")

print(count(bigFive$case_id))

#  count the number of appearances of all countries
countryCount <- as.data.frame(table(bigFive$country))[2:236,]

# Calculate Means and add the count column to the dataset
bigFiveCountryMeans <- aggregate(bigFive[,5:9], list(bigFive$country), mean)

bigFiveCountryMeans <- merge(bigFiveCountryMeans, countryCount, by.x="Group.1", by.y="Var1")

names(bigFiveCountryMeans)[1] <- "country"

# top 10 countries with most participants

countriesMostParticipants <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(-Freq)),]
countriesMostParticipants <- countriesMostParticipants[,-c(2:6)]
extraCountries <- filter(countriesMostParticipants, country %in% 
                           c("Italy", "South Korea", "Spain", "Japan", "Turkey", "France", "China", "Germany", "Russia"))
countriesMostParticipants <- countriesMostParticipants[1:10,]

row.names(countriesMostParticipants) <- NULL

formattable(countriesMostParticipants, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

row.names(extraCountries) <- NULL

formattable(extraCountries, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))
f <- as.data.table(formattable(extraCountries, align =c("l","c","c"), 
                               list(`country` = formatter(
                                 "span", style = ~ style(color = "grey",font.weight = "bold"))
                               )))
print(kable(f))
# Exploratory analysis into the number of appearances of countries

mean(bigFiveCountryMeans$Freq) # 1306.983  
median(bigFiveCountryMeans$Freq) # 26

# exclude countries with less than 365 participants

bigFiveCountryMeans <- dplyr::filter(bigFiveCountryMeans, Freq >= 365)
bigFiveCountryMeans$country <- as.character(bigFiveCountryMeans$country)

# correct the names of some countries
bigFiveCountryMeans$country[1] <- "Afghanistan"
bigFiveCountryMeans$country[29] <- "Cayman Islands"
bigFiveCountryMeans$country[37] <- "Czechia"
bigFiveCountryMeans$country[39] <- "Dominican Republic"
bigFiveCountryMeans$country[42] <- "El Salvador"
bigFiveCountryMeans$country[76] <- "Netherlands"
bigFiveCountryMeans$country[77] <- "New Zealand"
bigFiveCountryMeans$country[84] <- "Philippines"
bigFiveCountryMeans$country[87] <- "Puerto Rico"
bigFiveCountryMeans$country[89] <- "Russia"
bigFiveCountryMeans$country[90] <- "Saudi Arabia"
bigFiveCountryMeans$country[95] <- "South Africa"
bigFiveCountryMeans$country[96] <- "South Korea"
bigFiveCountryMeans$country[100] <- "Switzerland"
bigFiveCountryMeans$country[103] <- "Trinidad and Tobago"
bigFiveCountryMeans$country[106] <- "United Kingdom"
bigFiveCountryMeans$country[108] <- "United Arab Emirates"
bigFiveCountryMeans$country[110] <- "US"

# exclude rows where the names are not properly specified (Arabian Gu, British Vi,)
bigFiveCountryMeans <- bigFiveCountryMeans[-c(9, 25), ]

# get top 5 countries for every score
# Note: könnte mit collectivism korrelieren
top5CountriesAggr <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(-agreeable_score)),]
top5CountriesAggr <- top5CountriesAggr[1:5,]
top5CountriesAggr <- top5CountriesAggr[,-c(3:6)]

top5CountriesExtra <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(-extraversion_score)),]
top5CountriesExtra <- top5CountriesExtra[1:5,]
top5CountriesExtra <- top5CountriesExtra[,-c(2,4:6)]

top5CountriesOpen <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(-openness_score)),]
top5CountriesOpen <- top5CountriesOpen[1:5,]
top5CountriesOpen <- top5CountriesOpen[,-c(2,3,5,6)]

top5CountriesConsc <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(-conscientiousness_score)),]
top5CountriesConsc <- top5CountriesConsc[1:5,]
top5CountriesConsc <- top5CountriesConsc[,-c(2:4,6)]

top5CountriesNeuro <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(-neuroticism_score)),]
top5CountriesNeuro <- top5CountriesNeuro[1:5,]
top5CountriesNeuro <- top5CountriesNeuro[,-c(2:5)]

low5CountriesAggr <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(agreeable_score)),]
low5CountriesAggr <- low5CountriesAggr[1:5,]
low5CountriesAggr <- low5CountriesAggr[,-c(3:6)]

low5CountriesExtra <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(extraversion_score)),]
low5CountriesExtra <- low5CountriesExtra[1:5,]
low5CountriesExtra <- low5CountriesExtra[,-c(2,4:6)]

low5CountriesOpen <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(openness_score)),]
low5CountriesOpen <- low5CountriesOpen[1:5,]
low5CountriesOpen <- low5CountriesOpen[,-c(2,3,5,6)]

low5CountriesConsc <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(conscientiousness_score)),]
low5CountriesConsc <- low5CountriesConsc[1:5,]
low5CountriesConsc <- low5CountriesConsc[,-c(2:4,6)]

low5CountriesNeuro <- bigFiveCountryMeans[with(bigFiveCountryMeans, order(neuroticism_score)),]
low5CountriesNeuro <- low5CountriesNeuro[1:5,]
low5CountriesNeuro <- low5CountriesNeuro[,-c(2:5)]

# create agegroups and summarise over it 

agebreaks <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
agelabels <- c("10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")

# setDT(bigFive)[ , agegroups := cut(age, 
#                                 breaks = agebreaks, 
#                                 right = FALSE, 
#                                 labels = agelabels)]


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

formattable(bigFiveAgeMeans, align =c("l","c","c","c","c", "c"), 
            list(`agegroup` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) ,
              `agreeable_score`= color_tile(customGreen, customGreen0),
              `extraversion_score`= color_tile(customGreen, customGreen0),
              `openness_score`= color_tile(customGreen, customGreen0),
              `conscientiousness_score`= color_tile(customGreen, customGreen0),
              `neuroticism_score`= color_tile(customGreen, customGreen0)
            ))

ggplot(bigFiveAgeMeans, aes(x=agegroup, y=agreeable_score)) + geom_bar(stat="identity")

# calculate mean scores per sex and create table


bigFiveSexMeans <- bigFive %>%
  group_by(sex) %>%
  summarise_at(vars(agreeable_score, extraversion_score, openness_score, conscientiousness_score, neuroticism_score), 
               list(mean)) %>%
  mutate_if(is.numeric, round, 3)


formattable(bigFiveSexMeans, align =c("l","c","c","c","c", "c"), 
            list(`sex` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) ,
              `agreeable_score`= color_tile(customGreen, customGreen0),
              `extraversion_score`= color_tile(customGreen, customGreen0),
              `openness_score`= color_tile(customGreen, customGreen0),
              `conscientiousness_score`= color_tile(customGreen, customGreen0),
              `neuroticism_score`= color_tile(customGreen, customGreen0)
            ))


# create tables for top and low five countries in every dimension
row.names(top5CountriesAggr) <- NULL

formattable(top5CountriesAggr, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

row.names(top5CountriesExtra) <- NULL

formattable(top5CountriesExtra, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

row.names(top5CountriesOpen) <- NULL

formattable(top5CountriesOpen, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))


row.names(top5CountriesConsc) <- NULL

formattable(top5CountriesConsc, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))


row.names(top5CountriesNeuro) <- NULL

formattable(top5CountriesNeuro, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

# low

row.names(low5CountriesAggr) <- NULL

formattable(low5CountriesAggr, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

row.names(low5CountriesExtra) <- NULL

formattable(low5CountriesExtra, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

row.names(low5CountriesOpen) <- NULL

formattable(low5CountriesOpen, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))


row.names(low5CountriesConsc) <- NULL

formattable(low5CountriesConsc, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))


row.names(low5CountriesNeuro) <- NULL

formattable(low5CountriesNeuro, align =c("l","c","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

# count number of people in agegroups

agegroupCount <- as.data.frame(table(bigFive$agegroup))
print(agegroupCount)
names(agegroupCount)[1] <- "AgeGroup"

formattable(agegroupCount, align =c("c","c","c", "c","c","c", "c","c","c"), 
            list(`agegroup` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

class(agegroupCount$`AgeGroup`)

ggplot(agegroupCount, aes(x=AgeGroup, y=Freq)) + geom_bar(stat="identity", fill="lightsteelblue") + scale_y_continuous(limits = c(0,150000))

# count number of males/females per agegroup
class(bigFive$sex)

agegroupSexCount1 <- dplyr::filter(bigFive, as.numeric(bigFive$sex)==1)
agegroupSexCount1 <- as.data.frame(table(agegroupSexCount1$agegroup, agegroupSexCount1$sex))
names(agegroupSexCount1) <- c("Age Group", "Sex(M)", "Freq")

agegroupSexCount2 <- dplyr::filter(bigFive, as.numeric(bigFive$sex)==2)
agegroupSexCount2 <- as.data.frame(table(agegroupSexCount2$agegroup, agegroupSexCount2$sex))
names(agegroupSexCount2) <- c("Age Group", "Sex(F)", "Freq")

formattable(agegroupSexCount1, align =c("l", "c", "c"), 
            list(`Age Group` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))


formattable(agegroupSexCount2, align =c("l", "c", "c"), 
            list(`Age Group` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))





