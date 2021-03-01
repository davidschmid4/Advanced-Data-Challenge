install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, magrittr)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)



# Number of cases worldwide
cases <- read.csv(file = 'covid_19_clean_complete.csv', header=TRUE, sep=",")
colnames(cases)[2] <- "country"
class(cases$Date)
cases$Date <- as.Date(cases$Date, format = "%Y-%m-%d")
cases$Province.State <- as.character(cases$Province.State)
cases$country <- as.character(cases$country)
cases <- dplyr::select(cases, -c(10))
summary(cases)



## 
##
# For countries with attached Provinces sum up over all provinces for every Date
##
##

countriesWithProv <- cases %>%
  dplyr::filter(Province.State != "")
countriesWithProv <- unique(countriesWithProv$country) # [1] "Australia" [1] "Canada"[1] "China" [1] "Denmark" [1] "France" [1] "Netherlands" [1] "United Kingdom"

sumCountries <- data.frame()

for(i in countriesWithProv) {
  country <- dplyr::filter(cases, country %in% c(i))
  countrySum <- data.frame()
  name <- paste(i, "_All", sep="")
  datelist <- as.list(country$Date)
  datelist <- unique(datelist)
  for(j in datelist) {
    perDate <- dplyr::filter(country, Date == j)
    summedUp <- data.frame(Province.State = "" , country = name, Lat = NA, Long = NA, Date = j, Confirmed = NA, Deaths = NA, Recovered = NA, Active = NA )
    summedUp[1, 6:8] = colSums(perDate[6:9], na.rm=TRUE)
    countrySum <- rbind(countrySum, summedUp)
  }
  sumCountries <- rbind(sumCountries, countrySum)
}

cases <- rbind(cases, sumCountries)


# remove the province rows
for(i in countriesWithProv) {
  print(i)
  cases <- subset(cases, country != i)
}

# get all the countries available in both data.frames
equalCountries <- intersect(population$country, cases$country) 

###
##
#EXTRACT all first Dates of 1 or more cases per country per 10k, 100k & 1m population
###
###


firstDateOver1per10k <- data.frame()
firstDateOver1per100k <- data.frame()
firstDateOver1per1m <- data.frame()


for(i in equalCountries) {
  country_it <- dplyr::filter(cases, country %in% c(i))
  populationCountry <- dplyr::filter(population, country %in% c(i))
  country_it$Confirmed_10_k <- (country_it$Confirmed / populationCountry$Population)*10000
  country_it$Confirmed_100_k <- (country_it$Confirmed / populationCountry$Population)*100000
  country_it$Confirmed_1m <- (country_it$Confirmed / populationCountry$Population)*1000000
  

  firstDateOver_10k <- country_it %>%
    dplyr::filter(Confirmed_10_k > 1) %>%
    dplyr::filter(Confirmed_10_k == min(Confirmed_10_k))
  firstDateOver_10k <- dplyr::select(firstDateOver_10k, -c(3,4,6,7,8,9,10,11,12))

  firstDateOver_100k <- country_it %>%
    dplyr::filter(Confirmed_100_k > 1) %>%
    dplyr::filter(Confirmed_100_k == min(Confirmed_100_k))
  firstDateOver_100k <- dplyr::select(firstDateOver_100k, -c(3,4,6,7,8,9,10,11,12))

  firstDateOver_1m <- country_it %>%
    dplyr::filter(Confirmed_1m > 1) %>%
    dplyr::filter(Confirmed_1m == min(Confirmed_1m))
  firstDateOver_1m <- dplyr::select(firstDateOver_1m, -c(3,4,6,7,8,9,10,11,12))

  # in some small countries the death rate per 10k population was the same for multiple consecutive days
  if(nrow(firstDateOver_10k) > 1) {
    firstDateOver_10k <- dplyr::filter(firstDateOver_10k, Date == min(Date))
  }
  if(nrow(firstDateOver_100k) > 1) {
    firstDateOver_100k <- dplyr::filter(firstDateOver_100k, Date == min(Date))
  }
  if(nrow(firstDateOver_1m) > 1) {
    firstDateOver_1m <- dplyr::filter(firstDateOver_1m, Date == min(Date))
  }

  firstDateOver1per10k <- rbind(firstDateOver1per10k, firstDateOver_10k)
  firstDateOver1per100k <- rbind(firstDateOver1per100k, firstDateOver_100k)
  firstDateOver1per1m <- rbind(firstDateOver1per1m, firstDateOver_1m)
}


names(firstDateOver1per10k)[3] <- "Date_Over_1"
names(firstDateOver1per100k)[3] <- "Date_Over_1"
names(firstDateOver1per1m)[3] <- "Date_Over_1"


##
##
# Calculate the 30th day date after First Date over 1 cases/death per ... population
##
##

firstDateOver1per10k$Date_30 <- firstDateOver1per10k$Date_Over_1 + 30
firstDateOver1per100k$Date_30 <- firstDateOver1per100k$Date_Over_1 + 30
firstDateOver1per1m$Date_30 <- firstDateOver1per1m$Date_Over_1 + 30

# Drop the Province column from these data.frames

firstDateOver1per10k <- dplyr::select(firstDateOver1per10k, -c(1))
firstDateOver1per100k <- dplyr::select(firstDateOver1per100k, -c(1))
firstDateOver1per1m <- dplyr::select(firstDateOver1per1m, -c(1))

##
##
#10 k
##
##

# Get the case and death number for Date_over_1 and Date_30 for every country for every 10k per population
firstDateOver1per10k$cases_1 <- NA
firstDateOver1per10k$cases_2 <- NA

for(i in 1:nrow(firstDateOver1per10k)) {
  country_ <- firstDateOver1per10k[i, 1]
  Date_1 <- firstDateOver1per10k[i, 2]
  Date_2 <- firstDateOver1per10k[i, 3]
  
  countryData1 <- dplyr::filter(cases, country == country_ &  Date == Date_1)
  confirmed1 <- countryData1[1, 6]
  firstDateOver1per10k[i, 4] <- confirmed1
  
  countryData2 <- dplyr::filter(cases, country == country_ &  Date == Date_2)
  confirmed_2 <- countryData2[1, 6]
  firstDateOver1per10k[i, 5] <- confirmed_2
}

# Get the case and death number for Date_over_1 and Date_30 for every country for every 100k per population
firstDateOver1per100k$cases_1 <- NA
firstDateOver1per100k$cases_2 <- NA

for(i in 1:nrow(firstDateOver1per100k)) {
  country_ <- firstDateOver1per100k[i, 1]
  Date_1 <- firstDateOver1per100k[i, 2]
  Date_2 <- firstDateOver1per100k[i, 3]
  
  countryData1 <- dplyr::filter(cases, country == country_ &  Date == Date_1)
  confirmed1 <- countryData1[1, 6]
  firstDateOver1per100k[i, 4] <- confirmed1
  
  countryData2 <- dplyr::filter(cases, country == country_ &  Date == Date_2)
  confirmed_2 <- countryData2[1, 6]
  firstDateOver1per100k[i, 5] <- confirmed_2
}

# Get the case and death number for Date_over_1 and Date_30 for every country for every 1m per population
firstDateOver1per1m$cases_1 <- NA
firstDateOver1per1m$cases_2 <- NA

for(i in 1:nrow(firstDateOver1per1m)) {
  country_ <- firstDateOver1per1m[i, 1]
  Date_1 <- firstDateOver1per1m[i, 2]
  Date_2 <- firstDateOver1per1m[i, 3]
  
  countryData1 <- dplyr::filter(cases, country == country_ &  Date == Date_1)
  confirmed1 <- countryData1[1, 6]
  firstDateOver1per1m[i, 4] <- confirmed1
  
  countryData2 <- dplyr::filter(cases, country == country_ &  Date == Date_2)
  confirmed_2 <- countryData2[1, 6]
  firstDateOver1per1m[i, 5] <- confirmed_2
}


# Calculate the differences between the case numbers of these dates if available (10k)

firstDateOver1per10k$diff <- firstDateOver1per10k$cases_2 - firstDateOver1per10k$cases_1

firstDateOver1per10k$population <- NA
for(i in firstDateOver1per10k$country) {
  x <- dplyr::filter(population, country == i) 
  firstDateOver1per10k$population[firstDateOver1per10k$country == i] <- x$Population
}

firstDateOver1per10k$diff_standardised <- firstDateOver1per10k$diff / firstDateOver1per10k$population
firstDateOver1per10k$diff_standardised_log <- round(log(firstDateOver1per10k$diff_standardised),2)

qplot(firstDateOver1per10k$diff_standardised, seq_along(firstDateOver1per10k$diff_standardised))
qplot(firstDateOver1per10k$diff_standardised_log, seq_along(firstDateOver1per10k$diff_standardised_log))


# try first correaltion of standardised difference for 10k per pop and big Five personality country means

diff_available <- dplyr::filter(firstDateOver1per10k, is.na(diff_standardised_log) == FALSE)
big_five_available <- dplyr::filter(bigFiveCountryMeans, country %in% diff_available$country)
testCorr <- merge(diff_available, big_five_available, by = "country")

cor(diff_available$diff_standardised_log, big_five_available$agreeable_score)


plot(testCorr$diff_standardised_log, testCorr$extraversion_score, main="Scatterplot Example",
     xlab="diff", ylab="neuro", pch=19)
abline(lm(testCorr$diff_standardised_log~testCorr$extraversion_score), col="red") # regression line (y~x)
lines(lowess(testCorr$diff_standardised_log,testCorr$extraversion_score), col="blue") # lowess line (x,y)

##
##
# 100 k
##
##


# Calculate the differences between the case numbers of these dates if available (100k)

firstDateOver1per100k$diff <- firstDateOver1per100k$cases_2 - firstDateOver1per100k$cases_1

firstDateOver1per100k$population <- NA
for(i in firstDateOver1per100k$country) {
  x <- dplyr::filter(population, country == i) 
  firstDateOver1per100k$population[firstDateOver1per100k$country == i] <- x$Population
}

firstDateOver1per100k$diff_standardised <- firstDateOver1per100k$diff / firstDateOver1per100k$population
firstDateOver1per100k$diff_standardised_log <- log(firstDateOver1per100k$diff_standardised, 2)

# try first correaltion of standardised log difference for 100k per pop and big Five personality country means

diff_available <- dplyr::filter(firstDateOver1per100k, is.na(diff_standardised_log) == FALSE)
big_five_available <- dplyr::filter(bigFiveCountryMeans, country %in% diff_available$country)
testCorr <- merge(diff_available, big_five_available, by = "country")

cor(diff_available$diff_standardised_log, big_five_available$agreeable_score)

plot(testCorr$diff_standardised_log, testCorr$openness_score, main="Scatterplot Example",
     xlab="diff", ylab="neuro", pch=19)
abline(lm(testCorr$diff_standardised_log~testCorr$openness_score), col="red") # regression line (y~x)
lines(lowess(testCorr$diff_standardised_log,testCorr$openness_score), col="blue") # lowess line (x,y)





##
##
# 1 m
##
##


# Calculate the differences between the case numbers of these dates if available (1m)

firstDateOver1per1m$diff <- firstDateOver1per1m$cases_2 - firstDateOver1per1m$cases_1

firstDateOver1per1m$population <- NA
for(i in firstDateOver1per1m$country) {
  x <- dplyr::filter(population, country == i) 
  firstDateOver1per1m$population[firstDateOver1per1m$country == i] <- x$Population
}

firstDateOver1per1m$diff_standardised <- firstDateOver1per1m$diff / firstDateOver1per1m$population
firstDateOver1per1m$diff_standardised_log <- log(firstDateOver1per1m$diff_standardised, 2)

# try first correaltion of standardised log difference for 1m per pop and big Five personality country means

diff_available <- dplyr::filter(firstDateOver1per1m, is.na(diff_standardised_log) == FALSE)
big_five_available <- dplyr::filter(bigFiveCountryMeans, country %in% diff_available$country)
testCorr <- merge(diff_available, big_five_available, by = "country")

cor(diff_available$diff_standardised_log, big_five_available$agreeable_score)

plot(testCorr$diff_standardised_log, testCorr$agreeable_score, main="Scatterplot Example",
     xlab="diff", ylab="neuro", pch=19)
abline(lm(testCorr$diff_standardised_log~testCorr$agreeable_score), col="red") # regression line (y~x)
lines(lowess(testCorr$diff_standardised_log,testCorr$agreeable_score), col="blue") # lowess line (x,y)


# change the column names of all 'firstDateover1per....' so that they can be merged
firstDateOver1per10k <- subset(firstDateOver1per10k, select=-c(population))
colnames(firstDateOver1per10k) <- c("country", "Date1_10k", "Date30_10k", "cases1_10k", "cases30_10k", 
                                 "diff_10k", "diff_stand_10k", "diff_stand_log_10k")

firstDateOver1per100k <- subset(firstDateOver1per100k, select=-c(population))
colnames(firstDateOver1per100k) <- c("country", "Date1_100k", "Date30_100k", "cases1_100k", "cases30_100k", 
                                    "diff_100k", "diff_stand_100k", "diff_stand_log_100k")

firstDateOver1per1m <- subset(firstDateOver1per1m, select=-c(population))
colnames(firstDateOver1per1m) <- c("country", "Date1_1m", "Date30_1m", "cases1_1m", "cases30_1m", 
                                    "diff_1m", "diff_stand_1m", "diff_stand_log_1m")




