install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare)
  
  

#Indiviudalism vs. Collectivism
indivCol <- read.csv(file = '!_indiv_collectiv.csv', header=TRUE, sep=";")
cols <- c(3,4,5,6,7,8)
indivCol[,cols] = apply(indivCol[,cols], 2, function(x) as.numeric(as.character(x)));
indivCol$country <- as.character(indivCol$country)

# change names in indivCol to get an equal name notation
indivCol$country[59] = "South Korea"
indivCol$country[43] = "United Kingdom"
indivCol$country[104] = "US"
indivCol$country[31] ="Dominican Republic"
indivCol$country[29] ="Czechia"

# Number of cases worldwide
cases <- read.csv(file = 'covid_19_clean_complete.csv', header=TRUE, sep=",")
colnames(cases)[2] <- "country"
cases$Date <- as.Date(cases$Date, format = "%m/%d/%y")
cases$Province.State <- as.character(cases$Province.State)
cases$country <- as.character(cases$country)
summary(cases)
class(cases$Date)


###################


# extract certain countries cases: "Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"
exploreCases <- dplyr::filter(cases, country %in% c("Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"), Date == as.Date("2020-04-12"))
# exclude exotic islands of the United Kingdom
exploreCases <- dplyr::filter(exploreCases, Province.State == "" )

# i should include canada but i will then have to sum the cases etc over all provinces first

# extract indiv vs. collectiv dimension value for "Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"
exploreIndiv <- select(indivCol, country, idv)
exploreIndiv <- dplyr::filter(exploreIndiv, country %in% c("Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"))

# merge cases and the indiv/col dimension on country
exploreCasesIndiv <- merge(exploreCases, exploreIndiv, by.x = "country", by.y = "country")

# test correlations
cor(exploreCasesIndiv$Confirmed, exploreCasesIndiv$idv)
cor(exploreCasesIndiv$Recovered, exploreCasesIndiv$idv)

germancases <-dplyr::filter(cases, Date == as.Date("2020-04-12"))


comp <- compare(germancase, indivCol, )
print(germancases)

diff <- setdiff(indivCol$country, germancases$country)
print(diff)

# extract chinese cases to sum up over all provinces (!only for 12.04!)

chineseCases <- dplyr::filter(cases, country %in% c("China"), Date == as.Date("2020-04-12"))
chineseCases <- dplyr::select(chineseCases, -c(1,3,4))

chineseCases[34] = c(country = "China_All", Date = as.Date("2020-04-12"), Confirmed = NA, Deaths = NA, Recovered = NA )

# Sum up over all provinces of China

chinaAllCases <- data.frame(country = "China_All", Date = as.Date("2020-04-12"), Confirmed = NA, Deaths = NA, Recovered = NA )
chinaAllCases[1, 3:5] = colSums(chineseCases[3:5], na.rm=TRUE)











