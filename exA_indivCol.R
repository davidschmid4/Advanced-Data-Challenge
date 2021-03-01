install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, formattable)


#Indiviudalism vs. Collectivism
indivCol <- read.csv(file = '!_indiv_collectiv.csv', header=TRUE, sep=";")
cols <- c(3,4,5,6,7,8)
indivCol[,cols] = apply(indivCol[,cols], 2, function(x) as.numeric(as.character(x)));
indivCol$country <- as.character(indivCol$country)

print(unique(indivCol$country))

# change names in indivCol to get an equal name notation
indivCol$country[indivCol$country == "Korea South"] = "South Korea"
indivCol$country[indivCol$country == "Great Britain"] = "United Kingdom_All"
indivCol$country[indivCol$country == "U.S.A."] = "US"
indivCol$country[indivCol$country == "Dominican Rep"] ="Dominican Republic"
indivCol$country[indivCol$country == "Czech Rep"] ="Czechia"
indivCol$country[indivCol$country == "China"] ="China_All"
indivCol$country[indivCol$country == "Australia"] ="Australia_All"
indivCol$country[indivCol$country == "Canada"] ="Canada_All"
indivCol$country[indivCol$country == "Denmark"] ="Denmark_All"
indivCol$country[indivCol$country == "France"] ="France_All"
indivCol$country[indivCol$country == "Netherlands"] ="Netherlands_All"

del <- c("Belgium French", "Belgium Netherl", "Africa East", "Africa West", "Arab countries", "Canada French", 
                  "Germany East", "Switzerland French", "Switzerland German", "South Africa white")
indivCol <- indivCol %>%
  dplyr::filter(!country %in% del)
  
indivCol<- indivCol[,-c(1)]




