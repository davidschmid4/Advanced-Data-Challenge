
install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, data.table, tydir, formattable, kableExtra, magrittr)


population <- read.csv("population_by_country_2020.csv", sep=",", header=TRUE)
population <- population[, -c(3,4,7,8,11)]

colnames(population) <- c("country", "Population", "Density", "Land Area", "Med. Age", "Urban Pop.")
population$country <- as.character(population$country)

population$country[population$country == "United States"] <- "US"
population$country[population$country == "Czech Republic (Czechia)"] <- "Czechia"

# exclude countries with less than 100k population
population <- population %>%
  dplyr::filter(Population > 100000)

# extract countries that are in both data.frames
equalCountries <- intersect(population$country, cases$country)

# wrong! might already be obsolete https://www.stats.indiana.edu/vitals/CalculatingARate.pdf
# population$Population_10_k <- population$Population / 10000
# population$Population_100_k <- population$Population / 100000
# population$Population_1_m <- population$Population / 1000000


# replace the countrie names where I summed up over the provinces with ..._All
# dependent on countriesWithProv in cases.file
for(i in countriesWithProv) {
  name <- paste(i, "_All", sep="")
  for(j in population$country) {
    if(i == j) {
      population$country[population$country == j] <- name
    }
  }
}

write.csv(population, "C:\\Users\\s1015\\Desktop\\Data Challenge\\Data\\Model_DATA\\population_prepared.csv", row.names = FALSE)




