
install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, data.table, tydir, formattable, kableExtra, magrittr)
library(dplyr)
library(formattable)
library(ggplot2)
library(kableExtra)
library(magrittr)

population_prepared <- read.csv("C:\\Users\\s1015\\Desktop\\Data Challenge\\Data\\Model_Data\\population_prepared.csv", sep=",", header=TRUE)
population_prepared$country <- as.character(population_prepared$country)


# try first correaltion of standardised log difference for 1m per pop and population density

diff_available <- dplyr::filter(firstDateOver1per1m, is.na(diff_standardised_log) == FALSE)
big_five_available <- dplyr::filter(population, country %in% diff_available$country)
testCorr <- merge(diff_available, big_five_available, by = "country")


plot(testCorr$diff_standardised_log, testCorr$`Urban Pop.`, main="Scatterplot Example",
     xlab="diff", ylab="neuro", pch=19)
abline(lm(testCorr$diff_standardised_log~testCorr$`Urban Pop.`), col="red") # regression line (y~x)
lines(lowess(testCorr$diff_standardised_log,testCorr$`Urban Pop.`), col="blue") # lowess line (x,y)



















