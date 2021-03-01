install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, formattable)


# try first correaltion of standardised log difference for 1m per pop and big Five personality country means

diff_available <- dplyr::filter(firstDateOver1per1m, is.na(diff_standardised_log) == FALSE)
big_five_available <- dplyr::filter(indivCol, country %in% diff_available$country)
testCorr <- merge(diff_available, big_five_available, by = "country")

plot(testCorr$diff_standardised_log, testCorr$ivr, main="Scatterplot Example",
     xlab="diff", ylab="neuro", pch=19)
abline(lm(testCorr$diff_standardised_log~testCorr$ivr), col="red") # regression line (y~x)
lines(lowess(testCorr$diff_standardised_log,testCorr$ivr), col="blue") # lowess line (x,y)









