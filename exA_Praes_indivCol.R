install.packages("pacman")
pacman::p_load(dplyr, tidyverse, ggplot2, compare, formattable)
library(formattable)  


#Indiviudalism vs. Collectivism
indivCol <- read.csv(file = '!_indiv_collectiv.csv', header=TRUE, sep=";")
cols <- c(3,4,5,6,7,8)
indivCol[,cols] = apply(indivCol[,cols], 2, function(x) as.numeric(as.character(x)));
indivCol$country <- as.character(indivCol$country)

print(unique(indivCol$country))

# change names in indivCol to get an equal name notation
indivCol$country[59] = "South Korea"
indivCol$country[43] = "United Kingdom"
indivCol$country[104] = "US"
indivCol$country[31] ="Dominican Republic"
indivCol$country[29] ="Czechia"

###################


# extract certain countries cases: "Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"
exploreCases <- dplyr::filter(cases, country %in% c("Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"), Date > as.Date("2020-04-01"))
# exclude exotic islands of the United Kingdom
exploreCases <- dplyr::filter(exploreCases, Province.State == "" )

# i should include canada but i will then have to sum the cases etc over all provinces first

# extract indiv vs. collectiv dimension value for "Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"
exploreIndiv <- dplyr::select(indivCol, country, idv)
exploreIndiv <- dplyr::filter(exploreIndiv, country %in% c("Austria", "Germany", "Italy", "South Korea", "US", "United Kingdom"))

# merge cases and the indiv/col dimension on country
exploreCasesIndiv <- merge(exploreCases, exploreIndiv, by.x = "country", by.y = "country")

# get top and lowest 5 for every dimension

top5pdi <- indivCol[with(indivCol, order(-pdi)),]
top5pdi <- top5pdi[1:5,]
top5pdi <- top5pdi[,-c(1,4:8)]
row.names(top5pdi) <- NULL

formattable(top5pdi, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

low5pdi <- indivCol[with(indivCol, order(pdi)),]
low5pdi <- low5pdi[1:5,]
low5pdi <- low5pdi[,-c(1,4:8)]
row.names(low5pdi) <- NULL

formattable(low5pdi, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

top5uai <- indivCol[with(indivCol, order(-uai)),]
top5uai <- top5uai[1:5,]
top5uai <- top5uai[,-c(1,3:5, 7, 8)]
row.names(top5uai) <- NULL

formattable(top5uai, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

low5uai <- indivCol[with(indivCol, order(uai)),]
low5uai <- low5uai[1:5,]
low5uai <- low5uai[,-c(1,3:5, 7, 8)]
row.names(low5uai) <- NULL

formattable(low5uai, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

top5idv <- indivCol[with(indivCol, order(-idv)),]
top5idv <- top5idv[1:5,]
top5idv <- top5idv[,-c(1,3,5:8)]
row.names(top5idv) <- NULL

formattable(top5idv, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

low5idv <- indivCol[with(indivCol, order(idv)),]
low5idv <- low5idv[1:5,]
low5idv <- low5idv[,-c(1,3,5:8)]
row.names(low5idv) <- NULL

formattable(low5idv, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

top5mas <- indivCol[with(indivCol, order(-mas)),]
top5mas <- top5mas[1:5,]
top5mas <- top5mas[,-c(1,3,4,6:8)]
row.names(top5mas) <- NULL

formattable(top5mas, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

low5mas <- indivCol[with(indivCol, order(mas)),]
low5mas <- low5mas[1:5,]
low5mas <- low5mas[,-c(1,3,4,6:8)]
row.names(low5mas) <- NULL

formattable(low5mas, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

top5ltowvs <- indivCol[with(indivCol, order(-ltowvs)),]
top5ltowvs <- top5ltowvs[1:5,]
top5ltowvs <- top5ltowvs[,-c(1,3:6,8)]
row.names(top5ltowvs) <- NULL

formattable(top5ltowvs, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

low5ltowvs <- indivCol[with(indivCol, order(ltowvs)),]
low5ltowvs <- low5ltowvs[1:5,]
low5ltowvs <- low5ltowvs[,-c(1,3:6,8)]
row.names(low5ltowvs) <- NULL

formattable(low5ltowvs, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

top5ivr <- indivCol[with(indivCol, order(-ivr)),]
top5ivr <- top5ivr[1:5,]
top5ivr <- top5ivr[,-c(1,3:7)]
row.names(top5ivr) <- NULL

formattable(top5ivr, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))

low5ivr <- indivCol[with(indivCol, order(ivr)),]
low5ivr <- low5ivr[1:5,]
low5ivr <- low5ivr[,-c(1,3:7)]
row.names(low5ivr) <- NULL

formattable(low5ivr, align =c("l","c"), 
            list(`country` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
            ))








