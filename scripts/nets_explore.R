# Laura Barraclough
# Script started: 15/06/2022
# Contact: s1729795@ed.ac.uk

## This script aims to explore the Scottish Government dataset on Salmon and Trout
## net catch numbers. This data is open access and available at:
## https://data.marine.gov.scot/dataset/salmon-and-sea-trout-fishery-statistics-1952-2021-season-reported-catch-district-and-method.

# 1. Load dataset and packages ----
setwd("C:\\Users\\lcbar\\OneDrive\\Documents\\MSc project ideas\\scottish_salmon") #set working directory to filename
#to check this we can getwd() which will tell us the working directory

# Packages
#load.package(tidyverse) #if tidyverse is not already installed on your computer
library(tidyverse)
#load.package(rsq)
library(rsq)

# The dataset I will look at here is net data from 1952-2021 for salmon and trout
net_catch <- read.csv("data/SalmonandSeaTroutNets1952-2021.csv")
# Check the dataset has loaded properly
View(net_catch)

# 2. Whole dataset graphs ----
summary(net_catch) #shows us the breakdown of the different parts of the dataset

# Try make a scatter graph of salmon Wild.MSW.Number against time with respect
# to Region

(scat1 <- ggplot(net_catch, aes(x = Year, y = Wild.MSW.Number, colour = Region)) +
    theme_bw() +
    geom_point(size = 1) +
    labs(y = " Estimated Salmon Number",
         x = "Year"))
#ggsave(scat1, file = "outputs/salmonpopchange.png", width = 5, height = 5) 

# Make a scatter graph of salmon weight over time with respect to Region

(scat2 <- ggplot(net_catch, aes(x = Year, y = Wild.MSW.Weight..kg., colour = Region)) +
    theme_bw() +
    geom_point(size = 1) +
    labs(y = " Salmon Weight (kg)",
         x = "Year"))
ggsave(scat2, file = "outputs/salmonweightchange.png", width = 5, height = 5) 

# 3. Model Salmon estimated populations in the North East Region ----  
# Make a new object with just the North East Salmon data
NE_data <- filter(net_catch, Region == "North East")
View(NE_data)

# Try modelling population change 
NE_salmon.mdl <- lm(Wild.MSW.Number ~ Year, data = NE_data)
NE_salmon.mdl #call model into console 
rsq(NE_salmon.mdl) # the rsq vale for this is 0.09011 which is pretty high
plot(NE_salmon.mdl) #analyse the model
# I think we need to correct for the sampling effort


# We can graph this:
(scat3 <- ggplot(NE_data, aes(x = Year, y = Wild.MSW.Number, colour = Region)) +
    theme_bw() +
    geom_point(size = 1) +
    labs(y = " Estimated Salmon Number",
         x = "Year") +
    stat_smooth(method = "lm", formula = y ~ x, colour = c("#1C86EE"),
                fill = c("#1C86EE")))
#ggsave(scat3, file = "outputs/salmonpopchangemdl.png", width = 5, height = 5) 
