# Set working dir
setwd("C:/users/99707/Desktop/Assignment/R_Assignment02")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#3
Keeling_Data <- read.csv("ROCKWELL_CITY.csv",header=T)
ROCKWELL_CITY_P_value <- as_tibble(Keeling_Data)
# Check the class
class(ROCKWELL_CITY_P_value)
# Check the variables
head(ROCKWELL_CITY_P_value)
# plot the time series
ROCKWELL_CITY_P_value                      %>%
  select(DATE,PRCP)                        %>%
  ggplot(aes(x=as.Date(DATE), y=PRCP)) +  
  geom_line()     
