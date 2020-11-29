install.packages("reshape2")
install.packages("ggcorrplot")

library(MASS)
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2)
library(ggcorrplot)
library(plotly)

#reading the dataset

airline <- read.csv("airline-safety.csv")
head(airline)
summary(airline)


#data manipulations

#renaming the columns
colnames(airline)[3] <- "incidents in 1985-1999"
colnames(airline)[4] <- "fatal_accidents in 1985-1999"
colnames(airline)[5] <- "fatalities in 1985-1999"
colnames(airline)[6] <- "incidents in 2000-2014"
colnames(airline)[7] <- "fatal_accidents in 2000-2014"
colnames(airline)[8] <- "fatalities in 2000-2014"

#creating the safety column to identify airlines based on the fatalities count in two time periods
airline$safety <- ifelse((airline$`fatalities in 1985-1999` > airline$`fatalities in 2000-2014`),"Improved",
                         (ifelse((airline$`fatalities in 1985-1999` < airline$`fatalities in 2000-2014`),"Deteriorated","Constant")))

#creating a new dataframe safety by arranging the airline dataframe
safety <- airline %>% group_by(safety) %>% select(airline,`fatalities in 1985-1999`,`fatalities in 2000-2014`) %>% arrange(safety)

#creating 3 new dataframes based on the value of  safety for each airline
improved_flights <- safety %>% filter(safety == "Improved")
deteriorated_flights <- safety %>% filter(safety == "Deteriorated")
constant_flights <- safety %>% filter(safety == "Constant")

#Plots


#1)Correlation Map for the fatal_accidents, fatalities and total no of incidents
air <- read.csv("airline-safety.csv")

#Creating data in a suitable format so as to obtain a correlation matrix for the required parameters
#gathering data
air <- air %>% 
  gather(incidents_85_99:fatalities_00_14, key = "type", value = "count")

#replacing underscore in gathered column(type) with .
air$type <- str_replace(air$type, "_(?=[0-9])", "\\.")

#seperating type and the time period into 2 different columns
air <- air %>% 
  separate(type, c("type", "time_span"), sep = "\\.")

#replacing time period values with correct values
air <- air %>% 
  mutate(time_span = fct_recode(time_span,
                                "1985 - 1999" = "85_99",
                                "2000 - 2014" = "00_14"))
#spreading the dataset again to obtain the final cleaned data
air <- air %>% 
  spread(type, count)

airs <- air[,c(4:6)]
head(airs)

corair <- round(cor(airs),2)
#final correlation matrix
corair

#1) corrplot of correlation matrix 
ggcorrplot(corair, lab = TRUE) + ggtitle("Correlation plot of the data")      



#2) Filtering all the airlines based on fatal accidents count > 0 in the time period 1985-1999
fatal_accidents_old <- airline %>% select(airline,`fatal_accidents in 1985-1999`,`fatalities in 1985-1999`,safety) %>%
  filter(`fatal_accidents in 1985-1999`>0)

#plotting each airline with the total fatalities count for each safety level in the time period 1995-1999
ggplot(fatal_accidents_old,aes(x=airline,y=`fatalities in 1985-1999`,colour = safety)) + 
  geom_point() + 
  coord_flip() + facet_wrap(~safety) + ggtitle("Airline Fatalities in 1985-1999 for different safety groups")

#scatter plot of fatal accidents and their corresponding fatalities in the period 1985-1999
ggplot(fatal_accidents_old,aes(`fatal_accidents in 1985-1999`,`fatalities in 1985-1999`,colour = safety)) + 
  geom_point(size=3) + geom_jitter(width =3) + ggtitle("Fatal accidents vs total no. of fatalities") 



#3) Filtering all the airlines based on fatal accidents count > 0 in the time period 2000-2014
fatal_accidents_new <- airline %>% select(airline,`fatal_accidents in 2000-2014`,`fatalities in 2000-2014`,safety) %>%
  filter(`fatal_accidents in 2000-2014`>0) 

#plotting each airline with the total fatalities count for each safety level in the time period 2000-2004
ggplot(fatal_accidents_new,aes(x=airline,y=`fatalities in 2000-2014`,colour = safety)) + 
  geom_point() + 
  coord_flip() + facet_wrap(~safety) + ggtitle("Airline Fatalities in 2000-2014 for different safety groups")

#scatter plot of fatal accidents and their corresponding fatalities in the period 2000-2004
ggplot(fatal_accidents_new,aes(`fatal_accidents in 2000-2014`,`fatalities in 2000-2014`,colour = safety)) + 
  geom_point(size=3) + geom_jitter(width =3) + ggtitle("Fatal accidents vs total no. of fatalities")



#4) A barchart of airline and fatalities based on safety for period 1985-1999
plot1 <- plot_ly(
  data = fatal_accidents_old,
  x = ~airline, 
  y = ~`fatalities in 1985-1999`,
  color = ~safety
)
plot1 <- plot1 %>% layout(title = "Airline and Total Fatalities in 1985-1999",                
                          xaxis = list(title = "Airline"),
                          yaxis = list(title = "Fatalities Count"))
plot1

#5) A barchart of airline and fatalities based on safety for period 2000-2014
plot2 <- plot_ly(
  data = fatal_accidents_new,
  x = ~airline, 
  y = ~`fatalities in 2000-2014`,
  color = ~safety
)
plot2 <- plot2 %>% layout(title = "Airline and Total Fatalities in 2000-2014",                
                          xaxis = list(title = "Airline"),
                          yaxis = list(title = "Fatalities Count"))
plot2

#6) creating a new data by keeping airlines which have fatalitis count > 0 in both the time periods
data <- airline %>% filter((`fatalities in 1985-1999`>0 & `fatalities in 2000-2014`>0))

#comparing the fatalities count of both the time periods to obtain information about the safety status of airline
fig <- plot_ly(data = data, x = ~airline) 
fig <- fig %>% add_trace(y = ~`fatalities in 1985-1999`, type = 'scatter', name = 'fatalities in 1985-99', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~`fatalities in 2000-2014`, type = 'scatter', name = 'fatalities in 2000-14', mode = 'markers')
fig <- fig %>% layout(title = "Comparison of Fatalities for both time period",                
                       xaxis = list(title = "Airline"),
                       yaxis = list(title = "Fatalities Count"))
fig


