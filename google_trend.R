install.packages("reshape2")
install.packages("ggcorrplot")
install.packages("caret")

library(MASS)
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2)
library(ggcorrplot)
library(plotly)
library(caret)
library(dplyr)

#read the dataset
gt <- read.csv("google_trend.csv")

#splitting the dataset into precovid and postcovid data
precovid <- gt %>% filter(Month < "2020-03")
postcovid <- gt %>% filter(Month >= "2020-03")

#creating new dataframe by splitting the Month Column into seperate Year and Month 
gtnew <- separate(data = gt, col = Month, into = c("Year", "Month"), sep = "\\-")

#Summarising the data based on sum of each topic grouped on the basis of year and creating new dataframes
airlines <- gtnew %>% group_by(Year) %>% summarise(`Airlines(Yearly)` = sum(airlines))
netflix <- gtnew %>% group_by(Year) %>% summarise(`netflix(Yearly)` = sum(netflix))
Recipe <- gtnew %>% group_by(Year) %>% summarise(`Recipe(Yearly)` = sum(Recipe))
Restaurant <- gtnew %>% group_by(Year) %>% summarise(`Restaurant(Yearly)` = sum(Restaurant))
Amazon <- gtnew %>% group_by(Year) %>% summarise(`Amazon(Yearly)` = sum(Amazon))

#Merging all the dataframe into a single dataframe i.e. Overall
Overall <- right_join(airlines,netflix)
Overall <- right_join(Overall,Recipe)
Overall <- right_join(Overall,Restaurant)
Overall <- right_join(Overall,Amazon) 

#1) Plotting overall yearly trend of the 5 search topics
yearlyplot <- plot_ly(data = Overall, x = ~Year, name = "Yearly Trend")
yearlyplot <- yearlyplot %>% add_trace(y = ~`Airlines(Yearly)`,type = 'scatter', name = 'Airlines', mode = 'lines+markers')
yearlyplot <- yearlyplot %>% add_trace(y = ~`netflix(Yearly)`,type = 'scatter', name = 'netflix', mode = 'lines+markers')
yearlyplot <- yearlyplot %>% add_trace(y = ~`Recipe(Yearly)`,type = 'scatter', name = 'Recipe', mode = 'lines+markers')
yearlyplot <- yearlyplot %>% add_trace(y = ~`Restaurant(Yearly)`,type = 'scatter', name = 'Restaurant', mode = 'lines+markers')
yearlyplot <- yearlyplot %>% add_trace(y = ~`Amazon(Yearly)`,type = 'scatter', name = 'Amazon', mode = 'lines+markers')
yearlyplot <- yearlyplot %>% layout(title = "Yearly Trend of 5 Search Topics",                
                                     xaxis = list(title = "Year"),
                                     yaxis = list(title = "Search Topics"))
yearlyplot

#2) Plotting the precovid monthly trend of 5 search topics
pre <- plot_ly(data = precovid, x = ~Month, name = "Precovid Trend")
pre <- pre %>% add_trace(y = ~airlines,type = 'scatter', name = 'airlines', mode = 'lines')
pre <- pre %>% add_trace(y = ~netflix,type = 'scatter', name = 'netflix', mode = 'lines')
pre <- pre %>% add_trace(y = ~Restaurant,type = 'scatter', name = 'Restaurant', mode = 'lines')
pre <- pre %>% add_trace(y = ~Amazon,type = 'scatter', name = 'Amazon', mode = 'lines')
pre <- pre %>% layout(title = "Precovid Trend of 5 Search Topics",                
                      xaxis = list(title = "Month"),
                      yaxis = list(title = "Search Topics"))
pre


#3) Plotting the postcovid monthly trend of 5 search topics
post <- plot_ly(data = postcovid, x = ~Month, name = "Postcovid Trend")
post <- post %>% add_trace(y = ~airlines,type = 'scatter', name = 'airlines', mode = 'lines')
post <- post %>% add_trace(y = ~netflix,type = 'scatter', name = 'netflix', mode = 'lines')
post <- post %>% add_trace(y = ~Recipe,type = 'scatter', name = 'Recipe', mode = 'lines')
post <- post %>% add_trace(y = ~Restaurant,type = 'scatter', name = 'Restaurant', mode = 'lines')
post <- post %>% add_trace(y = ~Amazon,type = 'scatter', name = 'Amazon', mode = 'lines')
post <- post %>% layout(title = "Postcovid Trend of 5 Search Topics",                
                      xaxis = list(title = "Month"),
                      yaxis = list(title = "Search Topics"))
post


#4) Creating a plot of Monthly values of each of the 5 topics based on the pre and post covid condition
#plot depicts the increase or decrease in the values of each of the 5 topics

new <- gtnew
new$status <- ifelse(new$Year >= 2020 & new$Month >= "03" ,"Post Covid", "Pre Covid")

#indicates that value of airlines went down post covid period 
ggplot(new,aes(Month,airlines,colour=status)) + geom_point() + facet_wrap(~status) + 
  ggtitle("Comparison of Airlines on Monthly basis in both post and pre covid period")

#indicates that value of netflix went up drastically post covid period
ggplot(new,aes(Month,netflix,colour=status)) + geom_point() + facet_wrap(~status) + 
  ggtitle("Comparison of Netflix on Monthly basis in both post and pre covid period")

#indicates that value of Recipe went up at a slower pace post covid period
ggplot(new,aes(Month,Recipe,colour=status)) + geom_point() + facet_wrap(~status) +
  ggtitle("Comparison of Recipe on Monthly basis in both post and pre covid period")

#indicates that value of Restaurant went down slowly post covid period
ggplot(new,aes(Month,Restaurant,colour=status)) + geom_point() + facet_wrap(~status) +
  ggtitle("Comparison of Restaurant on Monthly basis in both post and pre covid period")

#indicates that value of Amazon went up drastically post covid period
ggplot(new,aes(Month,Amazon,colour=status)) + geom_point() + facet_wrap(~status) + 
  ggtitle("Comparison of Amazon on Monthly basis in both post and pre covid period")


