# Math 167R — Group B Project R code
# Name: Megha Sengupta

# (Optional but recommended) start clean:
rm(list = ls())

# (Optional) make printing easier to read:
options(stringsAsFactors = FALSE)
# ------------------------------------------------------------
# Analysis on how does regulation on AI influence how AI is being adopted? 
# Overtime analysis (regulation over time impact) (POLITICAL wise)
# ------------------------------------------------------------

#load packages
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#loading .csv file
data <- read.csv("Data/Global_AI_Content_Impact_Dataset.csv")
data

ggplot(data,
       aes(x = AI.Adoption.Rate...., y = Year)) + geom_point()
#Comment: we compared AI adoption rate vs year, and the scatterplot
# shows similar adoption rates throughout the years

#let's try another visualization
#This time we compare regulation status with AI adoption rate
ggplot(data,
       aes(x = Regulation.Status, y =  AI.Adoption.Rate....)) + geom_boxplot()
#Figure 1
ggplot(data,
       aes(x = AI.Adoption.Rate...., y = Regulation.Status)) + geom_boxplot()

#AI adoption rate histogram distribution
ggplot(data, aes(x = AI.Adoption.Rate....)
) + 
  geom_histogram(bins = 18,
                 fill = "pink",
                 color = "white")
#year histogram distribution
ggplot(data, aes(x = Year)
) + 
  geom_histogram(bins = 18,
                 fill = "lightblue",
                 color = "white")

#Figure 2
ggplot(
  data,
  aes(x = Regulation.Status,
      fill = Country)
) + geom_bar()
  
#barplot comparing 2 variables 
barplot(data$AI.Adoption.Rate...., names.arg = data$'Country', cex.names = 0.8
        , main= "AI adoption in different countries", xlab = "Countries", ylab = "AI adoption rate")


#Figure 3
ggplot(  data,
         aes(x = Country,
             y =  AI.Adoption.Rate....,
             fill = "pink",
             color = "white")
) + geom_col(width = 0.9)

#(figure 4)
ggplot(
  data,
  aes(x = factor(Year), y = AI.Generated.Content.Volume..TBs.per.year.)
  ) + geom_col(fill = "lightblue",width = 0.5)
         
# (Fig 5)
ggplot(
  data, 
  aes(x=factor(Year), fill = Top.AI.Tools.Used)
  )+geom_bar()+
  scale_fill_manual(values = c(
    "ChatGPT" = "purple",
    "Claude" = "lightblue",
    "Gemini" = "pink"
  ))
##I did not use all of the graphs, just some of them
