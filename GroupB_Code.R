# -----------------------------------------------------------------------------------
# Final Project: AI-Generated Content and Revenue Growth
# TO START: Load the dataset and libraries 
data <- read.csv("/Users/surabhibage/Downloads/Global_AI_Content_Impact_Dataset.csv")
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# Julia 
# Research Question:
# How does AI-generated content influence revenue growth across industries over time?
#
# Main Outcome Variable:
# - Revenue Increase Due to AI (%)
#
# Main Explanatory Variables:
# - AI-Generated Content Volume (TBs per year)
# - Human-AI Collaboration Rate (%)
# - Industry
# - Year
#
# -----------------------------------------------------------------------------------

# Load package
library(ggplot2)

# Read dataset
# data <- read.csv("~/Downloads/Global_AI_Content_Impact_Dataset 2.csv")

# View dataset structure
names(data)
head(data)
summary(data)

# -----------------------------------------------------------------------------------
# 1. Overall Revenue Trend Over Time
# -----------------------------------------------------------------------------------

# Average revenue increase by year
avg_year <- aggregate(
  Revenue.Increase.Due.to.AI.... ~ Year,
  data,
  mean
)

# Average revenue increase over time plot
ggplot(avg_year,
       aes(x = Year,
           y = Revenue.Increase.Due.to.AI....)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Revenue Increase Due to AI Over Time",
    x = "Year",
    y = "Average Revenue Increase (%)"
  )

# -----------------------------------------------------------------------------------
# 2. Revenue Differences Across Individual Industries
# -----------------------------------------------------------------------------------

# Average revenue increase by industry
avg_industry <- aggregate(
  Revenue.Increase.Due.to.AI.... ~ Industry,
  data,
  mean
)

# Average revenue increase by industry plot
ggplot(avg_industry,
       aes(x = reorder(Industry, Revenue.Increase.Due.to.AI....),
           y = Revenue.Increase.Due.to.AI....)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Revenue Increase Due to AI by Industry",
    x = "Industry",
    y = "Average Revenue Increase (%)"
  )

# Average revenue increase by year and industry
avg_year_industry <- aggregate(
  Revenue.Increase.Due.to.AI.... ~ Year + Industry,
  data,
  mean
)

# Revenue trends over time by individual industry plot
ggplot(avg_year_industry,
       aes(x = Year,
           y = Revenue.Increase.Due.to.AI....,
           color = Industry)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Revenue Increase by Industry Over Time",
    x = "Year",
    y = "Average Revenue Increase (%)"
  )

# -----------------------------------------------------------------------------------
# 3. Create Industry Groups
# -----------------------------------------------------------------------------------

# Group industries by similar AI usage context
data$Industry.Group <- ifelse(
  data$Industry %in% c("Gaming", "Media", "Marketing", "Retail"),
  "Consumer-Facing Digital Industries",
  ifelse(
    data$Industry %in% c("Finance", "Legal", "Education"),
    "Knowledge-Intensive Service Industries",
    "Operational & Technical Industries"
  )
)

# Check number of records in each group
table(data$Industry.Group)

# -----------------------------------------------------------------------------------
# 4. Revenue Differences Across Industry Groups
# -----------------------------------------------------------------------------------

# Average revenue increase by industry group
avg_group <- aggregate(
  Revenue.Increase.Due.to.AI.... ~ Industry.Group,
  data,
  mean
)

# Average revenue increase by industry group plot
ggplot(avg_group,
       aes(x = reorder(Industry.Group, Revenue.Increase.Due.to.AI....),
           y = Revenue.Increase.Due.to.AI....,
           fill = Industry.Group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Revenue Increase by Industry Group",
    x = "Industry Group",
    y = "Average Revenue Increase (%)"
  )

# Average revenue increase by year and industry group
avg_year_group <- aggregate(
  Revenue.Increase.Due.to.AI.... ~ Year + Industry.Group,
  data,
  mean
)

# Revenue trends over time by industry group plot
ggplot(avg_year_group,
       aes(x = Year,
           y = Revenue.Increase.Due.to.AI....,
           color = Industry.Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Revenue Increase Over Time by Industry Group",
    x = "Year",
    y = "Average Revenue Increase (%)"
  )

# -----------------------------------------------------------------------------------
# 5. AI-Generated Content Volume and Revenue Increase
# -----------------------------------------------------------------------------------

# Relationship between AI-generated content volume and revenue increase by group
ggplot(data,
       aes(x = AI.Generated.Content.Volume..TBs.per.year.,
           y = Revenue.Increase.Due.to.AI....,
           color = Industry.Group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AI Content Volume vs Revenue Increase by Industry Group",
    x = "AI-Generated Content Volume (TBs/year)",
    y = "Revenue Increase (%)"
  )

# -----------------------------------------------------------------------------------
# 6. AI Tool Usage and Revenue Increase
# -----------------------------------------------------------------------------------

# Average revenue increase by AI tool
avg_tool <- aggregate(
  Revenue.Increase.Due.to.AI.... ~ Top.AI.Tools.Used,
  data,
  mean
)

# Sort tools from highest to lowest average revenue increase
avg_tool <- avg_tool[order(-avg_tool$Revenue.Increase.Due.to.AI....), ]

# View average revenue increase by tool
print(avg_tool)

# Average revenue increase by AI tool plot
ggplot(avg_tool,
       aes(x = reorder(Top.AI.Tools.Used, Revenue.Increase.Due.to.AI....),
           y = Revenue.Increase.Due.to.AI....)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Revenue Increase by AI Tool",
    x = "AI Tool",
    y = "Average Revenue Increase (%)"
  )

#_______________________________________________________________________________________________
# Name: Surabhi Bage
# Question being answered: Social Influence (job loss and human AI collaboration)
# How does AI being used affect job loss and human AI collaboration across different industries?
# Fields being used: Human AI Collaboration, Job Loss %
#_______________________________________________________________________________________________


# Package installation and data reading, exploration 
#_______________________________________________________________________________________________

#install.packages("ggplot2")  # Ran once
#library(ggplot2)             

# data <- read.csv("~/Downloads/Global_AI_Content_Impact_Dataset.csv")
head(data)
str(data)
names(data)


# Average Job Loss due per industry sorted in descending order 
#_______________________________________________________________________________________________
industry_loss <- aggregate(
  `Job.Loss.Due.to.AI....` ~ Industry, # job loss grouped by industry 
  data = data,
  FUN = mean # mean function for aggregation 
)
industry_loss <- industry_loss[ # sorting by job loss in descending order 
  order(-industry_loss$Job.Loss.Due.to.AI....),
]
industry_loss # final result 


# Finding the industries with the highest job loss percentage (may leave out of final analysis)
#_______________________________________________________________________________________________
industry_loss_max <- aggregate(
  Job.Loss.Due.to.AI.... ~ Industry,
  data = data,
  FUN = max
)
industry_loss_max


# Boxplot showing distribution of Job Loss per industry 
#_______________________________________________________________________________________________
ggplot(data,
       aes(
         x = reorder( # instead of directly taking the column, we reorder in descending order
           Industry,
           Job.Loss.Due.to.AI....,
           FUN = mean # reorder based on the job loss mean 
         ),
         y = Job.Loss.Due.to.AI....,
         fill = Industry)
) +
  geom_boxplot() + 
  labs( # rename the x and y axis to meaningful labels 
    x = "Industry Sector",
    y = "Job Loss Due to AI (%)",
    title = "AI-Related Job Loss Across Industries"
  )


# A table of the job loss of each industry for each year 
#_______________________________________________________________________________________________
industry_year_table <- aggregate(
  Job.Loss.Due.to.AI.... ~ Industry + Year,
  data = data,
  FUN = mean
)
industry_year_table


# Average AI related job loss trend per year for each industry 
#_______________________________________________________________________________________________
industry_year <- aggregate(
  Job.Loss.Due.to.AI.... ~ Industry + Year,
  data = data,
  FUN = mean
)
ggplot(industry_year,
       aes(
         x = Year,
         y = Job.Loss.Due.to.AI....,
         color = Industry,
         group = Industry
       )
) +
  
  geom_line() +
  
  #geom_point() +
  
  labs(
    title = "Average AI-Related Job Loss by Industry Over Time",
    x = "Year",
    y = "Average Job Loss Due to AI (%)"
  ) 


# Does higher human AI collaboration reduce job loss? 
#_______________________________________________________________________________________________
# This line shows the data is likely synthetic or randomly generated 
ggplot(data,
       aes(
         x = Human.AI.Collaboration.Rate....,
         y = Job.Loss.Due.to.AI....
       )
) +
  geom_point() +
  labs(
    title = "Human-AI Collaboration vs Job Loss",
    x = "Human-AI Collaboration Rate (%)",
    y = "Job Loss Due to AI (%)"
  ) 


# Which industried have the highest human-AI collaboration? 
#_______________________________________________________________________________________________
industry_collab <- aggregate( # group the Human AI Collaboration by Industry
  Human.AI.Collaboration.Rate.... ~ Industry,
  data = data,
  FUN = mean
)
ggplot(industry_collab,
       aes(
         x = reorder(
           Industry,
           Human.AI.Collaboration.Rate....
         ),
         
         y = Human.AI.Collaboration.Rate....,
         fill = Industry
       )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Average Human-AI Collaboration by Industry",
    x = "Industry",
    y = "Human-AI Collaboration Rate (%)"
  )


# Collaboration over time? 
#_______________________________________________________________________________________________
collab_year <- aggregate(
  Human.AI.Collaboration.Rate.... ~ Industry + Year,
  data = data,
  FUN = mean
)
ggplot(collab_year,
       aes(
         x = Year,
         y = Human.AI.Collaboration.Rate....,
         color = Industry,
         group = Industry
       )
) +
  geom_line() +
  labs(
    title = "Human-AI Collaboration Trends by Industry",
    x = "Year",
    y = "Average Human-AI Collaboration (%)"
  ) 
#_______________________________________________________________________________________________


# Math 167R — Group B Project R code
# Name: Megha Sengupta

# (Optional but recommended) start clean:
# rm(list = ls())

# (Optional) make printing easier to read:
options(stringsAsFactors = FALSE)
# ------------------------------------------------------------
# Analysis on how does regulation on AI influence how AI is being adopted? 
# Overtime analysis (regulation over time impact) (POLITICAL wise)
# ------------------------------------------------------------

#load packages
install.packages("tidyverse")
library(tidyverse)
#library(ggplot2)

#loading .csv file
# data <- read.csv("Data/Global_AI_Content_Impact_Dataset.csv")
# data

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
