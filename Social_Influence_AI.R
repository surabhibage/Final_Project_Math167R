#_______________________________________________________________________________________________
# Name: Surabhi Bage
# Question being answered: Social Influence (job loss and human AI collaboration)
# How does AI being used affect job loss and human AI collaboration across different industries?
#_______________________________________________________________________________________________

install.packages("ggplot2")  # Ran once
library(ggplot2)             

data <- read.csv("/Users/surabhibage/Downloads/Global_AI_Content_Impact_Dataset.csv")
head(data)
str(data)
names(data)

# Average Job Loss due per industry sorted in descending order 
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
industry_loss_max <- aggregate(
  Job.Loss.Due.to.AI.... ~ Industry,
  data = data,
  FUN = max
)
industry_loss_max

# Boxplot showing distribution of Job Loss per industry 
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
industry_year_table <- aggregate(
  Job.Loss.Due.to.AI.... ~ Industry + Year,
  data = data,
  FUN = mean
)
industry_year_table

# Average AI related job loss trend per year for each industry 
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
