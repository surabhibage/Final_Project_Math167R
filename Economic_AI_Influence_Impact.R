# -----------------------------------------------------------------------------------
# Final Project: AI-Generated Content and Revenue Growth
#
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
data <- read.csv("~/Downloads/Global_AI_Content_Impact_Dataset 2.csv")

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

# Aaverage revenue increase by year and industry group
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