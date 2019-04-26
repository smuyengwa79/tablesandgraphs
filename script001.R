library(haven)
library(janitor)
library(tidyverse)
library(hrbrthemes)

epworth_data <- read_sav("data/epworth_data.sav") %>% 
  clean_names()

write.csv(epworth_data, file = "ep3.csv")
ep3 <- read.csv("ep3.csv")

# Sex
ggplot(ep3, aes(sex_hhh, fill = sex_hhh)) + 
  geom_bar() + 
  theme_ipsum()+ 
  ggtitle("Sex of respondents") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "none")
# Age of HH by sex

ggplot(ep3, aes(age_hhh, fill = sex_hhh)) + 
  geom_histogram(alpha = 0.3) + 
  theme_ipsum()+ 
  ggtitle("Age of respondents by gender") + 
  labs( y = "No. of respondents", x = "Age in years", fill = "Sex")  

# Marital Status
ggplot(ep3, aes(marital_status_hh, fill = marital_status_hh)) + 
  geom_bar() + 
  theme_ipsum()+ 
  ggtitle("Marital status of respondents") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "none")


# Households sharing
ggplot(ep3, aes(number_households_sharing, fill = sex_hhh)) + 
  geom_bar(position = "dodge") + 
  theme_ipsum()+ 
  ggtitle("Number of households sharing") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom")

#  Structure of the household
ggplot(ep3, aes(age_respondent, hh_sizefemalefemales_0_5years)) + 
  geom_point()

