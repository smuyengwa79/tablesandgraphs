library(haven)
library(janitor)
library(tidyverse)
library(hrbrthemes)
library(sjPlot)
library(foreign)


ep_data <- read.spss("data/epworth_data.sav", use.value.labels=TRUE, to.data.frame=TRUE) %>% 
  clean_names()

# Respondent position 
sjp.grpfrq(ep_data$respondent_position, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Position of respondent in the household") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Number migrated 

sjp.grpfrq(ep_data$migrated_household_members, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Percentage of households that migrated \n by sex") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Place of origin 

sjp.grpfrq(ep_data$place_origin_migration, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Place of origin by sex") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")



sjp.grpfrq(ep_data$informal_settlement_origin_migration, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Informal settlement ") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Main reason for migration
sjp.grpfrq(ep_data$main_reason_migration, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main reasons for migration ") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Children not attending school 
sjp.grpfrq(ep_data$children_not_attending_school, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Children not attending school ") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")


# Children not attending school (Period)
sjp.grpfrq(ep_data$duration_abscent_from_school, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Children not attending school (Period) ") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Physical disabilities /chronic illness
sjp.grpfrq(ep_data$disabled_chonic_hh_members, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("People living with disability) ") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Households thatr got regular treatment
sjp.grpfrq(ep_data$number_disability_chronic_treatment, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Household members treated for chronic diseases") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")



# Households other chronic diseases
sjp.grpfrq(ep_data$other_chronic_diseaes, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Household members treated for other chronic diseases") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")



# Households thatr got regular treatment
sjp.grpfrq(ep_data$number_hh_members_treated_non_chronic_diseases, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Household members treated for non-chronic diseases") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")



# Unsafe places for girls
sjp.grpfrq(ep_data$unsafe_environment_girls, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Unsafe places for girls and women") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")


# Sources of insecurity
ggplot(ep_data, aes(sources_insecurity,sex_hhh)) +
  geom_col()+
  theme_ipsum() + 
  ggtitle("Sources of insecurity for girls and women") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")
