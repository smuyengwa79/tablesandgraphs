library(haven)
library(janitor)
library(tidyverse)
library(hrbrthemes)
library(sjPlot)
library(foreign)
library(ggpubr)


ep_data <- read.spss("data/epworth_data.sav", use.value.labels=TRUE, to.data.frame=TRUE) %>% 
  clean_names()


ggplot(ep3, aes(x= Wealthquintile,  group=state)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Weath Quintiles") +
  labs(x = "", title = "Wealth Quintiles: Musokotwane/Nyawa Chiefdom") 

ggplot(ep3, aes(sex_hhh, number_households_sharing, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Number of households sharing rooms by \n sex of household head") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "none")

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



# Households that got regular treatment
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
 
### Start here
# Other sources of income
sjp.grpfrq(ep_data$other_sourcefrequency_job_other_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Other sources and frequency of income") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Challenges generating enough income
sjp.grpfrq(ep_data$challenges_generating_enough_income, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Challenges generating enough income") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Source of bond notes
sjp.grpfrq(ep_data$bond_notes_access, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Source of bond notes") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Source of USD notes
sjp.grpfrq(ep_data$usd_cash_access, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Source of USD cash") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")


# Mobile money
sjp.grpfrq(ep_data$loading_mobile_money_methods, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Methods of loading mobile money") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Number of days buying food
sjp.grpfrq(ep_data$number_days_buying_food_from_shops, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Number of days buying food") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Reason four times
sjp.grpfrq(ep_data$reason_buying_more_4times, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Why they bought more than 4 times") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")


# Other Reason four times
sjp.grpfrq(ep_data$other_reason_buying_more_4times, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Why they bought more than 4 times (other)") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

# Cereals
p1 <- sjp.grpfrq(ep_data$cerealscereals_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of cereals") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

cereals <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = cerealscereals_cash_expenditure , 
         `Credit` = cerealscereals_credit_expenditure, 
         `Non-purchased` = cerealscereals_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(cereals, aes(key, value, fill = sex_hhh)) + 
   geom_boxplot() + 
   theme_ipsum() + 
   ggtitle("Types of expenditures on cereals") + 
   labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
   theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)


# Tubers
p1 <- sjp.grpfrq(ep_data$tuberstubers_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of tubers") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

tubers <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = tuberstubers_cash_expenditure , 
         `Credit` = tuberstubers_credit_expenditure, 
         `Non-purchased` = tuberstubers_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(tubers, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on tubers") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

# Legumes
p1 <- sjp.grpfrq(ep_data$legumeslegumes_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of legumes") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

legumes <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = legumeslegumes_cash_expenditure , 
         `Credit` = legumeslegumes_credit_expenditure, 
         `Non-purchased` = legumeslegumes_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(legumes, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on legumes") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

# Vegetables
p1 <- sjp.grpfrq(ep_data$vegetablesvegetables_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of vegetables") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

vegetables <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = vegetablesvegetables_cash_expenditure , 
         `Credit` = vegetablesvegetables_credit_expenditure, 
         `Non-purchased` = vegetablesvegetables_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(vegetables, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on vegetables") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)


# fruits
p1 <- sjp.grpfrq(ep_data$fruitsfruits_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of fruits") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

fruits <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = fruitsfruits_cash_expenditure , 
         `Credit` = fruitsfruits_credit_expenditure, 
         `Non-purchased` = fruitsfruits_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(fruits, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on fruits") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)


# meat_fish_poultrymeat_fish_poultry
p1 <- sjp.grpfrq(ep_data$meat_fish_poultrymeat_fish_poultry_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of meat, fish, poultry") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

meat_fish_poultrymeat_fish_poultry <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = meat_fish_poultrymeat_fish_poultry_cash_expenditure , 
         `Credit` = meat_fish_poultrymeat_fish_poultry_credit_expenditure, 
         `Non-purchased` = meat_fish_poultrymeat_fish_poultry_estimated_value_non_purchase) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(meat_fish_poultrymeat_fish_poultry, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on meat, fish and poultry") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure


# milk_milk_productsmilk_milk_products
p1 <- sjp.grpfrq(ep_data$milk_milk_productsmilk_milk_products_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of milk products") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

milk_products <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = milk_milk_productsmilk_milk_products_cash_expenditure , 
         `Credit` = milk_milk_productsmilk_milk_products_credit_expenditure, 
         `Non-purchased` = milk_milk_productsmilk_milk_products_estimated_value_non_purcha) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(milk_products, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on milk products") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)


# Oil and butter products
p1 <- sjp.grpfrq(ep_data$oil_butter_fatoil_butter_fat_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of oil products") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

oil_products <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = oil_butter_fatoil_butter_fat_cash_expenditure , 
         `Credit` = oil_butter_fatoil_butter_fat_credit_expenditure, 
         `Non-purchased` = oil_butter_fatoil_butter_fat_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(oil_products, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on oil products") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure


# Sugars and sweets
p1 <- sjp.grpfrq(ep_data$sugar_sweetssugar_sweets_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of sugars and sweets") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

sugar_products <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = sugar_sweetssugar_sweets_cash_expenditure , 
         `Credit` = sugar_sweetssugar_sweets_credit_expenditure, 
         `Non-purchased` = sugar_sweetssugar_sweets_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(sugar_products, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on sugars and sweets") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure


# Tea and coffee
p1 <- sjp.grpfrq(ep_data$tea_coffee_salttea_coffee_salt_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of teas and coffees") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

tea_products <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = tea_coffee_salttea_coffee_salt_cash_expenditure , 
         `Credit` = tea_coffee_salttea_coffee_salt_credit_expenditure, 
         `Non-purchased` = tea_coffee_salttea_coffee_salt_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(tea_products, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on teas and coffees") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure


# Condiments
p1 <- sjp.grpfrq(ep_data$condimentscondiments_main_food_source, ep_data$sex_hhh) + 
  theme_ipsum() + 
  ggtitle("Main source of condiments") + 
  labs( y = "Number of respondents", x = "") + 
  theme(legend.position = "bottom") +
  labs(fill = "Sex of Household head")

con_products <- ep_data %>%  
  select(sex_hhh, 
         `Cash` = condimentscondiments_cash_expenditure, 
         `Credit` = condimentscondiments_credit_expenditure, 
         `Non-purchased` = condimentscondiments_estimated_value_non_purchased) %>% 
  gather("key", "value", - sex_hhh)
p2 <-  ggplot(con_products, aes(key, value, fill = sex_hhh)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Types of expenditures on condiments") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
p3 <- last_plot() + coord_flip()
figure <- ggarrange(p1, p3,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure

### Monthly expenditures
m_exp <- ep_data %>% 
  select(sex_hhh, starts_with("monthly_expenditure"))
names(m_exp) <- gsub("monthly_expenditure", "", names(m_exp))
names(m_exp) <- gsub("_expenditure", "", names(m_exp))
names(m_exp) <- gsub("_", " ", names(m_exp))
m_exp <- m_exp %>% 
  gather("key", "value", -1)

ggplot(m_exp, aes(key, value, fill = `sex hhh`)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Monthly cash and credit expenses") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
last_plot() + coord_flip()

### Quarterly expenditures 
q_exp <- ep_data %>% 
  select(sex_hhh, starts_with("quarterly_expenditure"))
names(q_exp) <- gsub("quarterly_expenditure", "", names(q_exp))
names(q_exp) <- gsub("_expenditure", "", names(q_exp))
names(q_exp) <- gsub("_", " ", names(q_exp))
q_exp <- q_exp %>% 
  gather("key", "value", -1)

ggplot(q_exp, aes(key, value, fill = `sex hhh`)) + 
  geom_boxplot() + 
  theme_ipsum() + 
  ggtitle("Quarterly cash and credit expenses") + 
  labs(y = "Amount in RTGS", x = "", fill = "Sex") + 
  theme(legend.position = "bottom")
last_plot() + coord_flip()

