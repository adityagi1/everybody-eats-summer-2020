library(tidyverse)
library(ggplot2)

#read in data
fa_pm = read.csv("../Data/Modelling Data/fa_pm-model.csv")

#convert column types
fa_pm$total_population = as.numeric(fa_pm$total_population)
fa_pm$food_insecurity_num_2020 = as.numeric(fa_pm$food_insecurity_num_2020)
fa_pm$food_insecurity_num_2018 = as.numeric(fa_pm$food_insecurity_num_2018)

#target variable
target = 'food_insecurity_num_2020'

#plots of target variable
fa_pm %>% ggplot(aes(x = food_insecurity_num_2020)) + geom_histogram()

model_set1 = c('avg_household_size', 'avg_travel_time_to_work', 'computer_access',
                     'life_expectancy', 'median_age', 'median_home_loan_amount',
                     'median_home_value', 'median_household_income', 'median_leverage_ratio',
                     'num_jobs', 'num_housing_units', 'proportion_bachelors_degree',
                     'proportion_disabled', 'proportion_hispanic', 'prop_men',
                     'prop_nonwhite', 'prop_nonenglish_speaking', 'prop_families_poverty',
                     'prop_students_in_public_school', 'social_vulnerability_index',
                     'thiel_racial_segregation_index', 'total_population', target)

linear_model = lm(food_insecurity_num_2020 ~ ., data = fa_pm %>% select(model_set1))
summary(linear_model)
#the r-sq is quite low! 0.09845, adj.r-sq = 0.08304

model_set2 = c('avg_household_size','avg_travel_time_to_work', 'median_age', 
             'num_housing_units', 'proportion_hispanic', 'prop_nonwhite', 
             'social_vulnerability_index', 'total_population', target)

linear_model2 = lm(food_insecurity_num_2020 ~. ,data = fa_pm %>% select(model_set2))
summary(linear_model2)
#r-sq is still quite low! rsq = 0.08303, adj-r-sq = 0.07739


#new modelling set contains food_insec_rate_2020
target2 = 'food_insecurity_rate_2020'
#try out a glm for exponentially distributed dependent variable (use gamma family)
linear_model3 = glm(food_insecurity_rate_2020 ~., family = "Gamma", data = data.mod3)
summary(linear_model3)

modelling_cols3 = c('food_insecurity_num_2018','num_housing_units', 'social_vulnerability_index',
                    'median_household_income', 'prop_nonwhite', 'median_home_value', 'median_home_loan_amount', 'prop_families_poverty',
                    'proportion_bachelors_degree', 'life_expectancy', target, 'total_population')
linear_model4 = lm(food_insecurity_num_2020 ~., data = fa_pm %>% select(modelling_cols3))
summary(linear_model4)

##################MODEL WITH THE EVANSTON ONLY NOW############
fa_pm_evanston = read_csv("../Data/Modelling Data/fa_pm_evanston-model.csv")


#convert column types
fa_pm_evanston$total_population = as.numeric(fa_pm_evanston$total_population)
fa_pm_evanston$food_insecurity_num_2020 = as.numeric(fa_pm_evanston$food_insecurity_num_2020)
fa_pm_evanston$food_insecurity_num_2018 = as.numeric(fa_pm_evanston$food_insecurity_num_2018)



##### model building ##########
#### iterate through variables in list #######
#the variables are in descending order of correlation with food_insecurity_num_2020
var_list = c( 'median_household_income',
'median_age', 'social_vulnerability_index',
'median_home_loan_amount', 'median_home_value', 'prop_nonwhite',
'prop_nonenglish_speaking', 'proportion_hispanic',
'thiel_racial_segregation_index', 'proportion_bachelors_degree',
'computer_access', 'prop_families_poverty', 'num_jobs',
'num_housing_units', 'life_expectancy', 'avg_travel_time_to_work',
'prop_students_in_public_school', 'median_leverage_ratio',
'proportion_disabled', 'avg_household_size', 'prop_men',
'local_census_tract', 'unemployment_change')

mod_set = c("food_insecurity_num_2020")
results = data.frame(index = c(), adj_rsq = c())
i = 0
for (var in var_list) {
  mod_set = append(mod_set, var)
  i = i + 1
  lin.mod = lm(food_insecurity_num_2020 ~ ., 
               data = fa_pm_evanston %>% select(mod_set))
  adj_rsq = summary(lin.mod)$adj.r.squared
  results = rbind(results, list(i, adj_rsq))
}

vars_list.subset = c( 'median_household_income',
                      'median_age', 'social_vulnerability_index',
                      'median_home_loan_amount', 'median_home_value', 'prop_nonwhite',
                      'prop_nonenglish_speaking', 'proportion_hispanic',
                      'thiel_racial_segregation_index', 'proportion_bachelors_degree',
                      'computer_access', 'prop_families_poverty', 'num_jobs',
                      'num_housing_units', 'life_expectancy', 'food_insecurity_num_2020')
mod.evanston.subset1 = lm(food_insecurity_num_2020 ~.,
                        data = fa_pm_evanston %>% select(vars_list.subset))
summary(mod.evanston.subset1)

##subset 2
mod.evanston.subset2 = lm(food_insecurity_num_2020 ~ 
                          median_household_income + 
                          median_age + 
                          social_vulnerability_index +
                          median_home_loan_amount + 
                          median_home_value + 
                          thiel_racial_segregation_index + 
                          proportion_bachelors_degree  + 
                          computer_access + 
                          num_jobs + 
                          num_housing_units + 
                          life_expectancy, 
                        data = fa_pm_evanston)
summary(mod.evanston.subset2)

########## subset 3
mod.evanston.subset3 = lm(food_insecurity_num_2020 ~ 
                            median_household_income + 
                            median_age + 
                            social_vulnerability_index +
                            median_home_loan_amount + 
                            median_home_value + 
                            thiel_racial_segregation_index + 
                            num_housing_units + 
                            life_expectancy, 
                          data = fa_pm_evanston)
summary(mod.evanston.subset3)

######subset 4
mod.evanston.subset4 = lm(food_insecurity_num_2020 ~ 
                            median_household_income + 
                            median_age + 
                            social_vulnerability_index +
                            median_home_value + 
                            thiel_racial_segregation_index + 
                            num_housing_units,
                          data = fa_pm_evanston)
summary(mod.evanston.subset4)
