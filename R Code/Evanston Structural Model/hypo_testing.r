library(tidyverse)
library(ggplot2)

#test hypotheses on evanston only data
fa_pm_evanston = read_csv("../Data/Modelling Data/fa_pm_evanston-model.csv")

#top 5 tracts
top_n = fa_pm_evanston %>% arrange(desc(food_insecurity_num_2018))%>% head(5) 
#bottom 5 tracts 
bottom_n = fa_pm_evanston %>% arrange(desc(food_insecurity_num_2018)) %>% tail(5)

#let's conduct a t-test on the avg. household size
t.test(top_n$avg_household_size, bottom_n$avg_household_size, 
       alternative = 'two.sided')

for (var in var_list) {
  print(var)
  top_n_mean = mean(top_n[[var]])
  bottom_n_mean = mean(bottom_n[[var]])
  diff = top_n_mean - bottom_n_mean
  print(paste("top_n_mean:", top_n_mean))
  print(paste("bottom_n_mean:", bottom_n_mean))
  print(paste("diff:", top_n_mean - bottom_n_mean))
  print("------")
}

