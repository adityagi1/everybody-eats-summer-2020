library(tidyverse)
library(readxl)
library(lubridate)
library(imputeTS)

pm = read_excel("../../Data/ProduceMobile/Producemobile_historical_data_clean.xlsx",
                sheet = "Sheet1")
#rename column names
names(pm) = c("date", "in_out", "amt_food_received","num_guests_signed_in",
   "num_individuals_served", "truck_arrival_time","num_volunteers",
   "leftover_pickup","num_boxes_pickup", "num_boxes_waste","note")

#form food received time series (has missing values)
amt_food_rec.ts = ts(pm$amt_food_received, frequency = 12)
plot(amt_food_rec.ts)

#form num individuals served time series (has missing values)
num_ind.ts = ts(pm$num_individuals_served, frequency = 12)
plot(num_ind.ts)

#form the num volunteers time series (has missing values)
num_volunteers.ts = ts(pm$num_volunteers, frequency = 12)
plot(num_volunteers.ts)




#num_ind, amt_food_rec, num_volunteers have na's that will need to be imputed
num_ind.ts.imp = na_seadec(num_ind.ts, algorithm = "interpolation")
plot(num_ind.ts.imp)
ggplot_na_imputations(num_ind.ts, num_ind.ts.imp)

#amount food received
amt_food_rec.ts.imp = na_seadec(amt_food_rec.ts, algorithm = "interpolation")
plot(amt_food_rec.ts.imp)
ggplot_na_imputations(amt_food_rec.ts, amt_food_rec.ts.imp)

#num_volunteers
num_volunteers.ts.imp = na_seadec(num_volunteers.ts, algorithm = "interpolation")
ggplot_na_imputations(num_volunteers.ts, num_volunteers.ts.imp)


#####Autocorrelation plots to check seasonality

#use smoothed data (using ma) as a preliminary estimate of the trend
num_ind.ts.ma = ma(num_ind.ts.imp, order = 12, centre = TRUE)
amt_food.ts.ma = ma(amt_food_rec.ts.imp, order = 12, centre = TRUE)
num_volunteers.ts.ma = ma(num_volunteers.ts.imp, order = 12, centre = TRUE)

#now compute the autocorr function of the de-trended data
acf(na_remove(num_ind.ts.imp - num_ind.ts.ma))
acf(na_remove(amt_food_rec.ts.imp - amt_food.ts.ma))
acf(na_remove(num_volunteers.ts.imp - num_volunteers.ts.ma))

#there are strong auto-correlations (nearing 0.4-0.5), 
#indicating seasonality is present

######Time Series Decomposition
num_ind.decomp = decompose(num_ind.ts.imp, type = "additive")
num_volunteers.decomp = decompose(num_volunteers.ts.imp, type = "additive")
amt_food_rec.decomp = decompose(amt_food_rec.ts.imp, type = "additive")


#visualize decompositions
plot(num_ind.decomp)#, main = "Num. Individuals Decomposition")
plot(num_volunteers.decomp) #main = "Num. Volunteers Attending Decomposition")
plot(amt_food_rec.decomp)# main = "Amount food received Decomposition")
#there are strong seasonal and trend components, therefore it makes sense
#to use a holt-winters model

######MODELLING
num_ind.hw.mod = HoltWinters(num_ind.ts.imp, seasonal = "additive")
amt_food_rec.hw.mod = HoltWinters(amt_food_rec.ts.imp, seasonal = "additive")
num_volunteers.hw.mod = HoltWinters(num_volunteers.ts.imp, seasonal = "additive")

# let's look at summaries for the model
num_ind.hw.mod 
amt_food_rec.hw.mod
num_volunteers.hw.mod


#####INTERPRETATIONS

#a very strong trend detected because beta = 0 for all three time series

##interpretations for seasonality
###NUM_IND_SERVED
#fewer individuals are served during:
#- Jan
#- Feb 
#- April
#- october 
#- december
#more individuals are served during:
#- march
#- may
#- september
#- november

### AMT_FOOD_RECEIVED
#lesser food is received during
#- february
#- april
#- june
#- october
#- december

#more food is received during: 
#january, 
#march, 
#may, 
#july, 
#september, 
#november


#more volunteers tend to come in:
#january
#february
#march
#april
#september
#november

#less volunteers tend to come in:
#may
#june
#july
#august
#october
#december

######PREDICTIONS

num_ind.hw.pred = predict(num_ind.hw.mod, n.ahead = 6, prediction.interval = TRUE)
amt_food_rec.hw.pred = predict(amt_food_rec.hw.mod, n.ahead = 6, prediction.interval = TRUE)
num_volunteers.hw.pred = predict(num_volunteers.hw.mod, n.ahead = 6, prediction.interval = TRUE)


##plot predictions
plot(num_ind.hw.mod, num_ind.hw.pred, type = 'b', main = "Forecasted Num. Individuals Arriving Till December 2020")
plot(amt_food_rec.hw.mod, amt_food_rec.hw.pred, type = 'b', main = "Forecasted Amt. Food Arriving Till December 2020")
plot(num_volunteers.hw.mod, num_volunteers.hw.pred, type = 'b', main = "Forecasted Num. Volunteers Till December 2020")
