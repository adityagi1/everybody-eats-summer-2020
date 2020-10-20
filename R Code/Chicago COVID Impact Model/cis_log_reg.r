library(tidyverse)
library(car)
datapath = "../../Data/COVID Impact Survey/cis_proc_data/cis_week1_proc.csv"
cis = read_csv(datapath)
#drop the index read in
cis = cis %>% select(-X1)
#convert food insecurity to numeric from boolean
cis$food_insec = as.numeric(cis$food_insec)
#drop ECON5A_A, ECON5B_B since they were used to construct the dep var
cis = cis %>% select(-ECON5A_A, -ECON5A_B)

####### MOD 1 ###########

log.mod1 = glm(food_insec ~ ., 
               data = cis %>% select(-SU_ID), 
               family = 'binomial')
summary(log.mod1) #AIC: 365.27, Null Dev: 425.18, Res. Dev: 153.27
#the following variables are significant:
#- SOC1 relative to (1) All
#  (2) MOST: 2.592
#- ECON3:  4.192e-02
#- ECON4A relative to (1) Extremely likely
#  (3) Moderately likely:  2.189
#  (4) Not too likely:  3.756
#- ECON4B: relative to (1) Extremely likely
#  (4) Not too likely:  -2.687
#- ECON6B: relative to (1) Received
#  (4) Did not receive nor apply for any: 2.798
#- ECON6E: relative to (1) Received
#  (4) Did not receive nor apply for any: -4.091
#- PHYS9A: relative to (1) Yes
#  (2) No: -1.550
#- PHYS9B: relative to (1) Yes
#  (2) No: -2.359
#- PHYS9D: relative to (1) Yes
#  (2) No: -3.524
#- PHYS9H: relative to (1) Yes
#  (2) No: -1.794
#- RACETH: relative to (1) White, non-Hispanic
#  (2) Black, non-Hispanic: 1.655
#  (4) Other, non-Hispanic: 2.148
#- HHINCOME: relative to "$10,000 to under $20,000" 
#  $100,000 to under $150,000: -5.770
#  $150,000 or more: -4.333
#  $30,000 to under $40,000: -3.812
#  $50,000 to under $75,000: -3.869
#  $75,000 to under $100,000: -3.912
#- EDUCATION: relative to (1) No HS diploma"
#  (6) Masters degree: -2.488

####### MOD2 ##############
#let's build a model with the above significant vars
log.mod2 = glm(food_insec ~ SOC1 + ECON3 + ECON4A + ECON4B + ECON6B + ECON6E + PHYS9A 
    + PHYS9B + PHYS9D + PHYS9H + RACETH + HHINCOME + EDUCATION, data = cis,
    family = 'binomial') 
summary(lod.mod2) #AIC: 330.71; Null Dev. 425.18; Residual Dev: 248.71

#the residual deviance has increased from 153.2721 to 248.7105 
#but the AIC has decreased from 365.271 to 330.7105


####### MOD3 #############
#now we drop SOC1, ECON4B, ECON6B, ECON6E, PHYS9B, PHYS9H, EDUCATION

log.mod3 = glm(food_insec ~ ECON3 + ECON4A + PHYS9A 
                + PHYS9D + RACETH + HHINCOME, data = cis, family = 'binomial')
summary(log.mod3) #AIC: 311.15; Null Dev: 425.18; Res. Dev. 271.15

#the residual deviance has further increased from 248.7105 to 271.15 
#but the AIC has decreased from 330.7105 to 311.15


###### model building using forward selection algorithm
fs.mod1 = glm(food_insec ~ 1, data = cis %>% select(-SU_ID), 
          family = "binomial")
#scope includes all variables
step(fs.mod1, scope = ~ SOC1+ SOC2A + SOC3A + SOC4A + PHYS8 + 
       ECON1 + ECON3 + ECON4A + ECON4B + ECON6A + ECON6B + ECON6C + 
       ECON6D + ECON6E + ECON6F + ECON6G + ECON6H + ECON6I + ECON6J + 
       ECON6K   +  ECON6L + PHYS9A + PHYS9B + PHYS9C + PHYS9D + PHYS9E  + 
       PHYS9F + PHYS9G + PHYS9H + PHYS5 + PHYS6   + AGE7 + GENDER + 
       RACETH + HHINCOME + EDUCATION + HHSIZE1 + HH01S + HH25S + 
       HH612S + HH1317S + HH18OVS + P_DENSE)

#the final model as suggested by forward selection algorithm is:
fs.mod = glm(food_insec ~ HHINCOME + AGE7 + RACETH + ECON4A + 
      PHYS6 + ECON6K + PHYS9H + HH612S + SOC3A + ECON3 + PHYS9D, 
    family = "binomial", data = cis %>% select(-SU_ID))
summary(fs.mod) #aic: 293.5; deviance: 223.50

#this is superior to our hand built model (mod3)
#the aic decreased from 311.15 to 293.5 and 
#and the deviance decreased from 271.15 to 223.5

#let's try a backward elimination model
be.mod1 = glm(food_insec ~., data = cis %>% select(-SU_ID), 
              family = "binomial")
step(be.mod1)

#the final model as suggested by backward elimination is:
be.mod = glm(food_insec ~ SOC3A + ECON3 + ECON4A + ECON4B + 
      ECON6B + ECON6E + ECON6J + PHYS9A + PHYS9D + PHYS9H + PHYS5 + 
      AGE7 + GENDER + RACETH + HHINCOME + HH612S, family = "binomial", 
    data = cis %>% select(-SU_ID))
summary(be.mod)#aic: 299.14; #deviance: 205.14

#compared to the forward selection model:
#the aic increased to 299.14 from 293.5 and the deviance decreased to 205.14 from 223.5002


#let's make a final table with results 

results = data.frame("models" = c("log.mod1", "log.mod2", "log.mod3", "fs.mod", "be.mod"),
           "aic"= c(365.2721, 330.7105, 311.1549, 293.5002, 299.1369),
           "deviance" = c(153.2721, 248.7105, 273.1549,223.5002, 205.1369))

results %>% arrange(deviance)
results %>% arrange(aic)

#let's use the backward elimination model as it is a good all-round model
#(second lowest AIC, and second lowest deviance)
#here is an interpretation
