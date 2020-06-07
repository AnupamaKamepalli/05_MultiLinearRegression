getwd()
setwd("C:/Users/anupa/Desktop/ExcelR/Assignment/05_MultiLinear_Regression")
data1 <- read.csv("50_Startups.csv")
summary(data1)
attach(data1)
View(data1)
unique(State)

#Creating the dummy variables of categorical independent variable 'state'.
data2_su <- cbind(data1,ifelse(State=="New York",1,0),ifelse(State=="California",1,0),ifelse(State=="Florida",1,0))
View(data2_su)

# to view the list of columns in a dataset
colnames(data2_su)
install.packages("tidyverse")
library(tidyverse)

#Rename Column names
data2_su <- data2_su %>%
rename(
   NY = 'ifelse(State == \"New York\", 1, 0)',
   CA = 'ifelse(State == \"California\", 1, 0)',
   FL = 'ifelse(State == \"Florida\", 1, 0)'
)
data2_su <- data2_su %>%
  rename(
    RDspend='R.D.Spend',
    Marketing='Marketing.Spend'
  )
View(data2_su)
attach(data2_su)

# To view the all pairs of plot except the categorical variable 'state'.
names(data2_su)
plot(data2_su[,-4])
plot(data2_su[,-c(4,6,7,8)]) # Removing the state and dummy variables.
cor(data2_su[,-4]) #0.97290047, there is correlatiob between R.D.spend,Profit.
cor(data2_su[,-c(4,6,7,8)])


# Model1:

prof_model <- lm(Profit~RDspend+Administration+Marketing)
summary(prof_model) # Multiple R-squared:  0.9507,
                    #Marketing and Administration are insignificant to use in our model.

vif(prof_model)   #vif value is high for RDspend : 2.468903
# As some of the parameters are insignificant. Check for influencial records.
install.packages("car")
library(car)
influenceIndexPlot(prof_model)
influencePlot(prof_model,id.n=1) # obs 49 = 0.19052744,obs 50 = 0.28808229 are influential observation.

prof_model2 <- lm(Profit~RDspend+Administration+Marketing,data = data2_su[-c(50,49),])
summary(prof_model2) # Adjusted R-squared:  0.9601 , a slight increase in accuracy.
                     # Parameters are insignificant to use in our model.
vif(prof_model2) # vif value is high for RDspend : 2.250972

# Check the variation using AV plot
avPlots(prof_model)
avPlots(prof_model2) # I can see the variation in dependent variable 'profit'
                     # Explained by the input variable 'RDspend','admistration','marketing'.

# Now will see, the default models.
prof_rev_model <- lm(Profit~RDspend+Administration+Marketing+NY+CA+FL,data = data2_su)
summary(prof_rev_model)

install.packages("MASS")
library(MASS)

stepAIC(prof_rev_model) # AIC=915.18,Model = Profit ~ RDspend + Marketing, data = data2_su.

# create a model using stepaic info.
prof_model_final <- lm(Profit~RDspend+Marketing,data=data2_su) 
summary(prof_model_final) # Adjusted R-squared:  0.9483
                          # all parameters are significant to use expect marketing.






