getwd()

comp_da <- read.csv("Computer_Data.csv")
view(comp_da)
attach(comp_da)

#Creating the dummy variables.
names(comp_da)
comp_da2 <- cbind(comp_da,ifelse(cd=="yes",1,0),ifelse(multi=="yes",1,0),ifelse(premium=="yes",1,0))
view(comp_da2)
comp_da3 <- comp_da2[,-c(1,7:9)]
view(comp_da3)
names(comp_da3)
#Renaming columns
comp_da3 <- comp_da3 %>% 
  rename(
    cd_dummy="ifelse(cd == \"yes\", 1, 0)",
    multi_dumy= "ifelse(multi == \"yes\", 1, 0)",
    premium_dummy="ifelse(premium == \"yes\", 1, 0)"
  )
view(comp_da3)
attach(comp_da3)

# Check the Correlation.
pairs(comp_da3)
pairs(comp_da3[,-c(8,9,10)])
cor(comp_da3[,-c(8,9,10)]) 

#Model Building
comp_model1 <- lm(price~speed+hd+ram+screen+ads+trend+cd_dummy+multi_dumy+premium_dummy,data=comp_da3)
summary(comp_model1) # All parameters are significant to use in our model.
                     # Adjusted R-squared:  0.7752 
# Find influencial observations
influenceIndexPlot(comp_model1)
influencePlot(comp_model1,id.n=3) # obs 1441  = 0.011819949
                                  # obs 1701  = 7.1838002 are infulencial obs.
# model building without influencial observation.
comp_model2 <- lm(price~speed+hd+ram+screen+ads+trend+cd_dummy+multi_dumy+premium_dummy,
                  data=comp_da3[-c(1441,1701),])
summary(comp_model2) # Adjusted R-squared:  0.7774
                     # Parameters are significantly to use in the model.
#
avPlots(comp_model2)
vif(comp_model2) # VIF value for the independent variables < 10.

# Model building with log transformation.
comp_model3 <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+log(ads)+log(trend)+cd_dummy+multi_dumy+premium_dummy,
                  data=comp_da3[-c(1441,1701),])
summary(comp_model3) # Adjusted R-squared:  0.7441.

comp_model4 <- lm(log(price)~speed+hd+ram+screen+ads+trend+cd_dummy+multi_dumy+premium_dummy,
                  data=comp_da3[-c(1441,1701),])
summary(comp_model4) # Adjusted R-squared:  0.7833
                     # Parameters are significant to use in our model
comp_model3a <- lm(log(price)~log(speed)+log(hd)+log(ram)+log(screen)+log(ads)+log(trend)+cd_dummy+multi_dumy+premium_dummy,
                  data=comp_da3[-c(1441,1701),])

summary(comp_model3a) # Adjusted R-squared:  0.7625 
                      # Parameters are significant to use in our model
# Model building using sqrt transformation.
comp_model5 <- lm(sqrt(price)~speed+hd+ram+screen+ads+trend+cd_dummy+multi_dumy+premium_dummy,
                  data=comp_da3[-c(1441,1701),])
summary(comp_model5) # Adjusted R-squared:  0.7861
                     # Parameters are significant to use in our model.

comp_model6 <- lm(price~sqrt(speed)+sqrt(hd)+sqrt(ram)+sqrt(screen)+sqrt(ads)+sqrt(trend)+cd_dummy+multi_dumy+premium_dummy,
                  data=comp_da3[-c(1441,1701),])
summary(comp_model6) # Adjusted R-squared:  0.7889
                     # Parameters are significant to use in our model.

comp_model7 <- lm(sqrt(price)~sqrt(speed)+sqrt(hd)+sqrt(ram)+sqrt(screen)+sqrt(ads)+sqrt(trend)+
                    sqrt(cd_dummy)+sqrt(multi_dumy)+sqrt(premium_dummy),
                  data=comp_da3[-c(1441,1701),])
summary(comp_model7) # Adjusted R-squared:  0.8009
                     # Parameters are significant to use in our model.

confint(comp_model7,level = 0.95)
pred1 <- predict(comp_model7,interval = "predict")
pred1 <- as.data.frame(pred1)
view(pred1)

comp_da4 <- comp_da3[-c(1441,1701),]
cor(pred1$fit,comp_da4)
install.packages("corpcor")
library(corpcor)
cor(comp_da4)
cor2pcor(cor(comp_da4))

# To check the system autoamted models.
stepAIC(comp_model1)
#price ~ speed + hd + ram + screen + ads + trend + cd_dummy + 
#multi_dumy + premium_dummy: AIC=70336.65
# only the above model shown using step AIC










