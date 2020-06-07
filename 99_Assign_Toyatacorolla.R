getwd()
toyata_D1 <- read.csv("ToyotaCorolla.csv")
View(toyata_D1)
attach(toyata_D1)

toyata_D2 <- (toyata_D1[,c(3,4,7,9,13,14,16,17,18)])
View(toyata_D2)
attach(toyata_D2)
names(toyata_D2)

pairs(toyata_D2)
cor(toyata_D2)

#Model Building
toyota_m1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = toyata_D2)
summary(toyota_m1) # Adjusted R-squared:  0.863 
                   # All parameters are significant except 'cc' and 'doors'.

install.packages("car")
library(car)
vif(toyota_m1) # Vif values for all independent variables are < 10.
avPlots(toyota_m1) # Doors VS price: There could be no variation in price explained by doors.

# Find influencial observations.
influence.measures(toyota_m1)
influenceIndexPlot(toyota_m1) #81st observation could be influential observation.
influencePlot(toyota_m1)      # 81st,222 observation is the influencial observation.

# Model building without 81st observation.
toyota_m2 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = toyata_D2[-81,])
summary(toyota_m2) # Adjusted R-squared:  0.8686 
                   # All parameters are significant except doors.


# Model building without 81st and 222 observation.
toyota_m3 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = toyata_D2[-c(81,222),])
summary(toyota_m3) # Adjusted R-squared:  0.8772
                   # All parameters are significant except doors
vif(toyota_m3)
avPlots(toyota_m3) # no variation expalined in price by doors.

# Model building without doors independent variable.
toyota_m4<- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data = toyata_D2[-c(81,222),])
summary(toyota_m4) # Adjusted R-squared:  0.877 
                   # All parameters are significant to use in our model.

# Modeling building using log transformation.
toyota_m5 <- lm(log(Price)~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = toyata_D2[-c(81,222),])
summary(toyota_m5)  # Adjusted R-squared:  0.8542 , sligthly decreases when compared to previous model.
                    # All parameters are significant to use in our model.


# Modeling building using log transformation.
toyota_m6 <- lm(Price~log(Age_08_04)+log(KM)+log(HP)+log(cc)+log(Doors)+log(Gears)+log(Quarterly_Tax)+log(Weight)
                ,data = toyata_D2[-c(81,222),])
summary(toyota_m6) # Adjusted R-squared:  0.8455 , slightly decreases when compared to previous model.
                   # All parameters are significant to use in our model.


# R Automated models.
install.packages("MASS")
library(MASS)
stepAIC(toyota_m1) # lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
#Weight, data = toyata_D2) as this model have lower AIC value when compared to other automated models.



