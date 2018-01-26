
library(Hmisc)
library(tidyverse)
library(dplyr)
library(MASS)
library(car)
library(knitr)
library(gvlma)
library(ggplot2)
library(ggthemes)


dfComp2 <- read.csv("dfComp.csv")

dfComp <- dfComp2
dfComp <- dplyr::select(dfComp, 2:12)

  # I do not want to include totalRooms as a predictor because it appears to be innacurate and has many missing values
  # Also remove zpid column

formodel <- dplyr::select(dfComp, c(2:7, 9,11))

  # Observing initial model with all predictors

model <- lm(zestimate ~ . ,
            data = formodel)
summary(model)

  # Initial model appears solid. Next I need to remove any rows with NA values in order to examine a stepwise variable selection.

rm_NA <- na.omit(formodel)

  # That appeared to remove a fairly significant number of observations. Later we might consider a different method
  # for handling the missing values.

  # Refit model for stepwise variable selection

model2 <- lm(zestimate ~ . , 
             data = rm_NA)

step <- stepAIC(model2, direction = "both", trace = FALSE)
summary(step)
step$anova

  # Observe the best model based on stepwise variable selection

modelstep <- lm(zestimate ~ 
                  taxAssessment +
                  yearBuilt +
                  lotSizeSqFt +
                  finishedSqFt +
                  bathrooms +
                  lastSoldPrice, 
                data = rm_NA)

  # Determine if there are any problems with multicollinearity

vif(modelstep)

  # The vif values do not appear to be problematic with these predictors.
  # Next I need to shuffle the data and then split it into training, test, and validation sets.

set.seed(55)
shuffle <- rm_NA[sample(nrow(rm_NA), nrow(rm_NA)),]

row.names(shuffle) <- 1:nrow(shuffle)

train <- shuffle[c(1:200),]
test <- shuffle[c(201:243),]
validate <- shuffle[c(244:287),]

  # Build an initial model off the training data

modtrain <- lm(zestimate ~ 
                 taxAssessment +
                 yearBuilt +
                 lotSizeSqFt +
                 finishedSqFt +
                 bathrooms +
                 lastSoldPrice, 
               data = train)
summary(modtrain)

  # The training model appears to be solid. 
  # I want to run an initial prediction on the test data to see how well the model generalizes

test$pred <- predict(modtrain, test, type = "response")

test$differential <- test$zestimate - test$pred

test$pctdif <- abs((test$differential / test$zestimate)*100)

hist(test$pctdif)

count(test$pctdif < 10)
(41/43)

count(test$pctdif < 5)
(36/43)

problems <- filter(test, pctdif > 10)
problems

  # The model appears to generalize well to the test set, with 95 % of the predictions falling within 10 % of the true zestimate value,
  # and 83 % falling within 5 %. However, I need to determine if the model meets the assumptions of linear regression.

  # Examine the relationships between predictors and response individually

plot(train$zestimate, train$taxAssessment)
  # Linear w/ a few outliers
plot(train$zestimate, train$lastSoldPrice)
  # Linear w/ a few outliers
plot(train$zestimate, train$finishedSqFt)
  # Linear w/ a few outliers
plot(train$zestimate, train$lotSizeSqFt)
  # Not much relationship
plot(train$zestimate, train$yearBuilt)
  # Not much relationship
plot(train$zestimate, train$bathrooms)
  # Linear ish?

  # Because lotSizeSqFt and yearBuilt to not appear to have a relationship with the zestimate value, I do not think they need to be
  # included as predictors in the model.

  # Identifying outliers 

hist(scale(train$zestimate))
  # Potential Outliers
hist(scale(train$bathrooms))
hist(scale(train$lastSoldPrice))
  # Potential Outliers
hist(scale(train$finishedSqFt))
hist(scale(train$taxAssessment))

  # Remove observations that are outside 3 SDs for any one of the variables

train_OUT <- train %>%
  filter(lastSoldPrice < mean(lastSoldPrice) + 3*sd(lastSoldPrice)) %>%
  filter(lastSoldPrice > mean(lastSoldPrice) - 3*sd(lastSoldPrice)) %>%
  filter(zestimate < mean(zestimate) + 3*sd(zestimate)) %>%
  filter(zestimate > mean(zestimate) - 3*sd(zestimate)) %>%
  filter(finishedSqFt < mean(finishedSqFt) + 3*sd(finishedSqFt)) %>%
  filter(finishedSqFt > mean(finishedSqFt) - 3*sd(finishedSqFt)) %>%
  filter(taxAssessment < mean(taxAssessment) + 3*sd(taxAssessment)) %>%
  filter(taxAssessment > mean(taxAssessment) - 3*sd(taxAssessment)) %>%
  filter(bathrooms < mean(bathrooms) + 3*sd(bathrooms)) %>%
  filter(bathrooms > mean(bathrooms) - 3*sd(bathrooms))

hist(scale(train_OUT$zestimate))
hist(scale(train_OUT$bathrooms))
hist(scale(train_OUT$lastSoldPrice))
hist(scale(train_OUT$finishedSqFt))
hist(scale(train_OUT$taxAssessment))


  # Examine potential issues with multicollinearity

rcorr(as.matrix(train_OUT))
  # Predictors taxAssessment and lastSoldPrice have a correlation > .9. 

modtrainOUT <- lm(zestimate ~ 
                 taxAssessment +
                 finishedSqFt +
                 bathrooms +
                 lastSoldPrice, 
               data = train_OUT)
summary(modtrainOUT)

vif(modtrainOUT)
  # Although the VIFs for these variables are not > 10, they are relatively high. Therefore, I probably won't include both in the final model.


durbinWatsonTest(modtrainOUT)
  # Fail to reject the null. Conclude that no first order autocorrelation exists.
ncvTest(modtrainOUT)
  # Fail to reject the null. Conclude that error variance is constant.
par(mfrow = c(2,2))
plot(modtrainOUT)
  # Based on the top left graph, the assumption for homoscedasticity appears to be satisfied.
  # Based on the top right graph, the assumption for normality of residuals appears to be violated.

mean(modtrainOUT$residuals)
  # The mean of the residuals is approximately zero
residualPlot(modtrainOUT, type = "rstudent")
  # The red line is pretty flat
hist(scale(residuals(modtrainOUT)))
  # The distribution of residuals appears to be a bit skewed
boxplot(residuals(modtrainOUT))
  # Potential outliers?
qqnorm(rstandard(modtrainOUT))

qqline(rstandard(modtrainOUT))
  # Not a good sign

  # Which predictor is causing the problems with nonnormality?

x <- lm(zestimate ~
          lastSoldPrice, 
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Problem

x <- lm(zestimate ~ 
          taxAssessment, 
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Problem

x <- lm(zestimate ~ 
           bathrooms, 
           data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Better, but still not good


x <- lm(zestimate ~ 
          finishedSqFt, 
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Best so far. Start with finishedSqFt in the model. 

  # Choose between lastSoldPrice and taxAssessment.

x <- lm(zestimate ~ 
          finishedSqFt +
          lastSoldPrice,
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Not good. Maybe if we transform?

x <- lm(zestimate ~ 
          finishedSqFt +
          sqrt(lastSoldPrice),
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Better, but still not good.

x <- lm(zestimate ~ 
          finishedSqFt +
          taxAssessment,
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Appears to be pretty skewed. Transform?

x <- lm(zestimate ~ 
          finishedSqFt +
          sqrt(taxAssessment),
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Still pretty skewed, but the best so far. I will need to examine why those points are causing a problem. Should bathrooms be included in the model?

x <- lm(zestimate ~ 
          bathrooms +
          finishedSqFt +
          sqrt(taxAssessment),
          data = train_OUT)
summary(x)
qqnorm(rstandard(x))
qqline(rstandard(x))
  # Does not add any additional explanation of the variation. Does not help the violation of nonnormality -- do not include in model.

mod <- lm(zestimate ~ 
          finishedSqFt +
          sqrt(taxAssessment),
          data = train_OUT)

par(mfrow = c(2,2))
plot(mod)
  # It looks like observations 28, 112, and 147 are the troublemakers.

trouble <- dplyr::select(train_OUT, c(1,4,7))
trouble <- trouble[c(28,112,147),]

  # Making some comparisons to the entire data set.

mean(abs(train$taxAssessment - train$zestimate)) + 3*sd(abs(train$taxAssessment - train$zestimate))
abs(trouble$taxAssessment - trouble$zestimate)
  # One reason these observations are potentially skewing my model is because there is a much larger difference between the taxAssessment
  # and the zestimate than what is observed in the training data as a whole.

dfbetas(mod) > 2/sqrt(nrow(train_OUT))
  # A second reason could be that the dfbetas is large for two coefficients for observations 28 and 112, and it is large for one coefficient
  # for observation 147.

  # Let's observe the model without these three observations.

modOUT <- lm(zestimate ~ 
                  finishedSqFt + 
                  sqrt(taxAssessment),
                  data = train_OUT[-c(28,112,147),])
summary(modOUT)
qqnorm(rstandard(modOUT))
qqline(rstandard(modOUT))
  # The removal of these three observations appeared to bring the model closer to satisfying the assumption of normality.
  # Let's recheck our other assumptions.

vif(modOUT)
  # Looks good
durbinWatsonTest(modOUT)
  # Fail to reject the null. Conclude that no first order autocorrelation exists.
ncvTest(modOUT)
  # Fail to reject the null. Conclude that error variance is constant.
par(mfrow = c(2,2))
plot(modOUT)
  # Based on the top left graph, the assumption for homoscedasticity appears to be satisfied.

mean(modOUT$residuals)
  # The mean of the residuals is approximately zero
hist(scale(residuals(modOUT)))
  # The distribution of residuals appears to have a few outliers, observation 80.
boxplot(residuals(modOUT))

cooksd <- cooks.distance(modOUT)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
  # There are no observatiosn with a Cooks distance > 1, but observation 80 appears to be the most problematic.
  # Let's observe the model without observation 80

modOUT <- lm(zestimate ~ 
               finishedSqFt + 
               sqrt(taxAssessment),
               data = train_OUT[-c(28,80,112,147),])
summary(modOUT)
qqnorm(rstandard(modOUT))
qqline(rstandard(modOUT))
vif(modOUT)
  # Looks good
durbinWatsonTest(modOUT)
  # Fail to reject the null. Conclude that no first order autocorrelation exists.
ncvTest(modOUT)
  # Fail to reject the null. Conclude that error variance is constant.
par(mfrow = c(2,2))
plot(modOUT)
  # Better
mean(modOUT$residuals)
  # The mean of the residuals is approximately zero
hist(scale(residuals(modOUT)))
  # The distribution appears normal

  # I think I can accept this as my final model

modfinal <- lm(zestimate ~ 
               finishedSqFt + 
               sqrt(taxAssessment),
             data = train_OUT[-c(28,80,112,147),])
summary(modfinal)

  # Let's test the final model on the validation set and compare to our previous results

validate$pred <- predict(modfinal, validate, type = "response")

validate$differential <- validate$zestimate - validate$pred

validate$pctdif <- abs((validate$differential / validate$zestimate)*100)

hist(validate$pctdif)

count(validate$pctdif < 10)
(35/44)

count(validate$pctdif < 5)
(22/44)

moreproblems <- filter(validate, pctdif > 10)
moreproblems <- dplyr::select(moreproblems, finishedSqFt, taxAssessment, zestimate)
moreproblems

  # This model did not perform as well, most likely because we no longer include both taxAssessment and 
  # lastSoldPrice, which together provided a very good prediction. However, half of the predictions are 
  # still within 5 % of the true zestimate, while 80 % are within 10 %. 

  # Use model to predict property of interest (1906 Brooke Stone Ct, 40014)

EST1906 <- -196300 + 12.32 * 1890 + 957.6 * sqrt(270000)
REAL1906 <- 319132

(abs(EST1906 - REAL1906)/REAL1906)*100

  # The prediction is 1.7 % away from the true zestimate for this property
  
# Nice plots for write up

ggplot(test, aes(x = test$pctdif)) +
  geom_histogram(aes(y = ..count..), breaks=seq(0, max(test$pctdif)+5, by = 5),
                 colour = "#FFFFFF", fill = "#666633") +
  labs(x = "% Difference") +
  scale_y_continuous(name = "Count") +
  ggtitle("Difference between Predicted Value and Zestimate (%)") +
  theme_wsj() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(size = 9))


ggplot(validate, aes(x = validate$pctdif)) +
  geom_histogram(aes(y = ..count..), breaks=seq(0, max(validate$pctdif)+5, by = 5),
                 colour = "#FFFFFF", fill = "#666633") +
  labs(x = "% Difference") +
  scale_y_continuous(name = "Count") +
  ggtitle("Difference between Predicted Value and Zestimate (%)") +
  theme_wsj() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(size = 9))
