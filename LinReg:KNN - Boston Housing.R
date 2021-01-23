library(MLBench)
library(caret)
library(corrplot)
library(ggplot2)
library(dplyr)

data("BostonHousing")
data = BostonHousing
str(data)
?BostonHousing

#Checking for Null values
colSums(is.na(data))

#corrplot: 
corrplot(cor(select(data,-chas)),method='pie')

#Data Partition
set.seed(123)
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train = data[ind ==1,]
test = data[ind ==2,]
str(test)
head(data)

#medv decreases as crim (medium), indus (High),nox(low),
#age(low),rad(low),tax(low),ptratio(high), lstat (High) increases
#and increases with increase in zn(low),rm(High).

# Linear regression model
model = lm(medv ~. , data = train) #medv: dependent variable
summary(model)
plot(model)

#Predictions
lnpred = predict(model,test)

#Plot actual and predicted results
predplot = test %>% 
  ggplot(aes(medv,lnpred)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='red')) +
  xlab('Actual median value of owner-occupied homes in USD (000s)') +
  ylab('Predicted median value of owner-occupied homes in USD (000s)') +
  theme_classic()

predplot


#KNN Model: 
trCtrl = trainControl(method = 'repeatedcv', #repeated cross validation
                         number = 10, #number of resampling iterations
                         repeats = 3) #number of complete set of folds to repeat CV. Repeats process 3x

fit = train(medv ~., 
            data = train, 
            method = 'knn',
            tuneLength = 20, 
            trControl = trCtrl,
            preProc = c("center", "scale"), #normalise data. Subtract mean and divide by std. dev
            tuneGrid = expand.grid(k = 1:50)) #Create a data frame from all combinations of the factors

#Rsquared' can also be used as a metric, higher the better. Alternative to RMSE (default)
?train
#Model performance
fit
#RMSE used for determining k. Lower RMSE more desirable. k=2

#plot
plot(fit)
 
#Variable Importance
varImp(fit)
#lstat most important in determining medv, chas least important

#Prediction: test data
knnpred = predict(fit, newdata = test)

plot(pred ~ test$medv) 
#Actual values = x axis, Predicted values = y axis
