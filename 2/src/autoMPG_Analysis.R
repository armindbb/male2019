# Load packages
library(ggplot2)                           # Data visualization
library(readr)                             # CSV file I/O, e.g. the read_csv function
library(base)                              # for finding the file names
library(data.table)                        # for data input and wrangling
library(dplyr)                             # for data wranging
library(forecast)                          # for time series forecasting
library(tseries)                           # for time series data    
library(lubridate)                         # for date modifications
library(tidyr)                             # for data wrangling
library(magrittr)                          # for data wrangling
library(psych)
library(car)
library(leaps)
library(MASS)
library(MLmetrics)
library(reshape2)
library(corrplot)
library(visreg)
library(caret)





# Make sure results are reproducible
set.seed(123)

# Load train.csv data
path_train_autoMPG <-  "2/data/auto/AutoMPG.shuf.train.csv"
autoMPG_train_csv_original <- read.csv(path_train_autoMPG, stringsAsFactors = FALSE)
autoMPG_train_csv <- autoMPG_train_csv_original
class(autoMPG_train_csv_original)
str(autoMPG_train_csv_original)
glimpse(autoMPG_train_csv_original)
summary(autoMPG_train_csv_original) # Find out more about each category of each attribute
names(autoMPG_train_csv_original)
sapply(autoMPG_train_csv_original, class)
sapply(autoMPG_train_csv_original, typeof)

# Load test.csv data
path_test_autoMPG <-  "2/data/auto/AutoMPG.shuf.test.csv"
autoMPG_test_csv_original <- read.csv(path_test_autoMPG, stringsAsFactors = FALSE)
autoMPG_test_csv <- autoMPG_test_csv_original
class(autoMPG_test_csv_original)
str(autoMPG_test_csv_original)
glimpse(autoMPG_test_csv_original)


# Check observations with missing values in train dataset
print(paste("The total number of missing data in train dataset are:",sum(is.na(autoMPG_train_csv_original))))
pattern <- "/|:|\\?|<|>|\\|\\\\|\\*"
grepl(pattern, autoMPG_train_csv_original)

# Check observations with missing values in test dataset
print(paste("The total number of missing data in test dataset are:",sum(is.na(autoMPG_test_csv_original))))
grepl(pattern, autoMPG_test_csv_original)


######################################### Quick view of data ########################
head(autoMPG_train_csv_original)
describe(autoMPG_train_csv_original)
# $ id           <int> 37, 312, 29, 330, 71, 38, 280, 68, ...
# $ mpg          <dbl> 19.0, 32.1, 9.0, 44.6, 13.0, 18.0, ...               continuous
hist(autoMPG_train_csv_original$mpg)
# $ carName      <chr> "ford torino 500", "chevrolet chevette", ...         string (unique for each instance)   --> factor/multi-valued discrete
table(autoMPG_train_csv_original$carName)
unique(autoMPG_train_csv_original$carName)
# $ cylinders    <int> 6, 4, 8, 4, 8, 6, 4, 8, 6, 8, 4, 4, 4,  ...          continuous                          --> factor/multi-valued discrete
table(autoMPG_train_csv_original$cylinders)
# $ displacement <dbl> 250.0, 98.0, 304.0, 91.0, 400.0, 232.0, ...          continuous
unique(autoMPG_train_csv_original$displacement)
# $ horsepower   <chr> "88", "70", "193", "67", "190", "100",  ...          character                           --> continuous/NA's
autoMPG_train_csv_original$horsepower # - When we print out all the unique values in horsepower, we find that there is '?' 
                                      #   which was used as a placeholder for missing values. 
grepl(pattern, autoMPG_train_csv_original$horsepower)
unique(autoMPG_train_csv_original$horsepower)
table(autoMPG_train_csv_original$horsepower)
# $ weight       <int> 3302, 2120, 4732, 1850, 4422, 3288,  ...             continuous
# $ acceleration <dbl> 15.5, 15.5, 18.5, 13.8, 12.5, 15.5, ...              continuous
# $ modelYear    <int> 71, 80, 70, 80, 72, 71, 78, 72, 79, ...              continuous                          --> factor/multi-valued discrete, dummy variable?
unique(autoMPG_train_csv_original$modelYear)
# $ origin       <int> 1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 1, 3,  ...             continuous                          --> factor/multi-valued discrete, dummy variable?
summary(autoMPG_train_csv_original$origin)



########################### Pre-processing for train.csv ##########################
### Data Cleaning
# carName column should be factors (multi-valued discrete) not string
autoMPG_train_csv$carName <- as.factor(autoMPG_train_csv$carName) 
summary(autoMPG_train_csv$carName)
autoMPG_train_csv$carName
# cylinders column should be factors (multi-valued discrete) not integer
autoMPG_train_csv$cylinders <- as.factor(autoMPG_train_csv$cylinders) 
summary(autoMPG_train_csv$cylinders)
autoMPG_train_csv$cylinders
# horsepower is character and it should be continuous numeric variable
autoMPG_train_csv$horsepower <- as.numeric(as.character(autoMPG_train_csv$horsepower)) # Obs: NAs introduced by coercion for "?" values
summary(autoMPG_train_csv$horsepower)
# horsepower has some missing values. We will impute those by mean.
colSums(is.na(autoMPG_train_csv))
autoMPG_train_csv$horsepower[is.na(autoMPG_train_csv$horsepower)] = mean(autoMPG_train_csv$horsepower,na.rm = T)
colSums(is.na(autoMPG_train_csv))
# converting modelYear to factor
autoMPG_train_csv$modelYear <- as.factor(autoMPG_train_csv$modelYear) 
summary(autoMPG_train_csv$modelYear)
autoMPG_train_csv$modelYear
# converting origin to factor since it has only 3 levels
autoMPG_train_csv$origin <- as.factor(autoMPG_train_csv$origin) 
summary(autoMPG_train_csv$origin)


########################### Pre-processing for test.csv ###########################
# carName column should be factors (multi-valued discrete) not string
autoMPG_test_csv$carName <- as.factor(autoMPG_test_csv$carName) 
summary(autoMPG_test_csv$carName)
autoMPG_test_csv$carName
# cylinders column should be factors (multi-valued discrete) not integer
autoMPG_test_csv$cylinders <- as.factor(autoMPG_test_csv$cylinders) 
# horsepower is character and it should be continuous numeric variable
autoMPG_test_csv$horsepower <- as.numeric(as.character(autoMPG_test_csv$horsepower)) # Obs: NAs introduced by coercion for "?" values
autoMPG_test_csv$horsepower[is.na(autoMPG_test_csv$horsepower)] = mean(autoMPG_test_csv$horsepower,na.rm = T)
# converting modelYear to factor
autoMPG_test_csv$modelYear <- as.factor(autoMPG_test_csv$modelYear) 
# converting origin to factor since it has only 3 levels
autoMPG_test_csv$origin <- as.factor(autoMPG_test_csv$origin) 
str(autoMPG_test_csv)



################## Exploratory Data Analysis #######################################
# checking for outliers by histogram representation of variables
ggplot(autoMPG_train_csv, aes(mpg, fill=cylinders)) +
    geom_histogram(color="black")
ggplot(autoMPG_train_csv, aes(mpg,fill=cylinders))+geom_histogram(binwidth=1)

ggplot(autoMPG_train_csv, aes(x=mpg)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 

ggplot(autoMPG_train_csv, aes(x=displacement)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")

ggplot(autoMPG_train_csv, aes(x=horsepower)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 

ggplot(autoMPG_train_csv, aes(x=weight)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")

ggplot(autoMPG_train_csv, aes(x=acceleration)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 

# Obs: Accelration data is normaly distributed. Rest are right skewed.
# Checking for outliers
ggplot(autoMPG_train_csv, aes(modelYear, mpg, color=cylinders)) +
    geom_boxplot()

ggplot(autoMPG_train_csv, aes(origin, mpg)) +
    geom_boxplot()

ggplot(autoMPG_train_csv, aes(origin, weight)) +
    geom_boxplot()

ggplot(autoMPG_train_csv, aes(cylinders, weight, fill=cylinders)) +
    geom_boxplot()

ggplot(autoMPG_train_csv, aes(x=cylinders, y=mpg, color=cylinders))+
    geom_boxplot(outlier.color = "blue")

# Scatterplot
# Miles per gallon (mpg) decreasing with increase of the weight
ggplot(autoMPG_train_csv,aes(weight, mpg)) +
    geom_point()+
    geom_smooth(method=lm) 
ggplot(autoMPG_train_csv, aes(x=mpg, y= weight))+geom_point()+facet_grid(cylinders~.)

ggplot(autoMPG_train_csv,aes(cylinders,mpg)) +
    geom_point()+
    geom_smooth(method=lm)  

ggplot(autoMPG_train_csv,aes(displacement,mpg)) +
    geom_point()+
    geom_smooth(method=lm)  

ggplot(autoMPG_train_csv,aes(weight, displacement)) +
    geom_point(color="red") +
    geom_smooth(method = lm)

ggplot(autoMPG_train_csv, aes(x=horsepower,y=mpg)) + geom_point()+facet_grid(cylinders~.)
ggplot(autoMPG_train_csv,aes(x=acceleration,y=mpg))+geom_point()

# Obs: Lets visualise some relationships between these data points.
newdata <- cor(autoMPG_train_csv[ , c('mpg', 'displacement', 'horsepower', 'weight', 'acceleration')], use='complete')
corrplot(newdata, method = "number") # Weight, Horsepower and Displacement are highly correlated
cor(autoMPG_train_csv[,c(2,5,6,7,8)]) 
heatmap(abs(cor(autoMPG_train_csv[-c(1,3,4,9,10)])), symm = T) 

# 6 and 8 cylinders cars are majorly built in origin 1
ggplot(autoMPG_train_csv, aes(cylinders,fill=origin)) +
    geom_bar(position = "dodge")

ggplot(autoMPG_train_csv, aes(cylinders,fill=origin)) +
    geom_bar(position = "stack")

ggplot(autoMPG_train_csv, aes(modelYear, y = weight, color=origin)) +
    geom_boxplot() +
    facet_wrap(~ origin) +
    xlab('Model Year') +
    ylab('Weight') +
    ggtitle('Car Weights Distributions Over Time by Region of Origin')


# We can see that over the year there was increase in the milege of the cars (Miles Per Gallon)
ggplot(autoMPG_train_csv, aes(modelYear, mpg, group=1))+geom_smooth()
# Significant drop in Car Engine's horsepower over the years
ggplot(autoMPG_train_csv, aes(modelYear, horsepower, group=1))+geom_smooth()



###################### Linear Regression Model #################################
#  - Weight is more significant among other features and it was highly correlated to Target variable "mpg"
str(autoMPG_train_csv)
summary(autoMPG_train_csv)
glimpse(autoMPG_train_csv)
# Linear Regression. Creating the Linear Model with significant features
model.lm.full <- lm(mpg ~ ., data = autoMPG_train_csv)
summary(model.lm.full) # Error "not defined because of singularities" will occur due to strong correlation between your independent variables. 
                    # This can be avoided by having n-1 dummy variables.

# Creating the Linear Model with significant features
model.lm.final <- lm(mpg ~ displacement+horsepower+weight+acceleration+modelYear+origin, data = autoMPG_train_csv)
summary(model.lm.final)
# Plots for the linear model
plot(model.lm.final)
visreg(model.lm.final)
# Stepwise regression for "model.lm.final" model
model.lm.final.forward <- step(model.lm.final, direction = "forward")
model.lm.final.backward <- step(model.lm.final, direction = "backward")
model.lm.final.both <- step(model.lm.final, direction = "both")
anova(model.lm.final.forward, model.lm.final.backward, model.lm.final.both)
step <- stepAIC(model.lm.final, direction="both",steps = 250)
step$anova # display results


# Prediction
predictions.lm.final <- predict(model.lm.final, newdata = autoMPG_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.lm.final))))
predictions.lm.final



############# Model performance metrics #########################
# Residual:
# - It is difference between predicted and actual values and mathematically, it can be represented  as Residual = yi - yi for ith data point.

# MSE: (Mean Squared Error); Compare all obtained models by calculating the MSE for the test data.
MSE <- function(y, y.hat) {
    mean((y - y.hat)^2)
}
#mse(data.test$Salary, predict(model.lm.full, newdata = data.test))
#print("R^2 Score :")
#print(R2_Score(y_pred = predictions ,y_true = bikeSharing_train_day_year$cnt))
#print("RMSE: ")
#print(RMSE(y_pred = predictions ,y_true = bikeSharing_train_day_year$cnt))

# RMSE - Root mean squared error
RMSE_1 = function(m, o){ # m is for model (fitted) values; o is for observed (true) values;
    sqrt(mean((m - o)^2))
}


# Function for Root Mean Squared Error
RMSE <- function(error) { 
    sqrt(mean(error^2)) 
}

RMSE(model.lm.full$residuals)
RMSE(model.lm.final$residuals)
RMSE(model.lm.final.both$residuals)

#Prediction on Train
#autoMPG_train_csv$pred_mpg <- predict(model.lm.final, newdata = autoMPG_train_csv)
#glimpse(autoMPG_train_csv)
#RMSE of Train
#RMSE.Train <- mean((autoMPG_train_csv$mpg - autoMPG_train_csv$pred_mpg)**2) %>%
#    sqrt()
#RMSE.Train

#Prediction on Test Set
#autoMPG_test_csv$pred_mpg <- predict(model.lm.final, newdata = autoMPG_test_csv)
#glimpse(autoMPG_test_csv)
#RMSE of Test Set
#RMSE.Test <- mean((autoMPG_test_csv$mpg - autoMPG_test_csv$pred_mpg)**2) %>%
#    sqrt()
#RMSE.Test


# RMS: (Root Mean Squared)

# If you want, say, MAE, you can do the following:
# Function for Mean Absolute Error
MAE <- function(error) { 
    mean(abs(error)) 
}

MAE(model.lm.final$residuals)

# Save the results
result.lm.final <- cbind(autoMPG_test_csv$id, as.character(predictions.lm.final))
colnames(result.lm.final) <- c("id", "mpg")
class(result.lm.final) # In R matrices are an extension of the numeric or character vectors
dim(result.lm.final)
str(result.lm.final)
write.table(result.lm.final, file="autoMPG_LM.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)





# use CV to train models
train_control <- trainControl(
    method="cv", 
    number=10,  
    verboseIter = TRUE
    # allowParallel = TRUE # FALSE for reproducible results 
)

# Fit lm model using 10-fold CV: 
formula.model.lm.full <- as.formula("mpg ~ .")
model.lm.full.cv <- train(
    formula.model.lm.full, 
    data = autoMPG_train_csv, 
    trControl = train_control, 
    method = 'lm'
    )
model.lm.full.cv$results
model.lm.full.cv$finalModel
plot(autoMPG_train_csv$mpg, predict(model.lm.full.cv$finalModel))
abline(c(0,1), col="red")

formula.model.lm.final <- as.formula("mpg ~ displacement + horsepower + weight + acceleration + modelYear + origin")
model.lm.final.cv <- train(
    formula.model.lm.final, 
    data = autoMPG_train_csv, 
    trControl = train_control, 
    method = 'lm'
    )
model.lm.final.cv$results
model.lm.final.cv$finalModel
plot(autoMPG_train_csv$mpg, predict(model.lm.final.cv$finalModel))
abline(c(0,1), col="red")

# Prediction
predictions.lm.final.cv <- predict(model.lm.final.cv, newdata = autoMPG_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.lm.final.cv))))
predictions.lm.final.cv

# Save the results
result.lm.final.cv <- cbind(autoMPG_test_csv$id, as.character(predictions.lm.final.cv))
colnames(result.lm.final.cv) <- c("id", "mpg")
class(result.lm.final.cv) # In R matrices are an extension of the numeric or character vectors
dim(result.lm.final.cv)
str(result.lm.final.cv)
write.table(result.lm.final.cv, file="autoMPG_LM_CV.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Robust regression #######################################
formula.rob <- as.formula("mpg ~ displacement + horsepower + weight + acceleration + modelYear + origin")
model_rob <- train(
    formula.rob, 
    data = autoMPG_train_csv, 
    trControl = train_control, 
    method = 'rlm',  
    #preProcess = c("center", "scale")
)
model_rob$results

plot(autoMPG_train_csv$mpg, predict(model_rob$finalModel), pch = model_rob$finalModel$weights)
abline(c(0,1), col="red")

# Prediction
predictions.rob <- predict(model_rob, newdata = autoMPG_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.rob))))
predictions.rob

# Save the results
result.rob <- cbind(autoMPG_test_csv$id, as.character(predictions.rob))
colnames(result.rob) <- c("id", "mpg")
class(result.rob) # In R matrices are an extension of the numeric or character vectors
dim(result.rob)
str(result.rob)
write.table(result.rob, file="autoMPG_ROB.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Random forrest    #######################################
grid_rf <- expand.grid(
    mtry = c(1, 10, 20, 50, 100)
)

formula.rf <- as.formula("mpg ~ displacement + horsepower + weight + acceleration + modelYear + origin")
model_rf <- train(
    formula.rf,
    data = autoMPG_train_csv,
    trControl = train_control,
    tuneGrid = grid_rf,
    verbose = TRUE,
    method="rf"
)

plot(model_rf$results$mtry, model_rf$results$Rsquared, xlab="mtry", ylab="r²", log = "x", type = "b")

plot(autoMPG_train_csv$mpg, predict(model_rf$finalModel), xlab="y", ylab="y-hat")
abline(c(0,1), col="red")

# Prediction
predictions.rf <- predict(model_rf, newdata = autoMPG_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.rf))))
predictions.rf

# Save the results
result.rf <- cbind(autoMPG_test_csv$id, as.character(predictions.rf))
colnames(result.rf) <- c("id", "mpg")
class(result.rf) # In R matrices are an extension of the numeric or character vectors
dim(result.rf)
str(result.rf)
write.table(result.rf, file="autoMPG_RF.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Lasso Regression    ######################################
#getModelInfo("glmnet")$glmnet$parameters
#parameter   class                    label
#1     alpha numeric        Mixing Percentage
#2    lambda numeric Regularization Parameter
grid_lasso <- expand.grid(
    alpha = c(0, 0.25, 0.5, 0.75, 1),
    lambda = c(0, 0.25, 0.5, 0.75, 1)
)

formula.lasso <- as.formula("mpg ~ displacement + horsepower + weight + acceleration + modelYear + origin")
model_lasso <- train(
    formula.lasso,
    data = autoMPG_train_csv,
    trControl = train_control,
    tuneGrid = grid_lasso,
    method="glmnet"
)

model_lasso$results

formula.lasso.full <- as.formula("mpg ~ .")
model_lasso.full <- train(
    formula.lasso.full,
    data = autoMPG_train_csv,
    trControl = train_control,
    tuneGrid = grid_lasso,
    method="glmnet"
)

model_lasso.full$results

# Prediction
predictions.lasso <- predict(model_lasso, newdata = autoMPG_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.lasso))))
predictions.lasso

# Save the results
result.lasso <- cbind(autoMPG_test_csv$id, as.character(predictions.lasso))
colnames(result.lasso) <- c("id", "mpg")
class(result.lasso) # In R matrices are an extension of the numeric or character vectors
dim(result.lasso)
str(result.lasso)
write.table(result.lasso, file="autoMPG_LASSO.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Regression with KNN #######################################
grid_knn <- expand.grid(
    k = 2:100
)

formula.knn <- as.formula("mpg ~ displacement + horsepower + weight + acceleration + modelYear + origin")
model_knn <- train(
    formula.knn,
    data = autoMPG_train_csv,
    trControl = train_control,
    tuneGrid = grid_knn,
    method="knn"
)

model_knn$results
plot(model_knn$results$k, model_knn$results$Rsquared, xlab="k", ylab="r²", type = "l")

# Prediction
predictions.knn <- predict(model_knn, newdata = autoMPG_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.knn))))
predictions.knn

# Save the results
result.knn <- cbind(autoMPG_test_csv$id, as.character(predictions.knn))
colnames(result.knn) <- c("id", "mpg")
class(result.knn) # In R matrices are an extension of the numeric or character vectors
dim(result.knn)
str(result.knn)
write.table(result.knn, file="autoMPG_KNN.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)
