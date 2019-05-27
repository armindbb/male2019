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
library(RColorBrewer)
library(relaimpo)



# Make sure results are reproducible
set.seed(123)

# Load train.csv data
path_train_bikeSharing <-  "2/data/bikesharing/bikeSharing.shuf.train.csv"
bikeSharing_train_csv_original <- read.csv(path_train_bikeSharing, stringsAsFactors = FALSE)
bikeSharing_train_csv <- bikeSharing_train_csv_original
class(bikeSharing_train_csv)
str(bikeSharing_train_csv)
glimpse(bikeSharing_train_csv)
summary(bikeSharing_train_csv) # Find out more about each category of each attribute
names(bikeSharing_train_csv)

# Load test.csv data
path_test_bikeSharing <-  "2/data/bikesharing/bikeSharing.shuf.test.csv"
bikeSharing_test_csv_original <- read.csv(path_test_bikeSharing, stringsAsFactors = FALSE)
bikeSharing_test_csv <- bikeSharing_test_csv_original
class(bikeSharing_test_csv)
str(bikeSharing_test_csv) # no cnt variable
glimpse(bikeSharing_test_csv)
summary(bikeSharing_test_csv) # Find out more about each category of each attribute
names(bikeSharing_test_csv)

# Check observations with missing values in train dataset
print(paste("The total number of missing data are",sum(is.na(bikeSharing_train_csv))))
pattern <- "/|:|\\?|<|>|\\|\\\\|\\*"
grepl(pattern, bikeSharing_train_csv)
# Check observations with missing values in test dataset
print(paste("The total number of missing data are",sum(is.na(bikeSharing_test_csv))))
pattern <- "/|:|\\?|<|>|\\|\\\\|\\*"
grepl(pattern, bikeSharing_train_csv)


######################################### Quick view of data ########################
head(bikeSharing_train_csv)
describe(bikeSharing_train_csv)
# id        : int  
# cnt       : int                                     count of total rental bikes including both casual and registered/Target variable (cnt) is not normalized
# dteday    : chr  "2012-06-14"                       date
is.data.table(bikeSharing_train_csv)
DT <- data.table(bikeSharing_train_csv) 
str(DT)
DT[, dteday:=as.Date(dteday)]
print(paste("Time frame of the data is from", min(DT$dteday), max(DT$dteday)))
# season    : int  2 3 2 4 3 2 4 2 2 3 ...            season (1:springer, 2:summer, 3:fall, 4:winter)                       --> factor
# yr        : int  1 1 1 1 1 0 0 1 0 1 ...            year (0: 2011, 1:2012)                                                --> factor
# mnth      : int  6 6 5 10 9 4 12 4 4 8 ...          month ( 1 to 12)                                                      --> factor                                
# hr        : int  5 19 19 3 1 10 15 12 2 6 ...       hour (0 to 23)                                                        --> factor
# holiday   : int  0 0 0 0 0 0 0 0 0 0 ...            weather day is holiday or not                                         --> factor
# weekday   : int  4 5 1 0 3 3 5 1 1 6 ...            day of the week                                                       --> factor
# workingday: int  1 1 1 0 1 1 1 1 1 0 ...            if day is neither weekend nor holiday is 1, otherwise is 0.           --> factor
# weathersit: int  2 2 1 2 2 2 1 1 1 2 ...            1: Clear... 2: Mist...  3: Light Snow... 4: Heavy Rain                --> factor
# temp      : num  0.6 0.76 0.66 0.5 ...              Normalized temperature in Celsius.
summary(bikeSharing_train_csv$temp)
# atemp     : num  0.621 0.697 0.621 ...              Normalized feeling temperature in Celsius.
summary(bikeSharing_train_csv$atemp)
# hum       : num  0.56 0.55 0.69 ...                 Normalized humidity. 
summary(bikeSharing_train_csv$hum)
# windspeed : num  0.2537 0.5224 ...                  Normalized wind speed. 
summary(bikeSharing_train_csv$windspeed)


########################### Pre-processing for train.csv ##########################
### Data Cleaning
# Assigning factors to the Categorical variables
bikeSharing_train_csv$dteday <- as.Date(bikeSharing_train_csv$dteday)
bikeSharing_train_csv$season <- factor(bikeSharing_train_csv$season)
bikeSharing_train_csv$yr <- factor(bikeSharing_train_csv$yr)
#bikeSharing_train_csv$mnth <- factor(bikeSharing_train_csv$mnth)
bikeSharing_train_csv$hr <- as.numeric(bikeSharing_train_csv$hr)
bikeSharing_train_csv$holiday <- factor(bikeSharing_train_csv$holiday)
#bikeSharing_train_csv$weekday <- factor(bikeSharing_train_csv$weekday)
#bikeSharing_train_csv$workingday <- factor(bikeSharing_train_csv$workingday)
bikeSharing_train_csv$weathersit <- factor(bikeSharing_train_csv$weathersit)
str(bikeSharing_train_csv)
# denormalize the actual values, since the normalized values were very low and factorized the categorical attributes.
bikeSharing_train_csv$temp <- bikeSharing_train_csv$temp*41
bikeSharing_train_csv$atemp <- bikeSharing_train_csv$atemp*50
bikeSharing_train_csv$windspeed <- bikeSharing_train_csv$windspeed*67
bikeSharing_train_csv$hum <- bikeSharing_train_csv$hum*100
#bikeSharing_train_csv$mean_acttemp_feeltemp <- (bikeSharing_train_csv$actual_temp+bikeSharing_train_csv$actual_feel_temp)/2
str(bikeSharing_train_csv)

########################### Pre-processing for test.csv ###########################
# Assigning factors to the Categorical variables
bikeSharing_test_csv$dteday <- as.Date(bikeSharing_test_csv$dteday)
bikeSharing_test_csv$season <- factor(bikeSharing_test_csv$season)
bikeSharing_test_csv$yr <- factor(bikeSharing_test_csv$yr)
#bikeSharing_test_csv$mnth <- factor(bikeSharing_test_csv$mnth)
bikeSharing_test_csv$hr <- as.numeric(bikeSharing_test_csv$hr)
bikeSharing_test_csv$holiday <- factor(bikeSharing_test_csv$holiday)
#bikeSharing_test_csv$weekday <- factor(bikeSharing_test_csv$weekday)
#bikeSharing_test_csv$workingday <- factor(bikeSharing_test_csv$workingday)
bikeSharing_test_csv$weathersit <- factor(bikeSharing_test_csv$weathersit)
# denormalize the actual values, since the normalized values were very low and factorized the categorical attributes.
bikeSharing_test_csv$temp <- bikeSharing_test_csv$temp*41
bikeSharing_test_csv$atemp <- bikeSharing_test_csv$atemp*50
bikeSharing_test_csv$windspeed <- bikeSharing_test_csv$windspeed*67
bikeSharing_test_csv$hum <- bikeSharing_test_csv$hum*100
#bikeSharing_test_csv$mean_acttemp_feeltemp <- (bikeSharing_test_csv$actual_temp+bikeSharing_test_csv$actual_feel_temp)/2
str(bikeSharing_test_csv)



################## Exploratory Data Analysis #################
ggplot(bikeSharing_train_csv,aes(yr,cnt)) +
    geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
    theme_classic() +
    labs(title = "Boxplot of rental bikes per year") +
    scale_x_discrete(labels = c("2011","2012"))

col <- brewer.pal(4,"Set3")
ggplot(bikeSharing_train_csv,aes(season,cnt)) +
    geom_boxplot(fill = col) +
    theme_classic() +
    labs(title = "Boxplot of rental bikes per season") +
    scale_x_discrete(labels = c("Spring","Summar","Fall","Winter"))

#col <- brewer.pal(12,"Set3")
#ggplot(bikeSharing_train_csv,aes(mnth,cnt)) +
#    geom_boxplot(fill = col) +
#    theme_classic() +
#    labs(title = "Boxplot of rental bikes per month")

ggplot(bikeSharing_train_csv,aes(holiday,cnt)) +
    geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
    theme_classic() +
    labs(title = "Boxplot of rental bikes by holiday") +
    scale_x_discrete(labels = c("no","yes"))

#col <- brewer.pal(7,"Set3")
#ggplot(bikeSharing_train_csv,aes(weekday,cnt)) +
#    geom_boxplot(fill = col) +
#    theme_classic() +
#    labs(title = "Boxplot of rental bikes by weekday")

#ggplot(bikeSharing_train_csv,aes(workingday,cnt)) +
#    geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
#    theme_classic() +
#    labs(title = "Boxplot of rental bikes by workingday") +
#    scale_x_discrete(labels = c("no","yes"))

col <- brewer.pal(4,"Set3")
ggplot(bikeSharing_train_csv,aes(weathersit,cnt)) +
    geom_boxplot(fill = col) +
    theme_classic() +
    labs(title = "Boxplot of rental bikes by weathersit") +
    scale_x_discrete(labels = c("Clear", "Mist", "Light Snow", "Heavy Rain"))

ggplot(bikeSharing_train_csv,aes(cnt,temp)) +
    geom_point() +
    geom_smooth(method = "loess") +
    theme_classic()

ggplot(bikeSharing_train_csv, aes(temp, cnt)) + 
    geom_point(aes(color = temp), alpha = 0.5) + 
    scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + 
    ggtitle("Temperature vs Count")

ggplot(bikeSharing_train_csv, aes(dteday, cnt)) + 
    geom_point(aes(color = temp)) + 
    scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + 
    ggtitle("Date vs Count")

# Generating boxplots for the categorical variables
#par("mar")
#par(mfrow = c(4,2))
boxplot(formula = cnt~season, data = bikeSharing_train_csv,main="Boxplot by Season",col='steelblue')
boxplot(formula = cnt~yr, data = bikeSharing_train_csv,main="Boxplot by Year",col='steelblue')
boxplot(formula = cnt~mnth, data = bikeSharing_train_csv,main="Boxplot by Month",col='steelblue')
boxplot(formula = cnt~hr, data = bikeSharing_train_csv,main="Boxplot by Hour",col='steelblue')
boxplot(formula = cnt~holiday, data = bikeSharing_train_csv,main="Boxplot by Holiday",col='steelblue')
boxplot(formula = cnt~weekday, data = bikeSharing_train_csv,main="Boxplot by Weekday",col='steelblue')
boxplot(formula = cnt~workingday, data = bikeSharing_train_csv,main="Boxplot by Working Day",col='steelblue')
boxplot(formula = cnt~weathersit, data = bikeSharing_train_csv,main="Boxplot by Weather Situation",col='steelblue')

# Histograms for numeric variables
par(mfcol = c(3,2))
hist(bikeSharing_train_csv$cnt,main="Histogram of Demand",xlab = "Demand",col="orange")
box()
hist(bikeSharing_train_csv$temp,main="Histogram of Temperature",xlab = "Temp",col="steelblue")
box()
hist(bikeSharing_train_csv$atemp,main="Histogram of Feeling Temperature",xlab = "Temp",col="steelblue")
box()
hist(bikeSharing_train_csv$hum,main="Histogram of Humidity",xlab = "Hum",col="steelblue")
box()
hist(bikeSharing_train_csv$windspeed,main="Histogram of Wind Speed",xlab = "Wind Speed",col="steelblue")
box()

# Generating a correlation (Pearson) matrix for the numeric variables)
cor(bikeSharing_train_csv[,c(2,12,13,14,15)]) # temp and atemp variables are almost the same, these are very similar
#scatterplotMatrix(bikeSharing_train_csv[,c(2,12,13,14,15)],col="lightblue")
heatmap(abs(cor(bikeSharing_train_csv[,c(2,12,13,14,15)])), symm = T)
cor(bikeSharing_train_csv$temp, bikeSharing_train_csv$cnt)



###################### Linear Regression Model #################################
# Full model
model.full <- lm(cnt ~., data = bikeSharing_train_csv)
model.full_nor <- lm(log(cnt) ~ ., data = bikeSharing_train_csv) # Normalized cnt
summary(model.full)
summary(model.full_nor)
length(model.full_nor$coefficients) > model.full_nor$rank
# Function for Root Mean Squared Error
RMSE <- function(error) { 
    sqrt(mean(error^2)) 
}

RMSE(model.full_nor$residuals)

# Prediction
predictions.lm.full.nor <- predict(model.full_nor, newdata = bikeSharing_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.lm.full.nor))))
predictions.lm.full.nor

# Save the results
result.full.nor <- cbind(bikeSharing_test_csv$id, as.character(predictions.lm.full.nor))
colnames(result.full.nor) <- c("id", "cnt")
write.table(result.full.nor, file="bikeSharing_LM_NOR.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)


# Stepwise regression
model.full.nor.forward <- step(model.full_nor, direction = "forward")
model.full.nor.backward <- step(model.full_nor, direction = "backward")
model.full.nor.both <- step(model.full_nor, direction = "both")
anova(model.full.nor.forward, model.full.nor.backward, model.full.nor.both)
step <- stepAIC(model.full, direction="both",steps = 250)
step$anova # display results

# Prediction
predictions.model.both <- predict(model.full.nor.both, newdata = bikeSharing_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.model.both))))
predictions.model.both

# Save the results
result.model.nor.both <- cbind(bikeSharing_test_csv$id, as.character(predictions.model.both))
colnames(result.model.nor.both) <- c("id", "cnt")
write.table(result.model.nor.both, file="bikeSharing_SR_both.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



str(bikeSharing_train_csv_original)
str(bikeSharing_test_csv_original)
str(bikeSharing_train_csv)
str(bikeSharing_test_csv)
# use CV to train models
train_control <- trainControl(
    method="cv", 
    number=10,  
    verboseIter = TRUE
    # allowParallel = TRUE # FALSE for reproducible results 
)

# Fit lm model using 10-fold CV: 
formula.model.lm.full <- as.formula("cnt ~ .")
model.lm.full.cv <- train(
    formula.model.lm.full, 
    data = bikeSharing_train_csv, 
    trControl = train_control, 
    method = 'lm'
)
model.lm.full.cv$results
model.lm.full.cv$finalModel
par(mfcol = c(1,1))
plot(bikeSharing_train_csv$cnt, predict(model.lm.full.cv$finalModel))
abline(c(0,1), col="red")

# Prediction
predictions.lm.final.cv <- predict(model.lm.full.cv, newdata = bikeSharing_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.lm.final.cv))))
predictions.lm.final.cv

# Save the results
result.lm.final.cv <- cbind(bikeSharing_test_csv$id, as.character(predictions.lm.final.cv))
colnames(result.lm.final.cv) <- c("id", "cnt")
class(result.lm.final.cv) # In R matrices are an extension of the numeric or character vectors
dim(result.lm.final.cv)
str(result.lm.final.cv)
write.table(result.lm.final.cv, file="bikeSharing_LM_CV.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Robust regression #######################################
formula.rob <- as.formula("cnt ~ .")
model_rob <- train(
    formula.rob, 
    data = bikeSharing_train_csv, 
    trControl = train_control, 
    method = 'rlm',  
    preProcess = c("center", "scale")
)
model_rob$results
plot(bikeSharing_train_csv$cnt, predict(model_rob$finalModel), pch = model_rob$finalModel$weights)
abline(c(0,1), col="red")

# Prediction
predictions.rob <- predict(model_rob, newdata = bikeSharing_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.rob))))
predictions.rob

# Save the results
result.rob <- cbind(bikeSharing_test_csv$id, as.character(predictions.rob))
colnames(result.rob) <- c("id", "cnt")
class(result.rob) # In R matrices are an extension of the numeric or character vectors
dim(result.rob)
str(result.rob)
write.table(result.rob, file="bikeSharing_ROB.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Random forrest    #######################################
train_control_rf <- trainControl(
    method="cv", 
    number=10,  
    verboseIter = TRUE
    # allowParallel = TRUE # FALSE for reproducible results 
)
grid_rf <- expand.grid(
    #mtry = c(1, 10, 20, 50, 100)
    mtry = c(1,50)
)

formula.rf <- as.formula("cnt ~ .")
model_rf <- train(
    formula.rf,
    data = bikeSharing_train_csv,
    trControl = train_control_rf,
    tuneGrid = grid_rf,
    verbose = TRUE,
    method="rf"
)

model_rf$results
plot(model_rf$results$mtry, model_rf$results$Rsquared, xlab="mtry", ylab="r²", log = "x", type = "b")
plot(bikeSharing_train_csv$cnt, predict(model_rf$finalModel), xlab="y", ylab="y-hat")
abline(c(0,1), col="red")

# Prediction
predictions.rf <- predict(model_rf, newdata = bikeSharing_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.rf))))
predictions.rf

# Save the results
result.rf <- cbind(bikeSharing_test_csv$id, as.character(predictions.rf))
colnames(result.rf) <- c("id", "cnt")
class(result.rf) # In R matrices are an extension of the numeric or character vectors
dim(result.rf)
str(result.rf)
write.table(result.rf, file="bikeSharing_RF.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Lasso Regression    ######################################
#getModelInfo("glmnet")$glmnet$parameters
#parameter   class                    label
#1     alpha numeric        Mixing Percentage
#2    lambda numeric Regularization Parameter
grid_lasso <- expand.grid(
    alpha = c(0, 0.25, 0.5, 0.75, 1),
    lambda = c(0, 0.25, 0.5, 0.75, 1)
)

formula.lasso <- as.formula("cnt ~ .")
model_lasso <- train(
    formula.lasso,
    data = bikeSharing_train_csv,
    trControl = train_control,
    tuneGrid = grid_lasso,
    method="glmnet"
)

model_lasso$results

# Prediction
predictions.lasso <- predict(model_lasso, newdata = bikeSharing_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.lasso))))
predictions.lasso

# Save the results
result.lasso <- cbind(bikeSharing_test_csv$id, as.character(predictions.lasso))
colnames(result.lasso) <- c("id", "cnt")
class(result.lasso) # In R matrices are an extension of the numeric or character vectors
dim(result.lasso)
str(result.lasso)
write.table(result.lasso, file="bikeSharing_LASSO.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)



###################### Regression with KNN #######################################
grid_knn <- expand.grid(
    k = 2:100
)

formula.knn <- as.formula("cnt ~ .")
model_knn <- train(
    formula.knn,
    data = bikeSharing_train_csv,
    trControl = train_control,
    tuneGrid = grid_knn,
    method="knn"
)

model_knn$results
plot(model_knn$results$k, model_knn$results$Rsquared, xlab="k", ylab="r²", type = "l")

# Prediction
predictions.knn <- predict(model_knn, newdata = bikeSharing_test_csv)
print(paste("The total number of missing prediction: ",sum(is.na(predictions.knn))))
predictions.knn

# Save the results
result.knn <- cbind(bikeSharing_test_csv$id, as.character(predictions.knn))
colnames(result.knn) <- c("id", "mpg")
class(result.knn) # In R matrices are an extension of the numeric or character vectors
dim(result.knn)
str(result.knn)
write.table(result.knn, file="bikeSharing_KNN.csv", col.names = TRUE, row.names = FALSE, sep = ",",quote=FALSE)
