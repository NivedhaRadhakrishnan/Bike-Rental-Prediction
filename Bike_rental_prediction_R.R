########################Bike Rental Count Prediction R Code#############################
#Cleaning the environment
rm(list = ls())

#Setting the working directory
setwd("/Users/nivedharakigmail.com/Desktop/Edwisor/Project 2")

#Load libraries
libraries = c("psych","tidyverse","ggpubr","DMwR2","corrplot","usdm","caret","rpart","rpart.plot","randomForest")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Importing the csv file
bike_rental_data= read.csv(file = "BikeRentalDataset.csv", header = T)

########################Exploratory data analysis###############################
#Observing sample data
head(bike_rental_data)

#Observing structure of the data
str(bike_rental_data)

#Dropping instant and dteday as they are non significant/redundent for the prediction
bike_rental_data = subset(bike_rental_data, select = -(which(names(bike_rental_data) %in% c("instant","dteday"))))

##Sorting the variables into numerical and categorical
categorical_variable_set = c("season","yr","mnth","holiday","weekday","workingday","weathersit")
numeric_variable_set = c("temp","atemp","hum","windspeed","casual","registered","cnt")

#Parsing the datatype of categorical variables and assigning levels
for(i in categorical_variable_set){
  bike_rental_data[,i] = as.factor(bike_rental_data[,i])
}

#verifying the structure of the categorical variables
str(bike_rental_data)

#Defining function to store categorical data and numeric data
fetch_data = function(data_set,typename){
  #Get the data with only numeric columns
  if (typename == "numeric"){
    numeric_index = sapply(data_set, is.numeric)
    numeric_data = data_set[,numeric_index]
  }
  #Get the data with only factor columns
  else{
    numeric_index = sapply(data_set, is.numeric)
    factor_data = bike_rental_data[,!numeric_index]
  }
}

#Calling function fetch_data to store numeric and cateogrical data separately
numeric_data = fetch_data(bike_rental_data,"numeric")
factor_data = fetch_data(bike_rental_data,"factor")

##########################Missing value Analysis################################
###------------------------Observation---------------------------------------###
#Count of missing values in each variable
missing_values_df = data.frame(apply(bike_rental_data,2,function(x){sum(is.na(x))}))

#Creating a new column for missing value percentage
names(missing_values_df)[1] = "NA_count"
missing_values_df$NA_Percent = (missing_values_df$NA_count/nrow(bike_rental_data))*100

#Sorting missing values in decreasing order
missing_values_df = missing_values_df[order(-missing_values_df$NA_Percent),]
missing_values_df

##No missing values found in any of the variable, hence imputaion is not required

##########################Analysis of data distribution using histogram#########

#Multiple histograms for numerical predictors
bike_rental_data_plot_hist = numeric_data[1:7]
multi.hist(bike_rental_data_plot_hist,dcol= c("blue","red"),dlty=c("solid", "solid"),bcol="linen")
#The predictor "casual" is skewed to the left. 
#The rest of the predictors are normally distributed.

#Multiple scatterplot for numerical predictors
scat1 = ggplot(data = bike_rental_data_plot_hist, aes(x =temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike Count")
scat2 = ggplot(data = bike_rental_data_plot_hist, aes(x =atemp, y = cnt)) + ggtitle("Distribution of Actual Temperature") + geom_point() + xlab("Actual Temperature") + ylab("Bike Count")
scat3 = ggplot(data = bike_rental_data_plot_hist, aes(x =hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point() + xlab("Humidity") + ylab("Bike Count")
scat4 = ggplot(data = bike_rental_data_plot_hist, aes(x =windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point() + xlab("Windspeed") + ylab("Bike Count")
scat5 = ggplot(data = bike_rental_data_plot_hist, aes(x =casual, y = cnt)) + ggtitle("Distribution of count of casual users") + geom_point() + xlab("Casual Users count") + ylab("Bike Count")
scat6 = ggplot(data = bike_rental_data_plot_hist, aes(x =registered, y = cnt)) + ggtitle("Distribution of count of registered users") + geom_point() + xlab("Registered Users count ") + ylab("Bike Count")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,scat5,scat6,ncol=2)
#The predictor "casual" is skewed to the left. 
#The rest of the predictors are normally distributed.

#Multiple barplot
#Bar plot for categorically predictors
bike_rental_data_plot_bar = factor_data[1:7]
gplot1 = ggplot(bike_rental_data_plot_bar, aes(x = season ) )+ geom_bar()
gplot2 = ggplot(bike_rental_data_plot_bar, aes(x = yr ) )+ geom_bar()
gplot3 = ggplot(bike_rental_data_plot_bar, aes(x = mnth ) )+ geom_bar()
gplot4 = ggplot(bike_rental_data_plot_bar, aes(x = holiday ) )+ geom_bar()
gplot5 = ggplot(bike_rental_data_plot_bar, aes(x = weekday ) )+ geom_bar()
gplot6 = ggplot(bike_rental_data_plot_bar, aes(x = workingday ) )+ geom_bar()
gplot7 = ggplot(bike_rental_data_plot_bar, aes(x = weathersit ) )+ geom_bar()
ggarrange(gplot1,gplot2,gplot3,gplot4,gplot5,gplot6,gplot7)
#The count of bikes hired are evenly distributed across different level of the predictor except holiday,workingday and weathersit

##########################Outlier Analysis######################################
#Outlier boxplot
for(i in 1:ncol(numeric_data)) {
  assign(paste0("box_plot",i), ggplot(data = bike_rental_data, aes_string(y = numeric_data[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) +
           ggtitle(paste("Boxplot of: ",colnames(numeric_data[i]))))
}

#Arrange the plots in grids
gridExtra::grid.arrange(box_plot1,box_plot2,box_plot3,box_plot4,box_plot5,
                        box_plot6,box_plot7,ncol=3)
#Outliers are found in the predictors hum,windspeed and casual

#Replacing all outliers with NA
for(i in numeric_variable_set){
  val1 = bike_rental_data[,i][bike_rental_data[,i] %in% boxplot.stats(bike_rental_data[,i])$out]
  print(paste(i,length(val1)))
  bike_rental_data[,i][bike_rental_data[,i] %in% val1] = NA
}

#Checking for missing values
sum(is.na(bike_rental_data))

#Imputing the outliers using KNNimputation
bike_rental_data = knnImputation(bike_rental_data, k = 10)
# bike_rental_data$casual[149]

#Checking for missing values
sum(is.na(bike_rental_data))

#Parsing datatype of casual to int
bike_rental_data[,"casual"] = as.integer(bike_rental_data[,"casual"])
#Observing structure of the data
str(bike_rental_data)

#Since cnt is the total of casual and registered replacing the values of cnt with the sum of the imputed casual registered values.
bike_rental_data$cnt <- bike_rental_data$casual + bike_rental_data$registered

# #Verifying if there are any outliers present after the imputation
# #Calling function fetch_data to store numeric and cateogrical data separately
# numeric_data = fetch_data(bike_rental_data,"numeric")
# factor_data = fetch_data(bike_rental_data,"factor")
# 
# #Outlier boxplot
# for(i in 1:ncol(numeric_data)) {
#   assign(paste0("box_plot",i), ggplot(data = bike_rental_data, aes_string(y = numeric_data[,i])) +
#            stat_boxplot(geom = "errorbar", width = 0.5) +
#            geom_boxplot(outlier.colour = "red", fill = "blue", outlier.size = 1) +
#            labs(y = colnames(numeric_data[i])) +
#            ggtitle(paste("Boxplot of: ",colnames(numeric_data[i]))))
# }
# 
# #Arrange the plots in grids
# gridExtra::grid.arrange(box_plot1,box_plot2,box_plot3,box_plot4,box_plot5,
#                         box_plot6,box_plot7,ncol=3)

#########################Feature Selection######################################
##Numerical columns
#Creating correlation matrix
correlation_matrix = cor(bike_rental_data[,numeric_variable_set])
# dev.off()
corrplot(correlation_matrix,method = "number",type = 'lower')
#The correlation coefficient of temp and atemp pair and registered and cnt pair seems to be higher than the level of significance ie., 0.8, hence one predictor in each pair can be omitted.

#Checking multi-collinearity
#Using VIF technique for numerical data
vif(bike_rental_data[,numeric_variable_set])
#The VIF of casual, registered and cnt is found to be infinite, which shows the presence of multicollinearity. Hence registered column should be dropped.

#Using ANOVA technique for categorical data
for(i in categorical_variable_set){
  print(i)
  aov_summary = summary(aov(cnt~bike_rental_data[,i],data = bike_rental_data))
  print(aov_summary)
}
#The p-value of weekday is above the level of significance (ie., 0.05). weekday column can be dropped too.

#Dimentionality reduction
bike_rental_data_all_columns = bike_rental_data
bike_rental_data = subset(bike_rental_data, select = -(which(names(bike_rental_data) %in% c("registered","atemp","weekday"))))

#Checking multi-collinearity after dimentionality reduction
#Using VIF technique for numerical data
numeric_variable_set = c("temp","hum","windspeed","casual","cnt")
vif(bike_rental_data[,numeric_variable_set])
#Multicollinearity is not present among the predictors

###########################Feature Sampling#####################################
variable_set = c("season","yr","mnth","holiday","workingday","weathersit")
#Parsing all the columns to numeric value
for(i in variable_set){
  bike_rental_data[,i] = as.numeric(bike_rental_data[,i])
}

#verifying the structure of the variables
str(bike_rental_data)

#Separating dataset into test and train set
set.seed(123)
#Splitting the train and test set in 4:1 ratio (ie., 80% training data and 20% test data)
train_index = sample(1:nrow(bike_rental_data), 0.8*nrow(bike_rental_data))        
train = bike_rental_data[train_index,]
test = bike_rental_data[-train_index,]

############################## Model Development  ##############################
###--------------------Multiple linear regression----------------------------###
# RMSE     : 735.8215126
# Rsquared : 0.8598805        
# MAE      : 510.7905055
#Train the model using training data
# Fitting Multiple Linear Regression to the Training set
regressor_mlr = lm(formula = cnt ~ .,data = train)
# Predicting the Test set results
y_pred_mlr = predict(regressor_mlr, newdata = test)
#Get the summary of the model
#summary(regressor_mlr)
#Create dataframe for actual and predicted values
model_pred = data.frame("actual"=test, "model_pred"=y_pred_mlr)
#head(model_pred)

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = y_pred_mlr, obs =test$cnt))
#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="red")
lines(y_pred_mlr,col="blue")

#Predict a sample data
predict(regressor_mlr,test[4,])
#18
#1481.066
#####***************MLR Hyperparameter tuning with caret********************####
# RMSE     : 735.8215126
# Rsquared : 0.8598805       
# MAE      : 510.7905055
# Define the control
trControl <- trainControl(method = "cv", number = 3, search = "grid",
                          verboseIter = TRUE)

set.seed(123)
tuned_model_regression = caret::train(cnt ~ .,
                                data = train,
                                method = "lm",
                                metric = "MAE",
                                trControl = trControl)

predictions_tuned_model_regression = predict(tuned_model_regression,test)

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = predictions_tuned_model_regression, obs = test$cnt))

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="red")
lines(predictions_tuned_model_regression,col="blue")

#Predict a sample data
predict(tuned_model_regression,test[4,])
#18
#1481.066

###--------------------Decision Tree-----------------------------------------###
# RMSE     : 782.6174370
# Rsquared : 0.8460872         
# MAE      : 594.3689043
#Build decision tree using rpart
regressor_dt = rpart(cnt ~., data = train, method = "anova")

#Predict the test cases
y_pred_dt = predict(regressor_dt, newdata = test)

#Create data frame for actual and predicted values
model_pred = cbind(model_pred,y_pred_dt)
#head(model_pred)

#Get the summary of the model
#summary(regressor_dt)

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = y_pred_dt, obs = test$cnt))

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="red")
lines(y_pred_dt,col="blue")

# Visualize the decision tree with rpart.plot
rpart.plot(regressor_dt, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#Predict a sample data
predict(regressor_dt,test[4,])
#18
#1585.271

#####*****************DT Hyperparameter tuning with caret*******************####
# RMSE     : 716.9992125
# Rsquared : 0.8685536        
# MAE      : 544.8011413
rpart_grid = expand.grid(cp = c(0.01,0.001,0.02,0.3,0.3,0.3,0.001,0.3,0.3,0.001))

# Define the control
trControl <- trainControl(method = "cv", number = 3, search = "grid",
                          verboseIter = TRUE)

set.seed(123)
tuned_model_tree = caret::train(cnt ~ .,
                                data = train,
                                method = "rpart",
                                metric = "MAE",
                                trControl = trControl,
                                tuneGrid = rpart_grid)

predictions_tuned_model_tree = predict(tuned_model_tree,test)

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = predictions_tuned_model_tree, obs = test$cnt))

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="red")
lines(predictions_tuned_model_tree,col="blue")

#Predict a sample data
predict(tuned_model_tree,test[4,])
#18
#1416

###--------------------Random forest-----------------------------------------###
# RMSE     : 587.0125339
# Rsquared : 0.9204742         
# MAE      : 423.8855792
regressor_rf = randomForest(cnt~., data = train, ntrees = 500)

#Predict the test cases
y_pred_rf = predict(regressor_rf,test)

#Create dataframe for actual and predicted values
model_pred = cbind(model_pred,y_pred_rf)
#head(model_pred)

#Get the summary of the model
#summary(regressor_rf)

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = y_pred_rf, obs = test$cnt))

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="red")
lines(y_pred_rf,col="blue")

#Predict a sample data
predict(regressor_rf,test[4,])
#18
#1252.337

#####*****************RF Hyperparameter tuning with caret*******************####
##------------Commenting the code as computation time is long------- ###########
# # RMSE     : 561.6378007
# # Rsquared : 0.9241534        
# # MAE      : 396.1909000
# set.seed(123)
# # default ntrees = 500
# tuned_model_forest = caret::train(cnt~.,
#                                   data = train,
#                                   method = 'rf',
#                                   metric = 'MAE',
#                                   trControl = trainControl(method = "repeatedcv",
#                                                            number = 10,
#                                                            repeats = 3,
#                                                            verboseIter = TRUE
#                                   ),
#                                   tuneGrid = expand.grid(.mtry = c(1:10)),
#                                   ntree = 500
# )
# 
# 
# predictions_tuned_model_forest = predict(tuned_model_forest,test[,-11])
# 
# #Calcuate MAE, RMSE, R-squared for testing data 
# print(postResample(pred = predictions_tuned_model_forest, obs = test$cnt))
# 
# #Plot a graph for actual vs predicted values
# plot(test$cnt,type="l",lty=2,col="red")
# lines(predictions_tuned_model_forest,col="blue")
# 
# #Predict a sample data
# predict(tuned_model_forest,test[4,])
# #18
# #1182.849

##############K-Fold Cross validation###########################################
set.seed(123)
KFData = bike_rental_data
train_KF = KFData[train_index,]
test_KF = KFData[-train_index,]

###--------------------Random forest-----------------------------------------###
# RMSE     :574.9892684 
# Rsquared :0.9218349        
# MAE      :408.0147753
KF_RF = train(cnt~.,
               data = train_KF,
               method = "rf",
               tuneGrid = expand.grid(mtry = c(2,3,4)),
               trControl = trainControl(method = "cv",
                                        number = 5,
                                        verboseIter = FALSE,))
print(KF_RF)
knitr::kable(head(KF_RF$results), digits = 3)
print(KF_RF$bestTune)
predictions_KF_RF = predict(KF_RF, test_KF[-25])

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = predictions_KF_RF, obs = test$cnt))

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="red")
lines(predictions_KF_RF,col="blue")

################################################################################


