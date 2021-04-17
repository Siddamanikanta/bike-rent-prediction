####Projet title :- Bike Renting using R####

#Problem statement :-
#The objective of this Case is to Predication of bike rental count on daily based 
#on the environmental and seasonal settings.

# INDEPENDENT VARIABLE: "RENTED BIKE COUNT"

#### We will do following steps
#1.import the data set
#2.remove unnecessary columns
#3.missing value analysis
#4.ouliers analysis
#5.seasons wise monthly distributions count
#6.working day wise distribution counts
#7.Encoding the categorical findMethodSignatures()
#8.split the dataset into train and test dataset
#9.modeling the training dataset
#10.cross validation prediction
#11.model performance on test dataset
#12.model Evaluation metrics
#13.choosing best model for predicting bike rental count

#To remove previous outputs and files in Rstudio
rm(list=ls())
#read xlsx file
library(readxl)
bike=read_excel("seoul bike count.xlsx")
View(bike)

#remove unnecessary columns
bike=bike[-1]
dim(bike)
#check the missing values of the data
data.frame(colSums(is.na(bike)))
#summary of data
summary(bike)
#count analysis of categorical data
table(bike$Seasons)
#str of the data
str(bike)
#dimensions of data
dim(bike)

#missing values numerical data replace with mean
bike$`Humidity(%)`[is.na(bike$`Humidity(%)`)]=mean(bike$`Humidity(%)`,na.rm=T)
bike$`Wind speed (m/s)`[is.na(bike$`Wind speed (m/s)`)]=mean(bike$`Wind speed (m/s)`,na.rm=T)
bike$`Visibility (10m)`[is.na(bike$`Visibility (10m)`)]=mean(bike$`Visibility (10m)`,na.rm=T)

#missing values categorical data replace with mode
bike$Seasons[is.na(bike$Seasons)]="Spring"

#check the outliers using bixplots
boxplot(bike$`Rented Bike Count`,bike$`Wind speed (m/s)`, bike$`Solar Radiation (MJ/m2)`,
        bike$`Rainfall(mm)`,bike$`Snowfall (cm)`)

#outliers replace with mean
Outlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- mean(x)
  x[x > (qnt[2] + H)] <- mean(x)
  return(x)
}
bike$`Rented Bike Count`=Outlier(bike$`Rented Bike Count`)
bike$`Wind speed (m/s)`=Outlier(bike$`Wind speed (m/s)`)
bike$`Solar Radiation (MJ/m2)`=Outlier(bike$`Solar Radiation (MJ/m2)`)
bike$`Rainfall(mm)`=Outlier(bike$`Rainfall(mm)`)
bike$`Snowfall (cm)`=Outlier(bike$`Snowfall (cm)`)

library(ggplot2)
#dev.off()
#column plot for season wise monthly distribution of counts
ggplot(bike,aes(x=Hour,y=`Rented Bike Count`,
                fill=Seasons))+theme_bw()+geom_col()+labs(x='Hour',
          y='Rented bike count',title='Season wise hourly distribution of counts')
#OBSERVED:From the above plots, we can observed that increasing the bike rental count in spring and
#summer season and then decreasing the bike rental count in Autumn and winter season.
#Column plot for holiday wise distribution of counts
ggplot(bike,aes(x=Holiday,y=`Rented Bike Count`,
                fill=Seasons))+theme_bw()+geom_col()+labs(x='Hour', y='Rented bike count',
                                    title='holiday wise  distribution of counts')
#OBSERVED:From the above bar plot, we can observed that during no holiday the bike rental
#counts is highest compared to during holiday for different seasons.

#label encoding features
bike$Holiday=as.numeric(factor(bike$Holiday))-1
bike$`Functioning Day`=as.numeric(factor(bike$`Functioning Day`))-1

library(fastDummies)
bike1=fastDummies::dummy_cols(bike$Seasons,remove_first_dummy = F)
bike1=bike1[-1]
bike2=scale(bike[c(1,4,6)])
bike3=bike[c(2,3,5,7,8,9,10,12,13)]
bikee=cbind(bike3,bike2,bike1)
View(bikee)
dim(bikee)

#Quintle-Quintle line
qqnorm(bikee$`Rented Bike Count`)
qqline(bikee$`Rented Bike Count`)

#Split the dataset based on simple random resampling
train_index<-sample(1:nrow(bikee),0.7*nrow(bikee))
train_data<-bikee[train_index,]
test_data<-bikee[-train_index,]
dim(train_data)
dim(test_data)

############ Modelling the training dataset #########


######## 1.linear regression model #########
#Set seed to reproduce the results of random sampling
set.seed(672)
#train the lm model
lr_model=lm(train_data$`Rented Bike Count`~.,train_data[,c(-10)])
#Summary of the model
summary(lr_model)

#we will be using the caret package for crossvalidation.function named "trainControl".
#method="CV" (used for crossvalidation)
#number=5 (means 5 fold crossvalidation)
#classProbs=T (model will save the prediction for each class)
#"train" is a function available in caret pakage
#Cross validation resampling method
#To ignore warning messages
options(warn=-1)
library(caret)
train.control=trainControl(method="CV",number=5,savePrediction=T,classProbs=T)
#Cross validation prediction
CV_predict=train(`Rented Bike Count`~.,data=train_data,method='lm',trControl=train.control)
#Summary of cross validation prediction
summary(CV_predict)
#OBSERVED:The adjusted R-squared or coefficient of determination is 0.548 on  cross validation ,
#it means that predictor is only able to predict 54% of the variance in the target 
#variable which is contributed by independent variables.



######## 2.knn ##########

knn_model=train(`Rented Bike Count`~.,data=train_data,method="knn",trControl=train.control)#,preProcess=c("center","scale"))
knn_model

######## 3.random forest ######
rf_model=train(`Rented Bike Count`~.,data=train_data,method="rf",trControl=train.control)#,preProcess=c("center","scale"))
rf_model


###### Final model for predicting the bike rental count on daily basis
#OBSERVED:When we compare the root mean squared error and mean absolute error of all 3 models,
#the random forest model has less root mean squared error and mean absolute error. 
#So, finally random forest model is best for predicting the bike rental count on daily basis.

predict=predict(rf_model,test_data)
tab=table(predict=predict,actual=test_data$`Rented Bike Count`)
View(head(tab))

