library(faraway)#for checking multicolinearity 
library(caret)  #for cross validation
library(ggcorrplot)#for data visualisation
library(naniar)#for replace with na function
library(mice)#For Multiple Imputation
library(dplyr)


#Importing dataset-----------------------------------------------------------

data<-read.csv("Life_Expectancy_Data1.csv",header = TRUE)
summary(data)

#Preprocessing / cleaning the dataset-----------------------------------------

na_indices<-which(is.na(data$SP.DYN.LE00.IN)) #storing indices of Missing Response variable values
na_indices
mydata = subset(data, select = -c(1,2,3) ) #Dropping columns 1,2,3 as It is country name and codes
summary(mydata)
round(sapply (mydata, function(x) (sum(is.na(x))/nrow(mydata))*100), digits = 0)#Calculating percentage of NAs from each feature
mydata=subset(mydata,select = -c(5,6,7,8,10,11,12,18,22,23,24,25))#Dropping columns as it has more than 40% missing data
summary(mydata)
View(mydata)
md.pattern(mydata) #Visualizing Missing Values

# MICE Imputation to Deal With Missing Data---------------------------------------------------------

#Stage 1
imputation<-mice(mydata,seed=23345,method = "cart",m=5)
print(imputation)
imputation$imp
stripplot(imputation,pch=20,cex=1.2)

#Stage 2
model.fit<-with(imputation,lm(SP.DYN.LE00.IN~SP.DYN.IMRT.IN,data = mydata))
summary(model.fit)

#Stage 3
pooled.model<-pool(model.fit)
pooled.model
summary(pooled.model)
pool.r.squared(model.fit)

 



# for(i in 1:ncol(mydata)){
#   mydata[is.na(mydata[,i]), i ] <- mean(mydata[,i], na.rm = TRUE)
# }              #replacing all NA values by mean in all columns
#
#
# View(mydata)



# Checking Multi Co-Linearity on each Imputed Data set---------------------------------------------
final_data1<-complete(imputation,1)
vif_Score<-vif(final_data1[,-1])
vif_Score

final_data2<-complete(imputation,2)
vif_Score<-vif(final_data2[,-1])
vif_Score

final_data3<-complete(imputation,3)
vif_Score<-vif(final_data3[,-1])
vif_Score

final_data4<-complete(imputation,4)
vif_Score<-vif(final_data4[,-1])
vif_Score

final_data5<-complete(imputation,5)
vif_Score<-vif(final_data5[,-1])
vif_Score

#Considering First Imputation as it has lowest VIF scores among 5
final_data<-final_data1
liner_regression<-lm(final_data$SP.DYN.LE00.IN~.,data = final_data )
corr<-cor(final_data)
round(corr,digits = 4)
View(final_data)


# Data Visualization  -----------------------------------------------------
ggcorrplot(corr) #Correlation Heat Map
hist(data$EG.ELC.ACCS.ZS,data = data,labels=TRUE,xlab = "% of People Electricity Access")
hist(data$SH.H2O.SMDW.ZS,data = data,labels=TRUE,xlab ="% of People Access to Safe Water")
boxplot(data$SI.POV.LMIC,las=1,xlab = "Porverty Head Count Ratio ")



# Stepwise Regression for feature selection-------------------------------

fit_model<-lm(final_data$SP.DYN.LE00.IN~.,data=final_data)
summary(fit_model)

back<-step(fit_model,direction = "backward")
forw<-step(fit_model,direction = "forward")
summary(forw)
summary(back)
final_data<-subset(final_data,select=c(1,2,5,6,7,9,13))#Selecting these features as it has Low Rsquared value(<0.05)
View(final_data)
# Separating countries with missing life expectancy ----------------------

final_data1<-final_data[na_indices,]
final_data2<-final_data[-na_indices,]
View(final_data1)
View(final_data2)



# Applying KNN Model for checking life expectancy with 10-fold Cross Validation  -----------------------------

train.index <- createDataPartition(final_data2[,"SP.DYN.LE00.IN"],p=0.75,list=FALSE)
final_data2.trn <- final_data2[train.index,]
final_data2.tst <- final_data2[-train.index,]
ctrl  <- trainControl(method  = "cv",number  = 10)
fit.cv_knn <- train(SP.DYN.LE00.IN ~ ., data = final_data2.trn, method = "knn",
                        trControl = ctrl, 
                        preProcess = c("center","scale"), 
                        tuneGrid =data.frame(k=15))
pred_knn <- predict(fit.cv_knn,final_data2.tst)
print(fit.cv_knn)





# Applying Linear Regression Model with Cross Validation with 10-fold
fit.cv_lr <- train(SP.DYN.LE00.IN ~ ., data = final_data2.trn, method = "lm",
  trControl = ctrl)
pred_lr <- predict(fit.cv_lr,final_data2.tst)
print(fit.cv_lr)




# Applying Random Forest with Cross Validation with 10 fold  -------------------------------------------------
fit.cv_rf <- train(SP.DYN.LE00.IN ~ ., data = final_data2.trn, method = "rf",
  trControl = ctrl)
pred_rf <- predict(fit.cv_rf,final_data2.tst)
print(fit.cv_rf)


# Choosing Linear Regression to predict Life expectancy of missing countries --------
pred_Life_Ex <- predict(fit.cv_lr,final_data1)
pred_Life_Ex<-as.data.frame(pred_Life_Ex)
country<-data$Country.Name[na_indices]
final_pred<-cbind(country,pred_Life_Ex)
View(final_pred)




#Question 5
data1<-final_data 
Continent<-data$Continent
data1<-cbind(data1,Continent)


#***Solomon Island was having blank continent, so I have replaced it with Autralia/oceania 

#Finding avg life expectancy across continents 
aggregate(x= data1$SP.DYN.LE00.IN,     
          # Specify group indicator
          by = list(data1$Continent),      
          # Specify function (i.e. mean)
          FUN = mean)
boxplot(data1$SP.DYN.LE00.IN~data1$Continent,data=data1,xlab = "Continent",ylab = "Life_Expectancy")
a<-aov(data1$SP.DYN.LE00.IN~data1$Continent ,data = data1)
summary(a)

pairwise.t.test(data1$SP.DYN.LE00.IN,data1$Continent,p.adj="bonferroni")
tuk<-TukeyHSD(a)
tuk
plot(tuk)

res<-a$residuals
par(mfrow=c(1,2))
hist(res,xlab = "Residuals")
qqnorm(res,pch=19)
qqline(res)

#Ho=Residuals are Normal
shapiro.test(res)#As p-value is >0.05 We fail to reject Null Hyp. i.e Residual are normal

#Ho=Variances are equal
bartlett.test(res~data1$Continent)#as p-value is <0.05, we reject Null hypothesis


#Conducting one way test as we have rejected Null Hypothesis
onway<-oneway.test(data1$SP.DYN.LE00.IN~data1$Continent)
onway




