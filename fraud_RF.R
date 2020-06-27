# Using Random Forest
install.packages("randomForest")
library(randomForest)

fraud<- read.csv(choose.files())
View(fraud)

str(fraud)
hist(fraud$Taxable.Income)

x<- fraud$Taxable.Income
fraud_income<- ifelse(x<=30000,"risky","good")
View(fraud_income)

fraud$Taxable.Income <- fraud_income
View(fraud)

str(fraud)
fraud$Taxable.Income<- as.factor(fraud$Taxable.Income)

fraud_good <- fraud[fraud$Taxable.Income=="good",]  #476 object
fraud_risky <- fraud[fraud$Taxable.Income=="risky",] # 124 object
fraud_train <- rbind(fraud_good[1:60,],fraud_risky[1:60,])
View(fraud_train)
fraud_test <- rbind(fraud_good[61:120,],fraud_risky[61:120,])
View(fraud_test)

# Building a random forest model on training data 
model_forest <- randomForest(Taxable.Income~.,data=fraud_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(fraud_train$Taxable.Income==predict(model_forest,fraud_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(model_forest,fraud_train)
library(caret)


# Confusion Matrix
confusionMatrix(fraud_train$Taxable.Income, pred_train)

# Predicting test data 
pred_test <- predict(model_forest,newdata=fraud_train)
mean(pred_test==fraud_test$Taxable.Income) # Accuracy = 100 % 


# Confusion Matrix 

confusionMatrix(fraud_test$Taxable.Income, pred_test)

# Visualization 
plot(model_forest,lwd=2)
legend("topright", colnames(model_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

