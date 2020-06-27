library(randomForest)

company<- read.csv(choose.files())
View(company)
str(company)
min(company$Sales)
max(company$Sales)
hist(company$Sales)

x<- company$Sales
comp_sales<- ifelse(x<=5,"5",ifelse(x<=10,"10","15"))
View(comp_sales)

company$Sales<- comp_sales
company$Sales <- as.factor(company$Sales)

sales_5 <- company[company$Sales=="5",]
sales_10 <- company[company$Sales=="10",]
sales_15 <- company[company$Sales=="15",]
comp_train<- rbind(sales_5[1:38,],sales_10[1:38,],sales_15[1:38,])
View(comp_train)
comp_test<- rbind(sales_5[39:76,],sales_10[39:76,],sales_15[39:76,])
View(comp_test)


# Building a random forest model on training data 
model_forest <- randomForest(Sales~.,data=comp_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(company$Sales==predict(model_forest,comp_train)) # 29.50% accuracy 

# Prediction of train data
pred_train <- predict(model_forest,comp_train)
library(caret)


# Confusion Matrix
confusionMatrix(comp_train$Sales, pred_train)

# Predicting test data 
pred_test <- predict(model_forest,newdata=comp_train)
mean(pred_test==comp_test$Sales) # Accuracy = 100 % 


# Confusion Matrix 

confusionMatrix(comp_test$Sales, pred_test)

# Visualization 
plot(model_forest,lwd=2)
legend("topright", colnames(model_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

