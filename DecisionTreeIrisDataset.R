#installing and loading required libraries
install.packages("rpart.plot")
install.packages(rpart)
library(rpart.plot)
library(rpart)

df=iris
#understanding the dataset
summary(df)
str(df)
sum(is.na(df))
# we can see that we dont have any missing values in this dataset.

#creating our train and test data
set.seed(1234)
train_idx=sample(nrow(df),nrow(df)*0.7)
length(train_idx)
nrow(df)
train_data=df[train_idx,]
test_data=df[-train_idx,]
dim(train_data)
dim(test_data)

#representation of each species in our training set.
table(train_data$Species)
#setting the control parameters
x=rpart.control()
print(x)
basic_model=rpart(Species~.,data=train_data,method = "class",control=x)
summary(basic_model)#get the summary of our model
rpart.plot(basic_model)

pred_values=predict(basic_model,type="class",newdata = test_data)
pred_values
conf_table=table(pred_values,test_data$Species)
conf_table
library(caret)
confusionMatrix(conf_table)#using the confusion matrix to determine the accuracy 

#tuning our model
new_control=rpart.control(minsplit = 50)
updated_model=rpart(Species~.,data = train_data,method = "class",control=new_control)

summary(updated_model)
rpart.plot(updated_model)

predicted_values=predict(updated_model,newdata = test_data,type="class")
conf_table=table(predicted_values,test_data$Species)
confusionMatrix(conf_table)
