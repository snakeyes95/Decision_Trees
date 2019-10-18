library(rpart)
library(rpart.plot)

#Working with the Credits dataset to predict the Creditability variable
#loading data into dataset
getwd()
setwd("E:\\Decision_Trees")
df=read.csv("Credit.csv")

#understanding our dataset

head(df)

summary(df)
str(df)#data_type mismatch is present in the dataset

sum(is.null(df))
#we see that we dont have to deal with any missing values
a=colnames(df)
df$Creditability=as.factor(df$Creditability)
df$Account_Balance=as.factor(df$Account_Balance)
df$Credit_History=as.factor(df$Credit_History)
df$Purpose=as.factor(df$Purpose)
df$Value_Savings=as.factor(df$Value_Savings)
df$Emp_Len=as.factor(df$Emp_Len)
df$Sex=as.factor(df$Sex)
df$Guarantors=as.factor(df$Guarantors)
df$asset=as.factor(df$asset)
df$Concurrent_Credits=as.factor(df$Concurrent_Credits)
df$Type_of_apartment=as.factor(df$Type_of_apartment)
df$Occupation=as.factor(df$Occupation)
df$Telephone=as.factor(df$Telephone)
df$Foreign_Worker=as.factor(df$Foreign_Worker)
str(df)#data_type fixed


#Building train and test set
set.seed(12345)
train_idx=sample(nrow(df),nrow(df)*0.7)
train_data=df[train_idx,]
test_data=df[-train_idx,]
dim(train_data)
dim(test_data)

#Our target variale in this case is Creditability 
#Visualizing our taget variable

a=table(df$Creditability)
print(a)
#0 are the defaulters people who have not paid back the loan

barplot(a,main="Creditability")

#setting the control
x=rpart.control()
basic_model=rpart(Creditability~.,data = train_data,method = "class",control = x)
summary(basic_model)

rpart.plot(basic_model)
predicted_values=predict(basic_model,type = "class",newdata = test_data)
conf_table=table(predicted_values,test_data$Creditability)

install.packages("lattice")
install.packages("caret")
library(caret)
conf_mat=confusionMatrix(conf_table)
conf_mat

#tuning our model









