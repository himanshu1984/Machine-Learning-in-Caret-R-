# Machine-Learning-in-Caret-R-
Applying Random Forest, linear regression and Logistic regression on Insurance data through CARET package in R

library(caret)
library(dplyr)

data=read.csv(file="C:\\Users\\lenovo\\Desktop\\Himanshu\\insurance.csv")

str(data)

##relationship between bmi, age and smoker(we can see that its pretty random)

ggplot(data,aes(age,bmi))+geom_point(aes(color=data$smoker))

##relationship between charges, bmi and smoker (this graph shows an interesting point: for smokers, relationship between charges and bmi seems to be linear)

ggplot(data,aes(charges,bmi))+geom_point(aes(color=data$smoker))

##relationship between charges, age and smoker(sameways a smoker pays higher insurance premiuim regardless of their age)

ggplot(data,aes(charges,age))+geom_point(aes(color=data$smoker))



##preprocess with center and scale

preproc_model= preProcess(data,method=c("scale","center"))

preproc_data=predict(preproc_model,data)

##creating dummy variable for categorical values

DV=dummyVars(charges~.,data=preproc_data)

data_OHE=as.data.frame(predict(DV,preproc_data))



data_OHE$charges=preproc_data$charges

final.data=data_OHE

##data partition

part.rows=createDataPartition(final.data$charges,p=.75,list=F)

train.data=final.data[part.rows,]
test.data=final.data[-part.rows,]


##randomForest

model.RF=train(charges~.,data=train.data,method="rf")

pre.RF=predict(model.RF,test.data)

postResample(pre.RF,test.data$charges)

##Linear Regression

model.LR=train(charges~.,data=train.data,method="lm")

pre.LR=predict(model.LR,test.data)

postResample(pre.LR,test.data$charges)

## assessing Variable Importance (we see that most important variable is non smoker in predicting charges, just beacause dummy variable model assigns 1 mostly to non smoker category)

plot(varImp(model.LR))

table(final.data$smoker.yes)

table(final.data$smoker.no)




## lets turn above problem into a classification, where i am going to predict weather a person is a smoker or a non smoker 


######classification#####
##predicting if a person is smoker or not

## Random Forest

preproc_data

data.class=select(preproc_data,-region)


part.rows.cl=createDataPartition(data.class$smoker,p=.75,list=F)

train.class=data.class[part.rows.cl,]
test.class=data.class[-part.rows.cl,]

model.class.RF=train(smoker~.,data=train.class,method = "rf")

model.cl.pre=predict(model.class.RF,test.class)

confusionMatrix(model.cl.pre,test.class$smoker)

## we see that random forest model performed very well on test data with 98% accuracy

##logistic regression

model.class.LOR=train(smoker~.,data=train.class,method = "bayesglm")

model.LOR.pre=predict(model.class.LOR,test.class)

confusionMatrix(model.LOR.pre,test.class$smoker)

## sameways logistic regression also performed well




## modelling the data for Smoker category only (predicting charges through random forest and linear regression)


data=read.csv(file="C:\\Users\\lenovo\\Desktop\\Himanshu\\insurance.csv")

data=data%>%filter(smoker=="yes")




ggplot(data,aes(charges,bmi))+geom_point()


ggplot(data,aes(charges,age))+geom_point()


ggplot(data,aes(charges,age))+geom_point(aes(color=data$bmi))


preproc_model= preProcess(data,method=c("scale","center"))

preproc_data=predict(preproc_model,data)

DV=dummyVars(charges~.,data=preproc_data)

data_OHE=as.data.frame(predict(DV,preproc_data))

data_OHE$charges=preproc_data$charges

final.data=data_OHE

part.rows=createDataPartition(final.data$charges,p=.9,list=F)

train.data=final.data[part.rows,]
test.data=final.data[-part.rows,]

##randomForest

model.RF=train(charges~.,data=train.data,method="rf")

pre.RF=predict(model.RF,test.data)

postResample(pre.RF,test.data$charges)

##Linear Regression

model.LR=train(charges~.,data=train.data,method="lm")

pre.LR=predict(model.LR,test.data)

postResample(pre.LR,test.data$charges)
