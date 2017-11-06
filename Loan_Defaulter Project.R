Data<-read.csv('loan_data.csv')

#Explorartory Data Analysis
library(corrgram)
corrgram(Data,lower.panel = "panel.pie")
library(ggplot2)
ggplot(Data,aes(fico))+geom_histogram(fill='blue')+facet_grid(factor(not.fully.paid)~.)
ggplot(Data,aes(fico,int.rate))+geom_point(aes(col=factor(not.fully.paid)))+facet_grid(factor(not.fully.paid)~.)
Data$not.fully.paid<-as.factor(Data$not.fully.paid)
Data1<-Data
Data1<-scale(Data1[3:13])
var(Data1[,2])
Data_final<-cbind(Data[,c(1,2)],Data1)
Data_final$not.fully.paid<-Data$not.fully.paid
#library(data.table)
#setnames(Data_final, "Data[,14]", "not.fully.paid")

#Test-Train Split
library(caTools)
set.seed(123)
sample<-sample.split(Data_final$credit.policy,0.7)
Train<-subset(Data_final,sample==T)
Test<-subset(Data_final,sample==F)

#Model Building
#KNN
library(class)
model1<-knn(train = Train[3:13],test = Test[3:13],Train$not.fully.paid,k = 3)
library(caret)
confusionMatrix(table(model1,Train$not.fully.paid))
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 10) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(not.fully.paid ~ ., data = Train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
plot(knnFit)
confusionMatrix(table(knnFit$results,Test$not.fully.paid))
pr<-predict(knnFit,Test,type="prob")

#Accuracy=81%
#Sensitivity=0.94
#Specificity=0.11
#Kappa=0.07


#Glm
library(caTools)
Data$credit.policy<-factor(Data$credit.policy)
set.seed(123)
sample<-sample.split(Data$credit.policy,0.7)
Train2<-subset(Data,sample==T)
Test2<-subset(Data,sample==F)
library(ROCR)
library(nnet)
model<-multinom(not.fully.paid~.,data=Train2)
summary(model)
p<-predict(model,Test2,type="probs")
Test2$prob2<-ifelse(p>=0.5,1,0)
head(p)
pred<-prediction(p,Test$not.fully.paid)
roc<-performance(c(pred,model1),"tpr","fpr")
plot(roc)
abline(a=0,b=1)
slot(performance(pred,"auc"),"y.values")
eval<-performance(pred,"acc")
performance()
plot(eval)
abline(h=0.84,v=0.45)
a<-which.max(slot(eval,"x.values")[[1]])
b<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][b]
cut<-slot(eval,"x.values")[[1]][b]
confusionMatrix(table(Test2$prob2,Test2$not.fully.paid))
#Accuracy=83.7%
#Sensitivity=0.99
#Specificity=0.02
#Kappa=0.033


#SVM
library(e1071)
set.seed(123)
sample<-sample.split(Data$credit.policy,0.7)
Train<-subset(Data,sample==T)
Test<-subset(Data,sample==F)
model3<-svm(not.fully.paid~.,Train)
summary(model3)
predictV<-predict(model,Test[1:13],type = "probs")
Test$prob<-ifelse(predictV>=0.5,1,0)
library(caret)
confusionMatrix(table(Test$prob,Test$not.fully.paid))

#ACcuracy 83.7%
#Kappa=0.033
#Specificity=0.028
#Sensitivity=0.992
predictV2<-prediction(predictV,Test$not.fully.paid)
ROC<-performance(predictV2,'tpr','fpr')
plot(ROC)
abline(a=0,b=1)
evaluate<-performance(predictV2,"acc")
plot(evaluate)
table(predictV,Test$not.fully.paid)
a<-which.max(slot(evaluate,"y.values")[[1]])
slot(evaluate,"y.values")[[1]][a]
abline(h=0.83)

#The results are quite bad, specificity is just 0.02 percent
#there fore we will tune our model
tune.model<-tune(svm,train.x = not.fully.paid~.,data = Train,kernel='radial',
                 ranges = list(cost=c(1,2),gamma=c(0.1,1)))
pred.value<-predict(tune.model,Test[1:13],type="probs")
?tune
#After running the accuraacy is further improved to 85%
#Specificity=0.36
#Sensitivity=0.97
#kappa=0.039