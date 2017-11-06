library(googleVis)
library(ggplot2)
library(caret)
library(gbm)
library(MASS)
library(readxl)
HR_data<-read_xls('HR_comma_sep.xls')
HR_data$left<-as.factor(HR_data$left)

#Few changes
levels(HR_data$sales)
HR_data$promotion_last_5years<-as.factor(HR_data$promotion_last_5years)
HR_data$Work_accident<-as.factor(HR_data$Work_accident)
#Boxplot diagram to know the distribution
ggplot(data=HR_data,aes(x=factor(left),y=time_spend_company))+geom_boxplot(aes(fill=salary))
ggplot(data=HR_data,aes(x=factor(left),y=satisfaction_level))+geom_boxplot(aes(fill=salary))
#From the box plot we ca say people who are highly experienced say years_ex>=8, they are not leaving the 
#company and also
#Histogram

ggplot(HR_data,aes(x=salary,y=mean(satisfaction_level)))+geom_bar(stat='identity')+facet_grid(left~.)+coord_cartesian(ylim = c(0,1))
ggplot(data=HR_data,aes(x=factor(left),y=average_montly_hours))+geom_boxplot(aes(fill=salary))
install.packages("shiny")
#library(shiny)
#ui <- fluidPage()
#server <- function(input, output) {}
#shinyApp(ui = ui, server = server)
#Lets partition data and remove some part 
library(caTools)
library(caret)
set.seed(123)
test <- HR_data[rbinom(20, 10, 0.5),]
?rbinom
Sample<-sample.split(HR_data$left,0.66)
train.data<-subset(HR_data,Sample==T)
test.data<-subset(HR_data,Sample==F)  
#Model<-train(left~.,data=train.data,method='glm',trainControl(method = 'cv',number = 5,savePredictions = T,classProbs = T),preProcess=c('center','scale'))
model<-glm(left~.,family = binomial,data = train.data)
summary(model)
library(corrgram)
corrgram(HR_data,upper.panel = panel.pie)
predictreg<-predict(model,test.data,type = 'response')
test.data$Prob<-ifelse(predictreg>=0.423,1,0)
table(test.data$Prob,test.data$left)
confusionMatrix(table(test.data$Prob,test.data$left))
library(nnet)
library(ROCR)
pr<-prediction(predictreg,test.data$left)
roc<-performance(pr,"tpr","fpr")
perf<-performance(pr,"acc")
plot(roc)
auc<-performance(pr,"auc")
plot(perf)
which.max(slot(perf,"y.values")[[1]])
slot(perf,"x.values")[[1]][830]
#AUC=.825
#Value cutofftaken 0.423
#ACcuracy @0.5 cutoff is 79.83
#accuracy @0.423 cutoff is 80.25

#Generalized boosted regression model
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 1)
mod_BR <- train(left ~., train.data, method="gbm", trControl=fitControl, verbose = FALSE)
plot(mod_BR, main = "Model 2")
confusionMatrix(table(predictbr,test.data$left))
#Accuracy 97.3%
#Kappa=0.92

#Random Forest
library(randomForest)
mod_RF<-randomForest(left~.,data=HR_data,subset=train.data, mtry=3,importance=TRUE)
set.seed(1)
rf.boston=randomForest(left~.,data=HR_data,subset=train.data, mtry=3,importance=TRUE)

preRF<-predict(mod_RF,test.data[,-11],type='class')
confusionMatrix(table(preRF,test.data$left))
pred<-predict(mod_RF,test)
head(test)
confusionMatrix(pred,test$left)
head(HR_data,10)
test
#Accuracy 99%
#Kappa 0.97
