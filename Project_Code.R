library(rpart)
library(caret)
library(party)
library(plyr)
library(MASS)
library(ROSE)
library(randomForest)
library(nnet)
library(ROCR)

#Importing the the Excel file to a data frame

Churn_Data <- read_excel("~/Spring 2018/Data Mining, IDS 570/Assignment4/UV6696-XLS-ENG.xlsx")
View(Churn_Data)

#Checking for the missing values in dataset.

apply(Churn_Data,2,function(x) sum(is.na(x)))
str(Churn_Data)

#Changing Column names

names(Churn_Data)<- c("ID","Customer_Age", "Churn","CHI_Score_0","CHI_Score_1","Support_Case_0","Support_Case_1","SP_0","SP_1","Logins","Blog_Articles","Views","Days_Since_Last_login")
View(Churn_Data)

#Converting Churn to Factor variable
Churn_Data$Churn<-as.factor(Churn_Data$Churn)

#Making dataframe copy

Churn_Data_Backup<- Churn_Data
attach(Churn_Data)


View(Churn_Data)
#Q.7
#(a) Is Wall's belief about the dependence of churn rates on customer age supported by the data? To
#get some intuition, try visualizing this dependence

table(Churn_Data$Churn)
barplot(table(Churn_Data$Churn,Churn_Data$Customer_Age))

barplot(table(Churn[Churn==1],Customer_Age[Churn==1]))

count(Churn[Customer_Age<13])
count(Churn[Customer_Age>12])
count(Churn[Customer_Age<25])

#Churn rate seems to be depended upon customer's age. Churn rate during the first year of customer high and
#seems shoot up during 12th month.Churn rate seems to be considerably reduce after 24th month.Which seems to
#in line with wall's believe that churn rate is higest in 1st year and seems to be close to zero for
#customer's age more than 48 month.



#7.b Construct the best model to predict customers churn. You can try different classification
#models such as Logistic Regression, Neural Network and Decision Trees. You need to find
#the best settings for these models. Explain how you evaluate your models i.e. what is your
#evaluation measure? Accuracy? Precision? Recall?


#Dividing data in training and testing sets. Beacuse the dataset is small, we will devide the traing 
#and testing sets in 7:3 ratio

set.seed(23)
index <- sample(1:nrow(Churn_Data),round(0.70*nrow(Churn_Data)))
train_churn <- Churn_Data[index,-1]
test_churn <- Churn_Data[-index,-1]

table(train$Churn)

table(Churn)
#As the data is highly unbalanced the alogorihtm will try to improve accuracy by biasing the data to
#Class 0 i.e. Churing 0( No Churn)

# We need to make the data balance, we using both over and under sampling
Churn_Data_Balanced <- ovun.sample(Churn ~ ., data = train_churn, method = "both", p=0.5, N=4217, seed = 2)$data
table(Churn_Data_Balanced$Churn)

#Building a RF Model

Churn_RF<-randomForest(Churn~., data = Churn_Data_Balanced, ntree=500, mtry= sqrt(ncol(train)-1), proximity= TRUE, sampsize= 0.65*nrow(train), importance= TRUE)
Churn_RF
summary(Churn_RF)

#OOB estimate of  error rate: 2.02%
#Confusion matrix:
#  0    1 class.error
#0 2006   74 0.035576923
#1   11 2126 0.005147403

#The OOB error is 2.02% and missclassification error of Churn is less than 1%, which is  desirable

# We need to make the data balance, we using both over sampling
Churn_Data_Balanced <- ovun.sample(Churn ~ ., data = Churn_Data_Balanced, method = "over", N=8500, seed = 2)$data
table(Churn_Data_Balanced$Churn)

#Building a RF Model

Churn_RF<-randomForest(Churn~., data = train, ntree=500, mtry= sqrt(ncol(train)-1), proximity= TRUE, sampsize= 0.65*nrow(train), importance= TRUE)
Churn_RF

#    OOB estimate of  error rate: 2.42%
#Confusion matrix:
#  0    1 class.error
#0 4035  182 0.043158644
#1   24 4259 0.005603549

#The OOB error is 2.42% and missclassification error of Churn is less than 1%, which is desirable



pred_churn_RF<-predict(Churn_RF, newdata = test_churn)
table(pred_churn_RF, test$Churn)

#Determining Accuracy, recall and other Paramenter by confusion matrix

confusionMatrix(predict(Churn_RF, type = "class", newdata = test_churn), test_churn$Churn, positive = '1', dnn = c("Predictions", "Actual Values"))

#Confusion Matrix and Statistics

#Actual Values
#Predictions    0    1
#0 1805   91
#1    2    6

#Accuracy : 0.9512          
#95% CI : (0.9405, 0.9604)
#No Information Rate : 0.9491          
#P-Value [Acc > NIR] : 0.3627          

#Kappa : 0.1074          
#Mcnemar's Test P-Value : <2e-16          

#Sensitivity : 0.061856        
#Specificity : 0.998893        
#Pos Pred Value : 0.750000        
#Neg Pred Value : 0.952004        
#Prevalence : 0.050945        
#Detection Rate : 0.003151        
#Detection Prevalence : 0.004202        
#Balanced Accuracy : 0.530374        

#'Positive' Class : 1    


importance(Churn_RF, type = 2)
#Customer Age, Happiness Index, Logins and Days since last login seems to be the important variables.


#Optimizing the RF model by tuning it to optimal mtry

optimal_RF<-tuneRF(train_churn[,-2], train_churn$Churn , stepFactor =0.5,improve = TRUE )

#optimal mtry found is '6' after hit and trial with step factor



#Using Neural Network


# Normalize data before applying neural network model,using min-max transformation to normalize data
maxs <- sapply(Churn_Data_Balanced, max) 
mins <- sapply(Churn_Data_Balanced[-2], min)

#Scaling the train data obtained after Balancing
scaled_train_Data <- as.data.frame(scale(Churn_Data_Balanced[-2], center = mins, scale = maxs - mins))
scaled_train_Data<-cbind(scaled_train_Data,Churn=Churn_Data_Balanced$Churn)



# scaling test data
maxs <- sapply(test_churn[-2], max) 
mins <- sapply(test_churn[-2], min)
scaled_test_Data <- as.data.frame(scale(test_churn[-2], center = mins, scale = maxs - mins))
scaled_test_Data<-cbind(scaled_test_Data,Churn=test_churn$Churn)


nrow(scaled_train_Data)
nrow(scaled_test_Data)


Churn_Neural<-nnet(scaled_train_Data$Churn~., data=scaled_train_Data, linout=F, size=10, decay=0.01, maxit=1000) 

#iter 410 value 1926.869094
#iter 420 value 1926.859746
#iter 430 value 1926.858740
#final  value 1926.858633 
#converged

pred_churn_Neural<-predict(Churn_Neural, scaled_test_Data,type='class')


table(pred_churn_Neural, scaled_test_Data$Churn)

confusionMatrix(pred_churn_Neural, scaled_test_Data$Churn,positive = '1',dnn=c('Predicted','Actual'))

#Confusion Matrix and Statistics

#Actual
#Predicted    0    1
#0  674   24
#1 1133   73

#Accuracy : 0.3923          
#95% CI : (0.3703, 0.4147)
#No Information Rate : 0.9491          
#P-Value [Acc > NIR] : 1          

#Accuracy of the prediction is considerably lower, that is moving to Logistic regression

#Logistic Regression

Churn_Logistic <- glm(Churn_Data_Balanced$Churn~., data = Churn_Data_Balanced, family = "binomial")
summary(Churn_Logistic)

#Call:
#  glm(formula = Churn_Data_Balanced$Churn ~ ., family = "binomial", 
#      data = Churn_Data_Balanced)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.1557  -1.1396   0.6448   1.0610   1.9353  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            2.442e-02  6.821e-02   0.358 0.720334    
#Customer_Age           2.742e-02  3.367e-03   8.144 3.81e-16 ***
#  CHI_Score_0           -5.316e-03  6.186e-04  -8.593  < 2e-16 ***
#  CHI_Score_1           -1.159e-02  1.388e-03  -8.349  < 2e-16 ***
#  Support_Case_0        -1.774e-01  4.834e-02  -3.670 0.000243 ***
#  Support_Case_1         1.292e-01  3.724e-02   3.469 0.000522 ***
#  SP_0                   3.402e-02  5.070e-02   0.671 0.502234    
#SP_1                   1.808e-02  3.796e-02   0.476 0.633930    
#Logins                 1.842e-03  1.093e-03   1.685 0.092060 .  
#Blog_Articles         -2.333e-04  1.139e-02  -0.020 0.983659    
#Views                 -1.290e-04  3.102e-05  -4.160 3.19e-05 ***
#  Days_Since_Last_login  1.158e-02  1.723e-03   6.723 1.78e-11 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 5845.2  on 4216  degrees of freedom
#Residual deviance: 5410.9  on 4205  degrees of freedom
#AIC: 5434.9

#Number of Fisher Scoring iterations: 4


pred_churn_Logistic<- predict(Churn_Logistic, newdata = test_churn, type ='response')

table(test_churn$Churn, pred_churn_Logistic > 0.5)

#FALSE TRUE
#0   980  827
#1    39   58


pred_Rocr <- prediction(pred_churn_Logistic, test_churn$Churn)
perfor_Rocr <- performance(pred_Rocr, measure = "tpr", x.measure = "fpr")
plot(perfor_Rocr)

perfor_AUC=performance(pred_Rocr,"auc") 
AUC=perfor_AUC@y.values[[1]]
plot(perfor_Rocr)
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))


Churn_Logistic_imp <- glm(Churn_Data_Balanced$Churn~ Churn_Data_Balanced$Customer_Age+Churn_Data_Balanced$CHI_Score_0+Churn_Data_Balanced$CHI_Score_1+Churn_Data_Balanced$Support_Case_0+Churn_Data_Balanced$Support_Case_1+Churn_Data_Balanced$Views+Churn_Data_Balanced$Days_Since_Last_login, data = Churn_Data_Balanced[,c(1:6,11,12)], family = "binomial")
summary(Churn_Logistic_imp)

#Call:
#  glm(formula = Churn_Data_Balanced$Churn ~ Churn_Data_Balanced$Customer_Age + 
#        Churn_Data_Balanced$CHI_Score_0 + Churn_Data_Balanced$CHI_Score_1 + 
#        Churn_Data_Balanced$Support_Case_0 + Churn_Data_Balanced$Support_Case_1 + 
#        Churn_Data_Balanced$Views + Churn_Data_Balanced$Days_Since_Last_login, 
#      family = "binomial", data = Churn_Data_Balanced[, c(1:6, 
#                                                          11, 12)])
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.1448  -1.1383   0.6474   1.0619   1.9274  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                4.007e-02  6.673e-02   0.601    0.548    
#Churn_Data_Balanced$Customer_Age           2.686e-02  3.330e-03   8.065 7.34e-16 ***
#  Churn_Data_Balanced$CHI_Score_0           -5.019e-03  5.889e-04  -8.523  < 2e-16 ***
#  Churn_Data_Balanced$CHI_Score_1           -1.076e-02  1.280e-03  -8.407  < 2e-16 ***
#  Churn_Data_Balanced$Support_Case_0        -1.472e-01  3.273e-02  -4.497 6.89e-06 ***
#  Churn_Data_Balanced$Support_Case_1         1.447e-01  2.864e-02   5.052 4.38e-07 ***
#  Churn_Data_Balanced$Views                 -1.284e-04  3.167e-05  -4.054 5.03e-05 ***
#  Churn_Data_Balanced$Days_Since_Last_login  1.146e-02  1.719e-03   6.665 2.65e-11 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 5845.2  on 4216  degrees of freedom
#Residual deviance: 5415.7  on 4209  degrees of freedom
#AIC: 5431.7

#Number of Fisher Scoring iterations: 4

pred_churn_Logistic<- predict(Churn_Logistic_imp, newdata = test_churn[,-c(7:10)], type ='response')
table(test_churn$Churn, pred_churn_Logistic_imp > 0.5)

#FALSE TRUE
#0   996  811
#1    36   61


#Auc has improved slightly but not significanlty


#Though Random forrest has better accuracy among all the tried alogrithm here. Logistic Regression is suggested
# as it is proven to be the best alogrithm to predict churn rate. The performance of Random Forest is better
#beacuse the instances are unbalanced and it supoose to give lowe accuracy beacuse of that in unseen data.


#c)

pred_churn_RF_100<-predict(rf_opt_mtry, newdata=test_churn,type='prob')
top_100 <- (1:nrow(test_churn))[order(pred_churn_RF_100[,2], decreasing=T)[1:100]]
