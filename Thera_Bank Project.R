setwd("C:/Users/ammu/Desktop/Great Lakes/4. Data Mining/Project")
getwd()
library("readxl")
library("mice")
library(e1071)
library(dplyr)
library(plyr)
library(caret)
library(DataExplorer)
library(caTools)
library(rpart)
library(grid)
library(gridExtra)
library(ggplot2)
library(rpart.plot)
library(corrplot)
library(caret)
library(ROCR)
library(ineq)
library(randomForest)
TheraBank=read_excel("Thera_Bank_dataset.xlsx",2)
View(TheraBank)
dim(TheraBank)
head(TheraBank)
tail(TheraBank)
str(TheraBank)
names(TheraBank)
summary(TheraBank)
anyNA(TheraBank)
sum(is.na(TheraBank))
sum(rowSums(is.na(TheraBank)))
sum(colSums(is.na(TheraBank)))
dev.off()
md.pattern(TheraBank)

#remove nulls
TheraBankData= TheraBank[complete.cases(TheraBank),]

#rename variables
names(TheraBankData)=c("ID","Age","Experience","Income","ZipCode","FamilyMembers",
                       "CCAvg","Education","Mortagage","PersonalLoan","SecuritiesAccount",
                       "CDAccount","Online","CreditCard")
names(TheraBankData)
attach(TheraBankData)

#change the experience negative values
TheraBankData %>% filter (TheraBankData$Experience <0)
TheraBankData$Experience =abs(TheraBankData$Experience)
TheraBankData %>% filter (TheraBankData$Experience <0)

#Converting Education into factors
TheraBankData$Education=as.factor(Education)
TheraBankData$Education=revalue(TheraBankData$Education,
            c("1"="Undergrad", "2"="Graduate","3"="Advanced/Professional"))
TheraBankData$Education=factor(TheraBankData$Education,
            levels = c("Undergrad","Graduate","Advanced/Professional")
            ,order= TRUE)

#converting Other variables into factors
TheraBankData$FamilyMembers =as.factor(TheraBankData$FamilyMembers)
TheraBankData$Education=as.factor(TheraBankData$Education)
TheraBankData$PersonalLoan =as.factor(TheraBankData$PersonalLoan )
TheraBankData$SecuritiesAccount =as.factor(TheraBankData$SecuritiesAccount)
TheraBankData$CDAccount =as.factor(TheraBankData$CDAccount)
TheraBankData$Online=as.factor(TheraBankData$Online)
TheraBankData$CreditCard=as.factor(TheraBankData$CreditCard)
attach(TheraBankData)
str(TheraBankData)
summary(TheraBankData)
anyNA(TheraBankData)


#feature sclaing
z=TheraBankData[c(2:4,7,9)]
m=apply(z ,2,mean)
s=apply(z,2,sd)
TheraBankDataScaling=scale(z,m,s)
TheraBankDataScaled=TheraBankData[-c(2:4,7,9)]
TheraBankDataScaled=cbind(TheraBankDataScaled,TheraBankDataScaling)
head(TheraBankDataScaled)
summary(TheraBankDataScaled)
View(TheraBankDataScaled)
boxplot(TheraBankDataScaling,
        main="Box plot for Continous variables"
        ,col=c("Red","Green","Yellow","Blue","Pink"))

#remove unecessary variables(ID, ZipCode)
names(TheraBankDataScaled)
TheraBankDataScaled=TheraBankDataScaled[,-c(1,2)]
names(TheraBankDataScaled)

#univariate
#Age
attach(TheraBankData)
uniqv <- unique(Age)
uniqv[which.max(tabulate(match(Age, uniqv)))]
min(Age)
max(Age)
AgeGroup=cut(Age,breaks = c(22,30,40,50,60,70)
             ,labels=c("<=30","31-40","41-50","51-60","more than 60"))
plot(AgeGroup,main="Age of Customer"
     ,col=c("Blue","Yellow","Red","Pink","Orange")
     ,ylab="No. of people",xlab="Age bucket")
#Experience
zeroexp=TheraBankData %>% select (Experience)%>%filter (TheraBankData$Experience ==0) %>% count()
zeroexp

Exp=cut(Experience,breaks = c(0,10,20,30,50)
        ,labels = c("<= 10 years","11-20 Yrs","21-30 Yrs","more than 30 Yrs"))

plot(Exp,main="Experience of Customer",col=c("Blue","Yellow","Red","Green","Pink"),ylab="No. of people",xlab="Experience bucket")
#Income
zeroinc=TheraBankData %>% select (Income)%>%filter (TheraBankData$Income ==0) %>% count()
zeroinc

Inc=cut(Income,breaks = c(0,50,100,150,300),labels = c("0-50 $","51-100 $","101-150 $","151-200 $"))
plot(Inc,main="Anual Income group of Customer in ($000)",col=c("Blue","Yellow","Red","Green","Pink"),ylab="No. of people",xlab="Income bucket")
#FamilyMembers
histogram(FamilyMembers
          ,xlab = "Family Size of customer"
          ,ylab="No. of Customers in each category"
          ,main="Family Size chart")
#Credit card Spend
CC=cut(CCAvg,breaks = c(0,1,4,6,20),labels = c("1","2-4","5-6", "more than 6"))
plot(CC,main="Average CC Spent by Customer",col=c("Blue","Yellow","Red","Green","Pink"),ylab="No. of people",xlab="Amount in $ ($000)")
#Education
histogram(Education,main="Education Chart", col=c("Green","Blue","Red"))
histogram(Mortagage,main="House Mortage Chart in.($000)")
histogram(PersonalLoan,main="Customer Acceptance of Personal Loan in last campaign")
table(PersonalLoan)
histogram(SecuritiesAccount,main="Securities Account holders")
table(SecuritiesAccount)
histogram(CDAccount,main="Certificate of Deposit holders")
table(CDAccount)
histogram(Online,main="Internet Banking Facilities")
table(Online)
histogram(CreditCard,main="Usage of Credit card")
table(CreditCard)

names(TheraBankData)
TheraBankData=TheraBankData[-c(1,5)]
plot_density(TheraBankData,title="Density plot for Continuous variables")
plot_histogram(TheraBankData, title = "Histogram for Continuous variables")


#Bivariate Analysis
TheraBankCor= TheraBank[complete.cases(TheraBank),]
names(TheraBankCor)=c("ID","Age","Experience","Income","ZipCode"
                      ,"FamilyMembers","CCAvg","Education","Mortagage"
                      ,"PersonalLoan","SecuritiesAccount",
                       "CDAccount","Online","CreditCard")
TheraBankCor %>% filter(TheraBankCor$Experience <0)
TheraBankCor$Experience =abs(TheraBankCor$Experience)
#remove unecessary variables
TheraBankCor=TheraBankCor[,-c(1,5)]
#corrplot
dev.off()
corrplot(cor(TheraBankCor),method = "number",type = "upper"
         ,main="Corrplot for relevant variables")
#continous variables corrploe
TheraBankCon=TheraBankCor[ ,c(1:3,5,7)]
corrplot(cor(TheraBankCon),method = "number"
         ,type = "upper"
         ,main="Corrplot for continous variables")

#qplots (bar), Personal loan with variables
bar1=qplot(AgeGroup,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="Age vs Personal Loan"
      ,xlab="Age Group"
      ,ylab="Count")
bar2=qplot(Experience,fill= PersonalLoan, data=TheraBankData,geom="bar"
        ,main="Exp vs Personal Loan"
        ,xlab="Experience"
        ,ylab="Count")
  
bar3=qplot(Inc,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="Income group vs Personal Loan"
      ,xlab="Income"
      ,ylab="Count")
bar4=qplot(FamilyMembers,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="Num of Family members vs Personal Loan"
      ,xlab="No. of Family members"
      ,ylab="Count")
bar5=qplot(Education,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="Education vs Personal Loan"
      ,xlab="Education"
      ,ylab="Count")
bar6=qplot(SecuritiesAccount,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="SecuritiesAccount vs Personal Loan"
      ,xlab="SecuritiesAccount"
      ,ylab="Count")
bar7=qplot(CDAccount,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="CDAccount vs Personal Loan"
      ,xlab="CDAccount"
      ,ylab="Count")
bar8=qplot(Online,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="Internet Banking vs Personal Loan"
      ,xlab="Online"
      ,ylab="Count")
bar9=qplot(CreditCard,fill= PersonalLoan, data=TheraBankData,geom="bar"
      ,main="CreditCard vs Personal Loan"
      ,xlab="CreditCard"
      ,ylab="Count")

grid.arrange(bar1,bar2,bar3,bar4,bar5,bar6,bar7,bar8,bar9,nrow=3,ncol=3)

#qplots density plots, Personal loan with continous variables

a=qplot(AgeGroup,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="Age vs Personal Loan"
      ,xlab="Age Group"
      ,ylab="density",alpha=I(.5))
b=qplot(Experience,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="Experience vs Personal Loan"
      ,xlab="Experience"
      ,ylab="density",alpha=I(.5))
grid.arrange(a,b, ncol = 1, nrow = 2)

qplot(Inc,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="Income group vs Personal Loan"
      ,xlab="Income"
      ,ylab="density",alpha=I(.5))
qplot(FamilyMembers,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="Num of Family members vs Personal Loan"
      ,xlab="No. of Family members"
      ,ylab="density",alpha=I(.5))
qplot(Education,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="Education vs Personal Loan"
      ,xlab="Education"
      ,ylab="density",alpha=I(.5))
qplot(SecuritiesAccount,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="SecuritiesAccount vs Personal Loan"
      ,xlab="SecuritiesAccount"
      ,ylab="density",alpha=I(.5))
qplot(CDAccount,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="CDAccount vs Personal Loan"
      ,xlab="CDAccount"
      ,ylab="density",alpha=I(.5))
qplot(Online,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="Internet Banking vs Personal Loan"
      ,xlab="Online"
      ,ylab="density",alpha=I(.5))
qplot(CreditCard,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="CreditCard vs Personal Loan"
      ,xlab="CreditCard"
      ,ylab="density",alpha=I(.5))
qplot(Mortagage,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="Mortagage vs Personal Loan"
      ,xlab="Mortage"
      ,ylab="density",alpha=I(.5))
qplot(CCAvg,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="CCAvg vs Personal Loan"
      ,xlab="CCAvg"
      ,ylab="density",alpha=I(.5))

qplot(CCAvg,fill= PersonalLoan, data=TheraBankData,geom="density"
      ,main="CCAvg vs Personal Loan"
      ,xlab="CCAvg"
      ,ylab="density",alpha=I(.5))

#qplots- boxplots Numerical vs categorical (personal loan)
?qplot
qplot(PersonalLoan, Experience, data=TheraBankData,geom="boxplot"
      ,main="Experience vs Personal Loan"
      ,xlab="Personal Loan"
      ,ylab="Experience",fill=PersonalLoan)
qplot(PersonalLoan, Age, data=TheraBankData,geom="boxplot"
      ,main="Age vs Personal Loan"
      ,xlab="Personal Loan"
      ,ylab="Age",fill=PersonalLoan)
qplot(PersonalLoan, Income, data=TheraBankData,geom="boxplot"
      ,main="Income vs Personal Loan"
      ,xlab="Personal Loan"
      ,ylab="Income",fill=PersonalLoan)

qplot(PersonalLoan, CCAvg, data=TheraBankData,geom="boxplot"
      ,main="CCAvg vs Personal Loan"
      ,xlab="Personal Loan"
      ,ylab="CCAvg",fill=PersonalLoan)

qplot(PersonalLoan, Mortagage, data=TheraBankData,geom="boxplot"
      ,main="Mortagage vs Personal Loan"
      ,xlab="Personal Loan"
      ,ylab="Mortagage",fill=PersonalLoan)

names(TheraBankDataScaled)
View(TheraBankDataScaled)
attach(TheraBankDataScaled)

#splitting data
set.seed(1234)
splitCart=sample.split(TheraBankDataScaled$PersonalLoan ,SplitRatio=0.7)
trainDataCart=subset(TheraBankDataScaled,splitCart=="TRUE")
testDataCart=subset(TheraBankDataScaled,splitCart=="FALSE")
View(splitCart)
#checking how the many and how much proportion opted personal loan and how many didn't
#dataset is imbalanced, baseline conversion
table(TheraBankDataScaled$PersonalLoan )
round(prop.table(table(TheraBankDataScaled$PersonalLoan)),3)
#checking the proportion of data in the train and test dataset
round(prop.table(table(trainDataCart$PersonalLoan)),3)
round(prop.table(table(testDataCart$PersonalLoan)),3)

#cart model
?rpart
?rpart.control()
treeCart=rpart(formula = PersonalLoan~.
           ,data=trainDataCart,method="class"
           ,control=rpart.control(minsplit = 20, minbucket = 4,cp=0))

treeCart
printcp(treeCart)
plotcp(treeCart)
rpart.plot(treeCart, cex=0.6)
treeCart$cptable
plot(treeCart$cptable)

boxcols = c("orange", "palegreen3")[treeCart$frame$yval]
par(xpd=TRUE)
prp(treeCart, faclen=0, cex=0.6, extra=1, box.col= boxcols, nn=TRUE, uniform=TRUE)


#pruning cart model
print(treeCart$cptable)
treeCart$cptable[,"xerror"]
min(treeCart$cptable[,"xerror"])
bestcp=treeCart$cptable[which.min(treeCart$cptable[,"xerror"]), "CP"]
bestcp
ptree=prune(treeCart, cp=bestcp)
print(ptree)
rpart.plot(ptree,cex=0.6)
boxcols = c("orange", "palegreen3")[ptree$frame$yval]
par(xpd=TRUE)
prp(ptree, faclen=0, cex=0.6, extra=1, box.col= boxcols, nn=TRUE, uniform=TRUE)

summary(ptree)

path.rpart(ptree,nodes = 2:4)
treeCart$variable.importance
ptree$variable.importance
printcp(ptree)
printcp(treeCart)
barplot(ptree$variable.importance)
barplot(treeCart$variable.importance)

#predict.class on train data
predict.class=predict(ptree, trainDataCart, type="class")
##Confusion Matrix for CART Model
?with
tabCartTrain=with(trainDataCart,table(PersonalLoan,predict.class))
trainDataCart$predict.class=predict(ptree,trainDataCart, type="class")
trainDataCart$predict.score=predict(ptree,trainDataCart, type="prob")[,"1"]
trainDataCart$predict.score
print(tabCartTrain)
#View((trainData))
#cart train confusion matrix
tabCartTrain[2,2]
CART_train_accuracy= round((tabCartTrain[1,1]+tabCartTrain[2,2])/(tabCartTrain[1,1]+tabCartTrain[1,2]+tabCartTrain[2,1]+tabCartTrain[2,2]),2)
CART_train_accuracy
CART_train_specificity = round((tabCartTrain[2,2])/(tabCartTrain[2,2]+tabCartTrain[1,2]),2)
CART_train_specificity
confusionMatrix(tabCartTrain)
#View(testData)
predict.class=predict(ptree, testDataCart, type="class")
tabCartTest=with(testDataCart,table(PersonalLoan ,predict.class))
testDataCart$predict.class=predict(ptree,testDataCart, type="class")
testDataCart$predict.score=predict(ptree,testDataCart, type="prob")[,"1"]
testDataCart$predict.score
print(tabCartTest)
#Cart test confusion matrix
CART_test_accuracy= round((tabCartTest[1,1]+tabCartTest[2,2])/(tabCartTest[1,1]+tabCartTest[1,2]+tabCartTest[2,1]+tabCartTest[2,2]),2)
CART_test_accuracy
CART_test_specificity = round((tabCartTest[2,2])/(tabCartTest[2,2]+tabCartTest[1,2]),2)
CART_test_specificity
confusionMatrix(tabCartTest)
#RF
set.seed(1234)
splitRF=sample.split(TheraBankDataScaled$PersonalLoan ,SplitRatio=0.7)
trainDataRF=subset(TheraBankDataScaled,splitRF=="TRUE")
testDataRF=subset(TheraBankDataScaled,splitRF=="FALSE")
mtry1 = floor(sqrt(ncol(trainDataRF)))
mtry1
#View(trainData)
names(trainDataRF)
#separating Y/predictor/dependent variable
x=trainDataRF[ ,-3]
#View(x)
#Viewing the independent variables
names(x)
y=trainDataRF$PersonalLoan
#building Random Forest model  
set.seed(1234)
?tuneRF
names(TheraBankDataScaled)
bestmtry = tuneRF(x, y, stepFactor = 1.5, improve = 1e-5, ntree=500)
print(bestmtry)
TRF = tuneRF(x, y,
             mtryStart = 6,ntreeTry = 500,stepFactor = 1.5,improve = 0.0001,
             trace=TRUE,
             plot=TRUE,
             doBest= TRUE,
             nodesize=100,
             importance=TRUE)
print(TRF)
TheraRF= randomForest(PersonalLoan~.,data = trainDataRF,ntree=500,mtry=4
                      ,nodesize=100,
                      importance=TRUE)
print(TheraRF)
plot(TheraRF, main= "Random Forest Error Rate with 500 trees")
print(TheraRF)
TheraRF= randomForest(PersonalLoan~.,data = trainDataRF,ntree=300,mtry=4
                      ,nodesize=100,
                      importance=TRUE)
print(TheraRF)
plot(TheraRF, main= "Random Forest Error Rate with 300 tress")
importance(TheraRF)
varImpPlot(TheraRF,main = "Variable Importance" )
predict.classRF=predict(TheraRF, trainDataRF, type="class")
#predict.classRF
tabdevRF=table(trainDataRF$PersonalLoan ,predict.classRF)
tabdevRF
confusionMatrix(tabdevRF)
predict.classRFTest=predict(TheraRF, testDataRF, type="class")
#predict.classRF
tabdevRFTest=table(testDataRF$PersonalLoan ,predict.classRFTest)
tabdevRFTest
confusionMatrix(tabdevRFTest)


#Model Performance Measure
#AUC and ROC

?prediction

predobjtrain = prediction(trainDataCart$predict.score, trainDataCart$PersonalLoan )
preftrain = performance(predobjtrain, "tpr", "fpr")
plot(preftrain,main="Train Data ROC")

predobjtest = prediction(testDataCart$predict.score, testDataCart$PersonalLoan)
preftest = performance(predobjtest, "tpr", "fpr")
plot(preftest,main="Test Data ROC")

auctrain = performance(predobjtrain, "auc")
auctrain= as.numeric(auctrain@y.values)
auctrain

auctest = performance(predobjtest, "auc")
auctest= as.numeric(auctest@y.values)
auctest

#Gini Coefficient train
Ginitrain= (2*auctrain) - 1
Ginitrain

Ginitrainnew = ineq(trainDataCart$predict.score , "gini")
Ginitrainnew

Ginitest= (2*auctest) - 1
Ginitest

Ginitestnew = ineq(trainDataCart$predict.score, "gini")
Ginitestnew

#KS value
KStrain=max(preftrain@y.values[[1]]- preftrain@x.values[[1]])
KStrain

KStest=max(preftest@y.values[[1]]- preftest@x.values[[1]])
KStest

