setwd("C:/Users/ADMIN")

dataset = read.csv('imputed_6000_final.csv')

dataset$X <- NULL
dataset$Educ <- NULL
dataset$Unmarried <- NULL
dataset$Income <- NULL
dataset$CareSource <- NULL
dataset$Insured <- NULL
dataset$Weight <- NULL
dataset$Height <- NULL
dataset$Waist <- NULL
dataset$SBP <- NULL
dataset$DBP <- NULL
dataset$HDL <- NULL
dataset$LDL <- NULL
dataset$Total.Chol <- NULL
dataset$Activity <- NULL
dataset$PoorVision <- NULL
dataset$Smoker <- NULL
dataset$CVD <- NULL
dataset$Obese <- NULL
dataset$Fam.Hypertension <- NULL
#dataset$Racegrpblack <- NULL
#dataset$Racegrphispa <- NULL
dataset$Racegrpother <- NULL
dataset$Racegrpwhite <- NULL

library(dplyr)

#dataset <- mutate(dataset, Age_below_50 = ifelse(Age < 50, 1,0))
#dataset <- mutate(dataset, Age_50_59 = ifelse(Age >= 50 & Age<60, 1,0))
#dataset <- mutate(dataset, Age_aboe_60 = ifelse(Age >= 60, 1,0))
#dataset$Age <- NULL

#dataset <- mutate(dataset, Malnutrition = ifelse(BMI < 18.5, 1,0))
#dataset$BMI <- NULL

library(caTools)
set.seed(123)
split = sample.split(dataset$CKD, SplitRatio = 0.6667)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
split2 = sample.split(test_set$CKD, SplitRatio = 0.5)
test_set1 = subset(test_set, split2 == TRUE)
test_set2 = subset(test_set, split2 == FALSE)
write.csv(dataset, "dataset_final.csv")
write.csv(training_set, "training_set_final.csv")
write.csv(test_set, "test_set_final.csv")

qualityTrain = read.csv('4152_final_SMOTE.csv')
qualityTrain = training_set
qualityTrain$X <- NULL
qualityTrain$Unnamed..0 <- NULL

qualityTest = test_set

QualityLog = glm(CKD~.,data=qualityTrain, family=binomial)
summary(QualityLog)

confint(QualityLog)
plot(predict(QualityLog,type="response"), ylab = 'Probability', xlab = 'Subjects')
vif(QualityLog)
(vif(QualityLog))^.5 < 2

predictTrain = predict(QualityLog, type="response")
summary(predictTrain)
table(qualityTrain$CKD,predictTrain >= 0.07)

library(ROCR)

ROCRpred = prediction(predictTrain, qualityTrain$CKD)


ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)

plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
table(qualityTest$CKD,predictTest >= 0.2)

classify=ifelse(predictTest>.2,1,0)

c1=1300   # penalize me  $100 for a false positive
c2=100  #  penalize me $200 for a false negatives
acc = c_accuracy(qualityTest$CKD,classify)
print(acc)

cost=acc[7]*c1-acc[9]*c2
print(cost)

pred_val <-prediction(predictTest ,qualityTest$CKD)

# Calculating Areaunder Curve
perf_val <- performance(ROCRpred,"auc")
perf_val

# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Plot the ROC curve
perf_val2 <- performance(pred_val, "tpr", "fpr")
plot(perf_val2, col = "green", lwd = 1.5)

#Calculating KS statistics
ks1.tree <- max(attr(perf_val2, "y.values")[[1]] - (attr(perf_val2, "x.values")[[1]]))
ks1.tree
 
thresholds = data.frame(threshold = numeric(), accuracy = numeric(), cost = numeric(), tpr = numeric(), fpr = numeric(), tp = numeric(), tn = numeric(), fp = numeric(), fn = numeric(), fmeasure = numeric(), recall = numeric(), precision = numeric())
thresholds1 = data.frame(threshold = numeric(), accuracy = numeric(), cost = numeric(), tpr = numeric(), fpr = numeric(), tp = numeric(), tn = numeric(), fp = numeric(), fn = numeric(), fmeasure = numeric(), recall = numeric(), precision = numeric())
thresholds2 = data.frame(threshold = numeric(), accuracy = numeric(), cost = numeric(), tpr = numeric(), fpr = numeric(), tp = numeric(), tn = numeric(), fp = numeric(), fn = numeric(), fmeasure = numeric(), recall = numeric(), precision = numeric())
write.csv(thresholds1, 'thresholds1.csv')
threshold3 = data.frame(thresholds1$threshold, thresholds1$cost, thresholds2$threshold, thresholds2$cost, thresholds1$cost+thresholds2$cost)
for ( i in 0:20) {
  QualityLog = glm(CKD~.,data=qualityTrain, family=binomial)
  predictTrain = predict(QualityLog, type="response")
  #predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
  predictTest1 = predict(QualityLog, type = "response", newdata = test_set2)
  #classify=ifelse(predictTest>(i/40),1,0)
  classify1=ifelse(predictTest1>(i/100),1,0)
  #acc=c_accuracy(qualityTest$CKD,classify)
  acc=c_accuracy(test_set2$CKD,classify1)
  cost=acc[7]*c1-acc[9]*c2
  #thresholds[nrow(thresholds) + 1,] = c(i/40,acc[3], cost, acc[4], acc[5], acc[7], acc[8], acc[9], acc[10], acc[6], acc[1], acc[2])
  thresholds2[nrow(thresholds2) + 1,] = c(i/100,acc[3], cost, acc[4], acc[5], acc[7], acc[8], acc[9], acc[10], acc[6], acc[1], acc[2])
  
}
plot(thresholds1$threshold, thresholds1$cost, xlab = 'Threshold', ylab = 'Cost')
predictTest2 = predict(QualityLog, type = "response", newdata = test_set2)
#classify=ifelse(predictTest>(i/40),1,0)
classify2=ifelse(predictTest2>0.07,1,0)
#acc=c_accuracy(qualityTest$CKD,classify)
acc=c_accuracy(test_set2$CKD,classify2)
print(acc)
cost=acc[7]*c1-acc[9]*c2
print(cost)
thresholds2[nrow(thresholds2) + 1,] = c(i/40,acc[3], cost, acc[4], acc[5], acc[7], acc[8], acc[9], acc[10], acc[6], acc[1], acc[2])

test_2819_final = read.csv('imputed_2819_final.csv')
summary(test_2819_final)

predictTest2819 = predict(QualityLog, type = "response", newdata = test_2819_final)
#classify=ifelse(predictTest>(i/40),1,0)
predictTest2819
classify3=ifelse(predictTest2819>0.07,1,0)
print(classify3)
df = data.frame(test_2819_final,predictTest2819, classify3)
write.csv(df, 'predicted_2819_0.07.csv')
#acc=c_accuracy(qualityTest$CKD,classify)
acc=c_accuracy(test_2819_final$CKD,classify3)
print(acc)
cost=acc[7]*c1-acc[9]*c2
print(cost)

write.csv(thresholds, 'thresholds.csv')
plot(thresholds$cost, thresholds$threshold)

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}

## Step 4 - Explore your new model
formula(QualityLog)
summary(QualityLog)

# confidence intervals of the model coefficients (should not include 0 if p-value<.05)
confint.default(QualityLog)
confint(QualityLog)

## Step 5 - Hypotehsis test of model, Compare 2 models, Definition 5-3
## difference in deviance  *this is sort of like R-squared but different because we use 0 or 1 only.
with(QualityLog, null.deviance - deviance)
##df
with(QualityLog, df.null - df.residual)
## pvalue of difference
with(QualityLog, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# if <.05, then model is significant from a null model (model with no variables)
# note that you can do this incrementally by adding one variable at a time.

## Step 5 - Alternate. Ho:  Model Fits the Data, Ha: Model does not Fit, Definition 5-2
## devniance
-2*logLik(QualityLog)
## test
with(QualityLog, pchisq(deviance, df.residual, lower.tail = FALSE))


############################################################

coef(QualityLog)
##Get Concordance Stats
#Method 1
install.packages('InformationValue')
library(InformationValue)
Concordance_test<-Concordance(qualityTrain$CKD,predictTrain)
Concordance_test

##Testing Model Fit
#test null deviance with having a constant as the only indep parameter
logregnull=glm(CKD ~ 1,family=binomial(logit),data=qualityTrain)
summary(logregnull)

chi.sq=logregnull$deviance - QualityLog$deviance
pchisq(chi.sq,16,lower.tail=FALSE)

anova(logregnull,QualityLog,test="Chisq")

#### Preparing Gains/Lift chart
library(ROCR)
library('rpart')

gain.chart(predictTrain,qualityTrain$CKD)

##Calculate GINI
gini.coeff(numericdata$predicted)

library(car)
plot(cooks.distance(QualityLog),pch=16)
identify(1:nrow(qualityTrain),cooks.distance(QualityLog))

vif(QualityLog)
outlierTest(QualityLog)
avPlot(QualityLog,"Age")

#plot glm
library(ggplot2)
ggplot(qualityTrain, aes(x=Age, y=CKD)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

# Plotting Lift curve

pred_val <-prediction(predictTest2 ,test_set2$CKD)
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

#Calculating KS statistics
ks1.tree <- max(attr(perf_val2, "y.values")[[1]] - (attr(perf_val2, "x.values")[[1]]))
ks1.tree
perf_val2 <- performance(pred_val, "tpr", "fpr")
