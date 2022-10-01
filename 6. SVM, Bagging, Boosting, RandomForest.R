fire = read.csv("Algerian_forest_fires_dataset_UPDATE.csv",header=T) 
fire <- fire[,-c(1,2,3)]
fire$Classes = factor(fire$Classes)

set.seed(123)
V = 2
n =  NROW(fire)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
fire.train = fire[ii,]
fire.test  = fire[-ii,]


## Boosting

set.seed(1234)
my.control = rpart.control(xval=0, cp=0, maxdepth=1)
fit = boosting(Classes~., data=fire.train, boos=T, mfinal=50, control=my.control)

fit$trees
sort(fit$importance, decreasing=T)
print(fit$importance)
importanceplot(fit)

## Predicting and Evaluating

pred = predict.boosting(fit, newdata=fire.test)
cutoff = 0.5
yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
ctable = table(fire.test$Classes, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

pred2 = predict.boosting(fit, newdata=fire.train)$prob
pred = prediction(pred2[,2], fire.train$Classes)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Boosting","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict.boosting(fit, newdata=fire.test)$prob
pred = prediction(pred2[,2], fire.test$Classes)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Boosting","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.test = 0
cutoff = 0.5

set.seed(12345)
id = sample(1:V, nrow(fire), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning
  
  fire.train = fire[id != i,] 
  fire.test = fire[id == i,] 
  
  ## Boosting
  
  my.control = rpart.control(xval=0, cp=0, maxdepth=1)
  fit = boosting(Classes~., data=fire.train, boos=T, mfinal=50, control=my.control)
  
  ## Predicting and Evaluating
  
  pred = predict.boosting(fit, newdata=fire.test)
  yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(fire.test$Classes != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END

library(e1071)
fire = read.csv("Algerian_forest_fires_dataset_UPDATE.csv",header=T) 
fire <- fire[,-c(1,2,3)]
fire <- fire[,-c(1,2,3,8,9,10,11,12,13)]
fire$Classes = factor(fire$Classes)

set.seed(123)
V = 2
n =  NROW(fire)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
fire.train = fire[ ii,]
fire.test  = fire[-ii,]


## Model fitting

fit = svm(Classes ~., data = fire.train, kernel="linear", cost=1, gamma=0.5)
summary(fit)

tune(svm, train.x =subset(fire, select=-Classes), 
     train.y =fire$Classes, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

## Predicting

fit.pred = predict(fit, newdata = fire.test,  decision.values=TRUE)
pred = attributes(fit.pred)$decision.values

cutoff = 0.5
fit.yhat = ifelse(pred <= cutoff, 0, 1)

ctable = table(fire.test$Classes, fit.yhat,  dnn = c("Actual", "Predicted"))  
ctable


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

fit.pred = predict(fit, newdata = fire.train,  decision.values=TRUE)
pred = attributes(fit.pred)$decision.values
pred = prediction(pred, fire.train$Classes)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


fit.pred = predict(fit, newdata = fire.test,  decision.values=TRUE)
pred = attributes(fit.pred)$decision.values
pred = prediction(pred, fire.test$Classes)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("SVM","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC



##########################
# Computing the CV error

cutoff = 0.5

V = 3 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(fire), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning 
  
  fire.train = fire[id != i,] 
  fire.test  = fire[id == i,] 
  
  ## Fitting
  
  fit = svm(Classes ~., data = fire.train, kernel="linear")
  
  ## Predicting and Evaluating
  
  #fit.pred = predict(fit, newdata = german.test,  decision.values=TRUE)
  #pred.test = attributes(fit.pred)$decision.values
  #yhat.test = ifelse(pred.test <= cutoff, 0, 1)
  yhat.test = predict(fit, newdata = fire.test) 
  miss.err.test = miss.err.test + mean(fire.test$Classes != yhat.test)
  
}

cv.err.test = miss.err.test/V; cv.err.test # CV test error





