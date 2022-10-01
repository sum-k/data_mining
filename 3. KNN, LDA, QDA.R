library(class)

fire <- read.csv("Algerian_forest_fires_dataset_UPDATE.csv",header=T)
fire
fire$Classes = factor(fire$Classes)

summary(fire)
par(mfrow = c(3,4))
hist(fire$Temperature, col="green")
hist(fire$RH, col="green")
hist(fire$Ws, col="green")
hist(fire$Rain, col="green")
hist(fire$FFMC, col="green")
hist(fire$DMC, col="green")
hist(fire$DC, col="green")
hist(fire$ISI, col="green")
hist(fire$BUI, col="green")
hist(fire$FWI, col="green")


str(fire)
dim(fire)
head(fire)
barplot(table(fire$Classes), col = "blue", xlab = "Fire", ylab = "Frequency")

y = fire[,14]
X = scale(fire[,c(4:13)])

## KNN with K=3

fit = knn(train=X, test=X, cl=y, k=3) 

yhat=fit
ctable = table(y, yhat, dnn=c("Actual", "Predicted"));  #classification table
miss.err = 1-sum(diag(ctable))/sum(ctable);
pred.acc = 1 - miss.err;
ctable
miss.err
pred.acc
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

set.seed(123)
V = 2
n =  NROW(fire)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
X.train = X[ii,]; X.test  = X[-ii,]
y.train = y[ii];  y.test  = y[-ii]


fit = knn(train=X.train, test=X.test, cl=y.train, k=3) 

yhat=fit
ctable = table(y.test, yhat, dnn=c("Actual", "Predicted"));  #classification table
miss.err = 1-sum(diag(ctable))/sum(ctable);
pred.acc = 1 - miss.err; 
ctable

miss.err
pred.acc
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5

set.seed(12345)
id = sample(1:V, nrow(fire), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning 
  
  X.train = X[id != i,]; X.test  = X[id == i,] 
  y.train = y[id != i];  y.test  = y[id == i] 
  
  ## Fitting
  fit = knn(train=X.train, test=X.test, cl=y.train, k=3) 
  
  ## Predicting and Evaluating
  yhat=fit
  miss.err.test = miss.err.test + mean(y.test != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error

install.packages("e1071")
library(e1071)
set.seed(1)
tune.out=tune.knn(x=X.test,y=as.factor(y.test),k=1:15)
plot(tune.out)





## LDA/QDA
library(MASS) #install.packages("MASS")
fire1 <- fire[,c(4:14)]

fit = lda(Classes ~., data=fire1) #LDA
plot(fit)
fit


fit1 = qda(Classes ~., data=fire1)


cutoff = 0.5
pred = predict(fit1, newdata=fire1)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(fire1$Classes, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

library(ROCR)
pred2 = predict(fit1, newdata=fire1)$posterior
pred = prediction(pred2[,2], fire1$Classes)
perf = performance(pred, "tpr","fpr")

par(mfrow=c(1,1))
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


set.seed(12345)
V = 2
n =  NROW(fire1)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
fire.train = fire1[ii,]
fire.test  = fire1[-ii,]

fit = lda(Classes ~., data=fire.train) #LDA
plot(fit)

fit1 = qda(Classes ~., data=fire.train)

cutoff = 0.5
pred = predict(fit1, newdata=fire.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(fire.test$Classes, yhat, dnn=c("Actual", "Predicted")); ctable

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


library(ROCR)
par(mfrow = c(1,2))

pred2 = predict(fit, newdata=fire.train)$posterior
pred = prediction(pred2[,2], fire.train$Classes)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit, newdata=fire.test)$posterior
pred = prediction(pred2[,2], fire.test$Classes)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5

set.seed(123)
id = sample(1:V, nrow(fire1), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  fire.train = fire1[id != i,] 
  fire.test  = fire1[id == i,] 
  
  ## LDA/QDA
  #fit = lda(Classes ~., data=fire.train)
  fit = qda(Classes ~., data=fire.train)
  
  ## Predicting and Evaluating
  pred = predict(fit, newdata=fire.test)$posterior
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(fire.test$Classes != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error

install.packages("klaR")
library(klaR)
partimat(Classes ~ ., data = fire1, method = "lda") 
partimat(Classes ~ ., data = fire1, method = "lda", plot.matrix = TRUE, imageplot = FALSE)
