library(neuralnet) #install.packages("neuralnet")
library(dummy) #install.packages("dummy")


## Data reading

fire = read.csv("Algerian_forest_fires_dataset_UPDATE.csv",header=T) 
fire <- fire[,-c(1,2,3)]
#fire$Classes = factor(fire$Classes)

max1 = apply(fire, 2, max) 
min1 = apply(fire, 2, min)

fdat = scale(fire, center = min1, scale = max1 - min1) #Standardization
fdat = as.data.frame(fdat)

set.seed(123)
V = 2
n =  NROW(fdat)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
fire.train = fdat[ii,]
fire.test  = fdat[-ii,]


## Model fitting


fn = names(fire.train)
f = as.formula(paste("Classes ~", paste(fn[!fn %in% "Classes"], collapse = " + ")))
fit.nn = neuralnet(f, data = fire.train, hidden=c(2,3,8), linear.output=F) #3 hidden neurons

plot(fit.nn)

## Predicting

cutoff = 0.5
p.test.nn = predict(fit.nn, fire.test)
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)

ctable = table(fire.test$Classes, yhat.test.nn, dnn=c("Actual","Predicted"))
print(ctable) # classification table  


## Evaluating

ctable = table(fire.test$Classes, yhat.test.nn, dnn=c("Actual","Predicted"))
miss.err = 1-sum(diag(ctable))/sum(ctable)
print(ctable)

miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

pred<-compute(fit.nn, fire.test)
plot(fire.test~pred$net.result)

## ROC and AUC

library(ROCR)
cutoff = 0.5
par(mfrow = c(2,2))

#Train 
p.train.nn = predict(fit.nn, fire.train)
yhat.train.nn = ifelse(p.train.nn > cutoff, 1, 0)
pred = prediction(p.train.nn, fire.train$Classes)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.4, 0.3, legend = c("Neural Networks","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


#Test
p.test.nn = predict(fit.nn, fire.test)
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)
pred = prediction(p.test.nn, fire.test$Classes)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.4, 0.3, legend = c("Neural Networks","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


## 

fire = read.csv("Algerian_forest_fires_dataset_UPDATE.csv",header=T) 
fire <- fire[,-c(1,2,3,8,9,10,11,12,13)]
fire$Classes = factor(fire$Classes)


set.seed(12)
V = 2
n =  NROW(fire)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
fire.train = fire[ii,]
fire.test  = fire[-ii,]

library(rpart)

## Growing a tree

fit = rpart(Classes ~., data=fire.train, method="class", control = rpart.control(xval=10, cp=0))
fit
summary(fit)
plot(fit);  text(fit)


## Pruning

tmp = printcp(fit)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE, all = T)


## Predicting and Evaluating

cutoff = 0.5
pred = predict(fit.pruned, newdata=fire.test, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(fire.test$Classes, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

pred2 = predict(fit.pruned, newdata=german.train, type="prob") #prediction
pred = prediction(pred2[,2], german.train$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit.pruned, newdata=german.test, type="prob") #prediction
pred = prediction(pred2[,2], german.test$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

