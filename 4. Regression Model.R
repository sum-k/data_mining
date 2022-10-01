attach(fire)
X = data.frame(Temperature,RH,Ws,Rain,FFMC,DMC,DC,ISI,BUI,FWI) 
X = as.matrix(X)
cor(X)
car::vif(glm(Classes~Temperature + RH + Ws + Rain+FFMC+DMC+DC+ISI+BUI+FWI))
pairs(X)
pairs(X, panel=panel.smooth)

## Data partitioning
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

library(dplyr)
fire %>% select_at(vars(Temperature,RH,Ws,Rain,FFMC,DMC,DC,ISI,BUI,FWI)) %>%
  chart.Correlation(histogram=TRUE)

install.packages("GGally")
library(GGally)
ggcorr(X, name="corr", label=T)


set.seed(123)
V = 2
n =  NROW(fire)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
fire.train = fire[ ii,]
fire.test = fire[-ii,]


## Model fitting

fit = glm(Classes ~ Temperature + RH + Ws + Rain+FFMC, data = fire.train, family = binomial(link = "logit"))
summary(fit)

fit2 = step(fit, direction = "both")
fit2$anova
summary(fit2)


## Predicting

fit2.pred = predict(fit2, newdata =  fire.test, type = "response") 
head(fit2.pred)

cutoff = 0.5
fit2.yhat = ifelse(fit2.pred <= cutoff, 0, 1)

ctable = table(fire.test$Classes, fit2.yhat,  dnn = c("Actual", "Predicted")) 
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

fit.pred = predict(fit, newdata = fire.train, type = "response") 
pred = prediction(fit.pred, fire.train$Classes)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


fit.pred = predict(fit, newdata = fire.test, type = "response") 
pred = prediction(fit.pred, fire.test$Classes)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC



##########################
# Computing the CV error

cutoff = 0.5

V = 10 #V-fold CV
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
  
  fit = glm(Classes~., data = fire.train, family = binomial(link = "logit"))
  fit2 = step(fit, direction="both", trace=FALSE) #Stepwise variable selection
  
  ## Predicting and Evaluating
  
  pred.test = predict(fit2, newdata=fire.test, type="response")
  yhat.test = ifelse(pred.test<= cutoff, 0, 1)
  miss.err.test = miss.err.test + mean(fire.test$Classes != yhat.test)
  
}

cv.err.test = miss.err.test/V; cv.err.test # CV test error



fire <- read.csv("Algerian_forest_fires_dataset_UPDATE.csv",header=T)
fire
fire$Classes = factor(fire$Classes)

# 회귀분석을 했는데, pvalue가 너무 높게 나와서 변수들간의 상관관계가 크다는 것을 알게 되었고, 
# 이를제거하기 위해 FWI지수들을 몇개 제거했다

fit = glm(Classes ~Temperature+RH+Ws+Rain, data=fire, family = binomial(link = "logit"))
summary(fit)

fit2 = step(fit, direction = "both")
fit2$anova
summary(fit2)


## Predicting

fit2.pred = predict(fit2, newdata = fire, type = "response") 

cutoff = 0.5
fit2.yhat = ifelse(fit2.pred <= cutoff, 0, 1)

ctable = table(fire$Classes, fit2.yhat,  dnn = c("Actual", "Predicted"))  
miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
pred.acc = 1 - miss.err #Prediction Accuracy



## Evaluating

ctable
miss.err
pred.acc  
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

#install.packages("ROCR")
library(ROCR)

fit.pred = predict(fit, newdata =  fire, type = "response") 
pred = prediction(fit.pred, fire$Classes)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

