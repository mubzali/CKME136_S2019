
# ******************************************************************************* #
# ********  Program:  Data Analytics, Big Data and Predictive Analytics  ******** #
# ********  Course:   CKME136 - Data Analytics: Capstone Course          ******** #
# ********  Name: Mubashir Ali                                           ******** #
# ********  Student ID: 500932569                                        ******** #
# ********  Email: mian1.ali@ryerson.ca                                  ******** #
# ********  Section: Model Selection                                     ******** #
# ******************************************************************************* #

# ***** Define training and testing datasets ***** #

sdata0708 <- filteredD[sample(nrow(filteredD), 25000), ]
sample0708 <- sample(nrow(sdata0708), floor(nrow(sdata0708)*0.5))
head(sample0708)

train <- sdata0708[sample0708,]
head(train)
test <- sdata0708[-sample0708,]
head(test)

library(rpart)
filteredtrain <- train[, !(colnames(train) %in% c("LateAircraft"))]
filteredtest <- test[, !(colnames(test) %in% c("LateAircraft"))]
head(filteredtrain)
    
# ***** RMSE Function ***** #

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# ***** Algorithm Selection & Tuning ***** #
library(ggplot2)
library(lattice)
library(caret)
library(Epi)

# ***** SVM in e1071, Radial ***** #
library(e1071)

# ***** 10-Fold Cross-Validation ***** #
summary(filteredtrain)
svm.modelR <- tune.svm(ArrDelL ~., data = filteredtrain, gamma = 10^(-5:-2), cost = 10^(-2:0))
summary(svm.modelR)

plot(svm.modelR, transform.x = log10, xlab = expression(log[10](gamma)), ylab = "C")

bestGamma <- svm.modelR$best.parameters[[1]]
bestC <- svm.modelR$best.parameters[[2]]

model <- svm(ArrDelL ~ ., data = filteredtrain, cost = bestC, gamma = bestGamma, cross = 10)
summary(model)

model <- svm(ArrDelL ~ ., data = filteredtrain, cost = 0.01, gamma = 0.01)
summary(model)

predR <- predict(model, filteredtest)
predR[predR > .5] = 1
predR[predR <= .5] = 0  
table(filteredtest$ArrDelL, predR)

plot(model$residuals)
plot(model$fitted)
plot(predR)

acc <- table(predR, filteredtest$ArrDelL)
classAgreement(acc)
table(filteredtest$ArrDelL, predR)
head(filteredtest)

errorR <- filteredtest$ArrDelL - predR
svrRpredictionRMSE <- rmse(errorR)
print(svrRpredictionRMSE)

confusionMatrix(table(predR, filteredtest$ArrDelL))

misClasificErrorSVM <- mean(predR != filteredtest$ArrDelL)
print(paste('Accuracy', 1-misClasificErrorSVM))

library(ROCR)
pS <- predict(model, data = filteredtest, type = "response")
pSr <- prediction(pS, filteredtest$ArrDelL)
pSrf <- performance(pSr, measure = "tpr", x.measure = "fpr")
plot(pSrf)


# ***** Naive Bayes ***** #
library(ggplot2)
library(lattice)
library(caret)
library(Epi)

model.NB <- naiveBayes(ArrDelL ~ ., data = filteredtrain)
summary(model.NB)

predNB <- predict(model.NB, filteredtest[1:6250,-1], type = "raw")
summary(predNB)

predNB[predNB > .5] = 1
predNB[predNB <= .5] = 0  
table(predNB, filteredtest$ArrDelL)
confusionMatrix(table(predNB, filteredtest$ArrDelL))

misClasificErrorNB <- mean(predNB != filteredtest$ArrDelL)
print(paste('Accuracy', 1-misClasificErrorNB))


# ***** Logistic Regression ***** #

reg = lm(ArrDelL ~ ., data = filteredtrain)
summary(reg)
reg.model <- glm(ArrDelL ~., family = binomial(link='logit'), data = filteredtrain)
summary(reg.model)

anova(reg.model, test = "Chisq")

reg.fit <- predict(reg.model, data = filteredtest, type = 'response')
reg.fit <- ifelse(reg.fit > 0.5, 1, 0)
summary(reg.fit)

plot(reg.model$fitted.values)
plot(reg$fitted.values)
plot(reg.fit)
plot(reg$residuals)
confusionMatrix(table(reg.fit, filteredtest$ArrDelL))

# ***** Test Accuracy ***** #
misClasificError <- mean(reg.fit != filteredtest$ArrDelL)
print(paste('Accuracy', 1-misClasificError))

# Accuracy:         The amount of correct classifications / the total amount of classifications.
# Confusion Matrix: A tabulation of the predicted class (usually vertically) against the actual class (thus horizontally).

library(ROCR)
p <- predict(reg.model, data = filteredtest, type = "response")
pr <- prediction(p, filteredtest$ArrDelL)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
