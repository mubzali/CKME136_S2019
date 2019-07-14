
# ******************************************************************************* #
# ********  Program:  Data Analytics, Big Data and Predictive Analytics  ******** #
# ********  Course:   CKME136 - Data Analytics: Capstone Course          ******** #
# ********  Name: Mubashir Ali                                           ******** #
# ********  Student ID: 500932569                                        ******** #
# ********  Email: mian1.ali@ryerson.ca                                  ******** #
# ********  Section: Descriptive Analysis                                ******** #
# ******************************************************************************* #

library(Boruta)
library(ranger)

set.seed(100)
boruta.train <- Boruta(ArrDelL~., data = filteredtrain, doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n", ylim = c(0,350))
lz <- lapply(1:ncol(boruta.train$ImpHistory),function(i)
      boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
labels <- sort(sapply(lz, median))
axis(side = 1,las = 2,labels = names(labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

getSelectedAttributes(boruta.train, withTentative = F)

boruta.df <- attStats(boruta.train)
print(boruta.df)