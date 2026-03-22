

library('DALEX')

library("archivist")

archivist::aread("pbiecek/models/ceb40")

#R Interface to Keras
library(keras)

#A Common Api to Modeling and Analysis Functions
library("parsnip")
#regression modeling strategies 
library(rms)
#Generalized Boosted Regression Models
library(gbm)

###Models for RMS Titanic, snippets for R

##Logistic regression model
library("rms")
titanic_lmr <- lrm(survived == "yes" ~ gender + rcs(age) + class +
                     sibsp + parch + fare + embarked, titanic)

## Random forest model
#这个地方给书本打个布丁
titanic<-titanic%>%na.omit()
library("randomForest")
set.seed(1313)
titanic_rf <- randomForest(survived ~ class + gender + age + 
                             sibsp + parch + fare + embarked, data = titanic)

###Gradient boosting model
library("gbm")
set.seed(1313)
titanic_gbm <- gbm(survived == "yes" ~ class + gender + age + 
                     sibsp + parch + fare + embarked, data = titanic, 
                   n.trees = 15000, distribution = "bernoulli")


###Support vector machine model
library("e1071")
titanic_svm <- svm(survived == "yes" ~ class + gender + age + 
                     sibsp + parch + fare + embarked, data = titanic, 
                   type = "C-classification", probability = TRUE)

###Models’ predictions
johnny_d <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", 
                                   "deck crew", "engineering crew", 
                                   "restaurant staff", "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8, sibsp = 0, parch = 0, fare = 72,
  embarked = factor("Southampton", levels = c("Belfast",
                                              "Cherbourg","Queenstown","Southampton")))


(pred_lmr <- predict(titanic_lmr, johnny_d, type = "fitted"))

(pred_rf <- predict(titanic_rf, johnny_d, type = "prob"))


(pred_gbm <- predict(titanic_gbm, johnny_d, type = "response", 
                     n.trees = 15000))

(pred_svm <- predict(titanic_svm, johnny_d, probability = TRUE))


henry <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", 
                                   "deck crew", "engineering crew", 
                                   "restaurant staff", "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 47, sibsp = 0, parch = 0, fare = 25,
  embarked = factor("Cherbourg", levels = c("Belfast",
                                            "Cherbourg","Queenstown","Southampton")))


predict(titanic_lmr, henry, type = "fitted")
predict(titanic_rf, henry, type = "prob")[,2]
predict(titanic_gbm, henry, type = "response", n.trees = 15000)
attr(predict(titanic_svm, henry, probability = TRUE),"probabilities")[,2]


#############

titanic_lmr_exp <- explain(model = titanic_lmr, 
                           data = titanic[, -9],
                           y = titanic$survived == "yes", 
                           label = "Logistic Regression",
                           type = "classification")
titanic_rf_exp <- explain(model = titanic_rf, 
                          data = titanic[, -9],
                          y = titanic$survived == "yes", 
                          label = "Random Forest")
titanic_gbm_exp <- explain(model = titanic_gbm, 
                           data = titanic[, -9],
                           y = titanic$survived == "yes", 
                           label = "Generalized Boosted Regression")
titanic_svm_exp <- explain(model = titanic_svm, 
                           data = titanic[, -9],
                           y = titanic$survived == "yes", 
                           label = "Support Vector Machine")



#######################

titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
titanic_rf <- archivist:: aread("pbiecek/models/4e0fc")
(henry <- archivist::aread("pbiecek/models/a6538"))

library("randomForest")
library("DALEX")
explain_rf <- DALEX::explain(model = titanic_rf,  
                             data = titanic_imputed[, -9],
                             y = titanic_imputed$survived == "yes", 
                             label = "Random Forest")

########Break Down profile 

bd_rf <- predict_parts(explainer = explain_rf,
                       new_observation = henry,
                       type = "break_down")
bd_rf 


plot(bd_rf)



########

bd_rf_order <- predict_parts(explainer = explain_rf,
                             new_observation = henry, 
                             type = "break_down",
                             order = c("class", "age", "gender", "fare", 
                                       "parch", "sibsp", "embarked"))
plot(bd_rf_order, max_features = 3) 



#######

bd_rf_distr <- predict_parts(explainer = explain_rf,
                             new_observation = henry, 
                             type = "break_down",
                             order = c("age", "class", "fare", "gender", 
                                       "embarked", "sibsp", "parch"),
                             keep_distributions = TRUE)
plot(bd_rf_distr, plot_distributions = TRUE) 







