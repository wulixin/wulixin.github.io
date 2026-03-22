
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

library("DALEX")
apartments_lm <- lm(m2.price ~ ., data = apartments)
anova(apartments_lm)

# Random forest model
library("randomForest")
set.seed(72)
apartments_rf <- randomForest(m2.price ~ ., data = apartments)


#Support vector machine model

library("e1071")
apartments_svm <- svm(m2.price ~ construction.year + surface + floor + 
                        no.rooms + district, data = apartments)


# Models’ predictions

apartments_test$m2.price[1:6]


predict(apartments_lm, apartments_test[1:6,])


predict(apartments_rf, apartments_test[1:6,])

predict(apartments_svm, apartments_test[1:6,])


predicted_apartments_lm <- predict(apartments_lm, apartments_test)

sqrt(mean((predicted_apartments_lm - apartments_test$m2.price)^2))


predicted_apartments_rf <- predict(apartments_rf, apartments_test)
sqrt(mean((predicted_apartments_rf - apartments_test$m2.price)^2))


###Models’ explainers

apartments_lm_exp <- explain(model = apartments_lm, 
                             data = apartments_test[,-1], 
                             y = apartments_test$m2.price, 
                             label = "Linear Regression")
apartments_rf_exp <- explain(model = apartments_rf, 
                             data = apartments_test[,-1], 
                             y = apartments_test$m2.price, 
                             label = "Random Forest")
apartments_svm_exp <- explain(model = apartments_svm, 
                              data = apartments_test[,-1], 
                              y = apartments_test$m2.price, 
                              label = "Support Vector Machine")


########

titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
titanic_rf <- archivist:: aread("pbiecek/models/4e0fc")
(henry <- archivist::aread("pbiecek/models/a6538"))

henry
#install.packages("ExplainPrediction")
library("DALEX")
library("randomForest")
explain_rf <- DALEX::explain(model = titanic_rf,  
                             data = titanic_imputed[, -9],
                             y = titanic_imputed$survived == "yes", 
                             label = "Random Forest")


bd_rf <- predict_parts(explainer = explain_rf,
                       new_observation = henry,
                       type = "break_down_interactions")
bd_rf


plot(bd_rf)

#####shap_henry 

shap_henry <- predict_parts(explainer = explain_rf, 
                            new_observation = henry, 
                            type = "shap",
                            B = 25)

shap_henry


plot(shap_henry)

plot(shap_henry, show_boxplots = FALSE) 


set.seed(1)
library("DALEXtra")
library("lime")
library(mlr)
library(parsnip)


titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
titanic_rf <- archivist:: aread("pbiecek/models/4e0fc")
henry <- archivist::aread("pbiecek/models/a6538")


library("randomForest")
library("DALEX")
explain_rf <- DALEX::explain(model = titanic_rf,  
                             data = titanic_imputed[, -9],
                             y = titanic_imputed$survived == "yes", 
                             label = "Random Forest")
predict(explain_rf, henry)


id_rf <- predict_diagnostics(explainer = explain_rf,
                             new_observation = henry,
                             neighbours = 100)
id_rf


plot(id_rf) 


id_rf_age <- predict_diagnostics(explainer = explain_rf,
                                 new_observation = henry,
                                 neighbours = 10,
                                 variables = "age")

plot(id_rf_age)



id_rf_class <- predict_diagnostics(explainer = explain_rf,
                                   new_observation = henry,
                                   neighbours = 10,
                                   variables = "class")


plot(id_rf_class) 


##### to compute CP-profile oscillations and the estimated value of the variable-importance measure as defined

oscillations_uniform <- predict_parts(explainer = explain_rf, 
                                      new_observation = henry, 
                                      type = "oscillations_uni")
oscillations_uniform


library(ggraph)
library(ggtext)
plot(oscillations_uniform)

oscillations_uniform$`_ids_` <- "Henry"
plot(oscillations_uniform) +
  ggtitle("Ceteris-paribus Oscillations", 
          "Expectation over uniform distribution (unique values)") 



oscillations_equidist <- predict_parts(explain_rf, henry, 
                                       variable_splits = list(age = seq(0, 65, 0.1),
                                                              fare = seq(0, 200, 0.1),
                                                              gender = unique(titanic_imputed$gender),
                                                              class = unique(titanic_imputed$class)), 
                                       type = "oscillations")
oscillations_equidist


oscillations_equidist$`_ids_` <- "Henry"
plot(oscillations_equidist) + 
  ggtitle("Ceteris-paribus Oscillations", 
          "Expectation over specified grid of points")

oscillations_equidist$`_ids_` <- "Henry"
plot(oscillations_equidist) + 
  ggtitle("Ceteris-paribus Oscillations", 
          "Expectation over specified grid of points")


library("DALEX")
library("rms")
explain_lmr <- explain(model = titanic_lmr, 
                       data  = titanic_imputed[, -9],
                       y     = titanic_imputed$survived == "yes",
                       type = "classification",
                       label = "Logistic Regression")

library("randomForest")
explain_rf <- DALEX::explain(model = titanic_rf,  
                             data  = titanic_imputed[, -9],
                             y     = titanic_imputed$survived == "yes", 
                             label = "Random Forest")


cp_titanic_rf <- predict_profile(explainer = explain_rf, 
                                 new_observation = henry)
cp_titanic_rf


library("ggplot2")
plot(cp_titanic_rf, variables = c("age", "fare")) +
  ggtitle("Ceteris-paribus profile", "") + ylim(0, 0.8)


plot(cp_titanic_rf, variables = c("class", "embarked"), 
     variable_type = "categorical", categorical_type = "bars") +
  ggtitle("Ceteris-paribus profile", "") 


variable_splits = list(age = seq(0, 70, 0.1), 
                       fare = seq(0, 100, 0.1))
cp_titanic_rf <- predict_profile(explainer = explain_rf, 
                                 new_observation = henry,
                                 variable_splits = variable_splits)


plot(cp_titanic_rf, variables = c("age", "fare")) + 
  ggtitle("Ceteris-paribus profile", "") 


(johnny_d <- archivist::aread("pbiecek/models/e3596"))

cp_titanic_rf2 <- predict_profile(explainer = explain_rf, 
                                  new_observation = rbind(henry, johnny_d),
                                  variable_splits = variable_splits)
library(ingredients)
plot(cp_titanic_rf2, color = "_ids_", variables = c("age", "fare")) + 
  
  scale_color_manual(name = "Passenger:", breaks = 1:2, 
                     values = c("#4378bf", "#8bdcbe"), 
                     labels = c("henry" , "johny_d")) 


cp_titanic_rf <- predict_profile(explain_rf, henry)
cp_titanic_lmr <- predict_profile(explain_lmr, henry)


plot(cp_titanic_rf, cp_titanic_lmr, color = "_label_",  
     variables = c("age", "fare")) +
  ggtitle("Ceteris-paribus profiles for Henry", "") 


#install.packages("localModel")
library("localModel")
locMod_johnny <- predict_surrogate(explainer = titanic_rf_exp, 
                                   new_observation = johnny_d, 
                                   size = 1000, 
                                   seed = 1,
                                   type = "localModel")


locMod_johnny[,1:3]


plot_interpretable_feature(locMod_johnny, "age")


plot(locMod_johnny)

#install.packages("iml")
library("DALEXtra")
library("iml")
iml_johnny <- predict_surrogate(explainer = titanic_rf_exp, 
                                new_observation = johnny_d, 
                                k = 3, 
                                type = "iml")
iml_johnny$results[,c(1:5,7)]




plot(iml_johnny) 


