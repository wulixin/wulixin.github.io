##################################################
#
#          rfe_ga_post
#
##################################################

data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]

head(bc)
str(bc)
bc<-bc[,-1]
for(i in 1:9) {
  bc[,i]<-as.numeric(as.character(bc[,i]))
}

#Change Y value to 1 and 0
bc$Class<-ifelse(bc$Class=="malignant",1,0)
bc$Class<-factor(bc$Class,levels=c(0,1))

library(oneR)
#######################Principal Component Analysis (PCA)

library(ggplot2)
library(ggrepel)
my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "navy", color = "navy", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "white"),
      legend.position = "right",
      legend.justification = "top", 
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

theme_set(my_theme())


# function for PCA plotting
library(pcaGoPromoter)
library(ellipse)

pca_func <- function(data, groups, title, print_ellipse = TRUE) {
  
  # perform pca and extract scores
  pcaOutput <- pca(data, printDropped = FALSE, scale = TRUE, center = TRUE)
  pcaOutput2 <- as.data.frame(pcaOutput$scores)
  
  # define groups for plotting
  pcaOutput2$groups <- groups
  
  # when plotting samples calculate ellipses for plotting (when plotting features, there are no replicates)
  if (print_ellipse) {
    
    centroids <- aggregate(cbind(PC1, PC2) ~ groups, pcaOutput2, mean)
    conf.rgn  <- do.call(rbind, lapply(unique(pcaOutput2$groups), function(t)
      data.frame(groups = as.character(t),
                 ellipse(cov(pcaOutput2[pcaOutput2$groups == t, 1:2]),
                         centre = as.matrix(centroids[centroids$groups == t, 2:3]),
                         level = 0.95),
                 stringsAsFactors = FALSE)))
    
    plot <- ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
      geom_polygon(data = conf.rgn, aes(fill = groups), alpha = 0.2) +
      geom_point(size = 2, alpha = 0.6) + 
      scale_color_brewer(palette = "Set1") +
      labs(title = title,
           color = "",
           fill = "",
           x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
           y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance"))
    
  } else {
    
    # if there are fewer than 10 groups (e.g. the predictor classes) I want to have colors from RColorBrewer
    if (length(unique(pcaOutput2$groups)) <= 10) {
      
      plot <- ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
        geom_point(size = 2, alpha = 0.6) + 
        scale_color_brewer(palette = "Set1") +
        labs(title = title,
             color = "",
             fill = "",
             x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
             y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance"))
      
    } else {
      
      # otherwise use the default rainbow colors
      plot <- ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
        geom_point(size = 2, alpha = 0.6) + 
        labs(title = title,
             color = "",
             fill = "",
             x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
             y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance"))
      
    }
  }
  
  return(plot)
  
}

library(gridExtra)
library(grid)

p1 <- pca_func(data = t(bc[, 1:10]), groups = as.character(bc$Class), title = "Breast cancer dataset 1: Samples")
p2 <- pca_func(data = bc[, 1:10], groups = as.character(colnames(bc[, 1:10])), title = "Breast cancer dataset 1: Features", print_ellipse = FALSE)
grid.arrange(p1, p2, ncol = 2)

###Hclust
h_1 <- hclust(dist(t(bc[, 1:10]), method = "euclidean"), method = "complete")
plot(h_1)


colnames(bc)
#########################Feature importance
library(caret)
library(doParallel) # parallel processing
registerDoParallel()

# prepare training scheme
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

feature_imp <- function(model, title) {
  
  # estimate variable importance
  importance <- varImp(model, scale = TRUE)
  
  # prepare dataframes for plotting
  importance_df_1 <- importance$importance
  importance_df_1$group <- rownames(importance_df_1)
  
  importance_df_2 <- importance_df_1
  importance_df_2$Overall <- 0
  
  importance_df <- rbind(importance_df_1, importance_df_2)
  
  plot <- ggplot() +
    geom_point(data = importance_df_1, aes(x = Overall, y = group, color = group), size = 2) +
    geom_path(data = importance_df, aes(x = Overall, y = group, color = group, group = group), size = 1) +
    theme(legend.position = "none") +
    labs(
      x = "Importance",
      y = "",
      title = title,
      subtitle = "Scaled feature importance",
      caption = "\nDetermined with Random Forest and
      repeated cross validation (10 repeats, 10 times)"
    )
  
  return(plot)
  
}

set.seed(27)
##随机森林

imp_1 <- train(Class ~ ., data =bc, method = "rf", preProcess = c("scale", "center"), trControl = control)

p1 <- feature_imp(imp_1, title = "Breast cancer dataset 1")
p1
##grid.arrange(p1, p2, p3, ncol = 3, widths = c(0.3, 0.35, 0.35))
library(lares)
mplot_importance(var = imp_1$finalModel$xNames, imp = imp_1$finalModel$importance,
                        subtitle = "cancer data")



set.seed(27)

bc_index <- createDataPartition(bc$Class, p = 0.7, list = FALSE)
bc_train <- bc[bc_index, ]
bc_test  <- bc[-bc_index, ]

###############Correlation
library(corrplot)

# calculate correlation matrix
corMatMy <- cor(bc_train[,-10])
corrplot(corMatMy, order = "hclust")

#Apply correlation filter at 0.70,
highlyCor <- colnames(bc_train[, -10])[findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)]

highlyCor
#then we remove these variables
bc_cor <- bc_train[, which(!colnames(bc_train) %in% highlyCor)]


###########################Recursive Feature Elimination (RFE)

# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function with cross validation
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# run the RFE algorithm
results_1 <- rfe(x = bc_train[, -10], y = bc_train$Class, sizes = c(1:9), rfeControl = control)

# chosen features
predictors(results_1)

# subset the chosen features
bc_rfe <- bc_train[, c(10, which(colnames(bc_train) %in% predictors(results_1)))]

#########################Genetic Algorithm (GA)

library(dplyr)

ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
                       method = "cv",    # 10 fold cross validation
                       genParallel = TRUE, # Use parallel programming
                       allowParallel = TRUE)

lev <- c("malignant", "benign")     # Set the levels

set.seed(27)
model_1 <- gafs(x = bc_train[, -10], y = bc_train$Class,
                iters = 10, # generations of algorithm
                popSize = 5, # population size for each generation
                levels = lev,
                gafsControl = ga_ctrl)

plot(model_1) # Plot mean fitness (AUC) by generation


model_1$ga$final

bc_ga <- bc_train[, c(10, which(colnames(bc_train) %in% model_1$ga$final))]


######################Model comparison
library(gplots)

venn_list <- list(cor = colnames(bc_cor),
                  rfe = colnames(bc_rfe),
                  ga = colnames(bc_ga))

venn <- venn(venn_list)

####cor_data
set.seed(27)
model_bc_cor <- train(Class ~ .,
                           data = bc_cor,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))

cm_cor_1 <- confusionMatrix(predict(model_bc_cor, bc_test[, -10]), bc_test$Class)
cm_cor_1

###rfe
model_bc_rfe <- train(Class~ .,
                     data = bc_rfe,
                     method = "rf",
                     preProcess = c("scale", "center"),
                     trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
cm_rfe_1 <- confusionMatrix(predict(model_bc_rfe, bc_test[, -10]), bc_test$Class)
cm_rfe_1

set.seed(27)
model_bc_ga <- train(Class~ .,
                           data = bc_ga,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))

cm_ga_1 <- confusionMatrix(predict(model_bc_ga, bc_test[, -10]), bc_test$Class)
cm_ga_1


model_bc_all <- train(Class ~ .,
                           data = bc_train,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
cm_all_1 <- confusionMatrix(predict(model_bc_all, bc_test[, -10]), bc_test$Class)
cm_all_1
##########Overall model parameters

overall <- data.frame(dataset = rep("1", each = 4),
                      model = rep(c("all","cor", "rfe", "ga"), 1),
                      rbind(cm_all_1$overall,
                            cm_cor_1$overall,
                            cm_rfe_1$overall,
                            cm_ga_1$overall))


library(tidyr)
overall_gather <- overall[, 1:4] %>%
  gather(measure, value, Accuracy:Kappa)

byClass<- data.frame(dataset = rep("1", each = 4),
                               model = rep(c("all","cor","rfe", "ga"), 1),
                               rbind(cm_all_1$byClass,
                                     cm_cor_1$byClass,
                                     cm_rfe_1$byClass,
                                     cm_ga_1$byClass))         
      
byClass_gather <- byClass[, c(1:4, 7)] %>%
  gather(measure, value, Sensitivity:Precision)

overall_byClass_gather <- rbind(overall_gather, byClass_gather)
overall_byClass_gather <- within(overall_byClass_gather, model <- factor(model, levels = c("all","cor", "rfe", "ga")))

ggplot(overall_byClass_gather, aes(x = model, y = value, color = measure, shape = measure, group = measure)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_path(alpha = 0.7) +
  scale_colour_brewer(palette = "Set1") +
  facet_grid(dataset ~ ., scales = "free_y") +
  labs(
    x = "Feature Selection method",
    y = "Value",
    color = "",
    shape = "",
    title = "Comparison of feature selection methods",
    subtitle = "",
    caption = "\nBreast Cancer Wisconsin (Diagnostic) Data Sets: 1
    Street et al., 1993;
    all: no feature selection
    cor: features with correlation > 0.7 removed
    rfe: Recursive Feature Elimination
    ga: Genetic Algorithm"
  )
