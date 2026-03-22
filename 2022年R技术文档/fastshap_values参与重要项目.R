

## Fast Approximate Shapley Values
library(fastshap)

#
# A projection pursuit regression (PPR) example
#

# Load the sample data; see ?datasets::mtcars for details
data(mtcars)

# Fit a projection pursuit regression model
fit <- lm(mpg ~ ., data = mtcars)

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap <- explain(fit, X = subset(mtcars, select = -mpg), nsim = 10, 
                pred_wrapper = predict)
shap

# Compute exact Shapley (i.e., LinearSHAP) values
shap <- explain(fit, exact = TRUE)
shap

# Shapley-based plots
library(ggplot2)
autoplot(shap)  # Shapley-based importance plot
autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
autoplot(shap, type = "contribution", row_num = 1)  # explain first row of X




# Load the sample data; see ?datasets::mtcars for details
data(mtcars)

# Fit a projection pursuit regression model
mtcars.ppr <- ppr(mpg ~ ., data = mtcars, nterms = 1)

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap <- explain(mtcars.ppr, X = subset(mtcars, select = -mpg), nsim = 10, 
                pred_wrapper = predict, adjust = TRUE)

# Visualize first explanation
preds <- predict(mtcars.ppr, newdata = mtcars)
x <- subset(mtcars, select = -mpg)[1L, ]  # take first row of feature values
force_plot(shap[1L, ], baseline = mean(preds), feature_values = x)

