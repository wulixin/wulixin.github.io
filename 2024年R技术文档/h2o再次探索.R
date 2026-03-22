
library(h2o)

# Start the H2O cluster (locally)
h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])

# Run AutoML for 20 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

aml@leader



library(h2o)
h2o.init()

# Import the birds dataset into H2O:
birds <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/pca_test/birds.csv")

# Split the dataset into a train and valid set:
birds_split <- h2o.splitFrame(birds, ratios = 0.8, seed = 1234)
train <- birds_split[[1]]
valid <- birds_split[[2]]

# Build and train the model:
birds_pca <- h2o.prcomp(training_frame = train,
                        k = 5,
                        use_all_factor_levels = TRUE,
                        pca_method = "GLRM",
                        transform = "STANDARDIZE",
                        impute_missing = TRUE)

# Generate predictions on a validation set (if necessary):
pred <- h2o.predict(birds_pca, newdata = valid)

