# Filename: data-raw/xgb_compact.R
#
# This script trains the final XGBoost model and saves it as a binary file
# in inst/extdata/. This script should be run manually by the developer
# whenever the model needs to be regenerated.

# --- 1. SETUP ---

# Load all required packages
library(tidymodels)
library(xgboost)
library(doFuture)
library(here)

# Load the package's internal data and functions
devtools::load_all()

# Configure parallel processing
registerDoFuture()
plan(multisession, workers = parallel::detectCores(logical = FALSE) - 1)
options(tidymodels.dark = TRUE)

# --- 2. MODEL TRAINING & TUNING (Your code is perfect here) ---

# Split data
set.seed(123)
data_split <- initial_split(
  desplim_compactness_data,
  prop = 0.8,
  strata = compact
)
train_data <- training(data_split)
test_data <- testing(data_split)

# Recipe
model_recipe <- recipe(compact ~ ., data = train_data)

# XGBoost model specification
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  mtry = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) |>
  set_engine(
    "xgboost",
    objective = "reg:squarederror",
    verbose = 0
  ) |>
  set_mode("regression")

# XGBoost parameter search space
xgb_params <- parameters(
  trees(),
  tree_depth(),
  learn_rate(),
  finalize(mtry(), train_data),
  min_n(),
  loss_reduction(),
  sample_prop()
)

# Workflow and cross-validation
xgb_workflow <- workflow() |>
  add_recipe(model_recipe) |>
  add_model(xgb_spec)

set.seed(456)
cv_folds <- vfold_cv(train_data, v = 10)

# Bayesian tuning
set.seed(789)
tune_results <- tune_bayes(
  xgb_workflow,
  resamples = cv_folds,
  param_info = xgb_params,
  initial = 10,
  iter = 100,
  metrics = metric_set(rmse, mae),
  control = control_bayes(
    verbose_iter = TRUE,
    save_pred = FALSE,
    save_workflow = FALSE,
    no_improve = 20
  )
)

# --- 3. FINALIZATION & EXPORT ---

# Select parameters
best_params <- select_best(tune_results, metric = "rmse")
print(best_params)

# Finalise the workflow and fit
final_xgb_wf <- finalize_workflow(xgb_workflow, best_params)
final_fit <- last_fit(final_xgb_wf, data_split)

# Model metrics
test_metrics <- collect_metrics(final_fit)
print(test_metrics)

# Final workflow
final_trained_workflow <- extract_workflow(final_fit)

# Extract XGBoost model object
final_xgb_model_engine <- extract_fit_engine(final_trained_workflow)

# Ensure the destination directory exists
usethis::use_directory("inst/extdata")

# Save the final model
xgb.save(
  final_xgb_model_engine,
  here("inst", "extdata", "xgb_model.bin")
)