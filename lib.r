load_libraries <- function() {
  suppressWarnings(suppressMessages({
    library(caret)
    library(MASS)
    library(dplyr)
    library(car)
    library(DescTools)

  }))
}
load_libraries()

remove_nulls <- function(df) {
  cleaned_df <- na.omit(df)
  return(cleaned_df)
}

train_test_split <- function (df, partition_col = "Genre_factor") {
  set.seed(21)
  target <- df[[partition_col]]
  splitIndex <- createDataPartition(target, p = 0.75, list = FALSE)
  train_data <- df[splitIndex, ]
  test_data <- df[-splitIndex, ]
  return(list(train_data = train_data, test_data = test_data))
}

calculate_result <- function (model, test_data) {
  test_set <- test_data %>% select(-Gross)
  predictions <- predict(model, test_set)
  residuals <- test_data$Gross - predictions
  mse <- mean(residuals^2)
  return(mse)
}

run_model <- function (df) {
  split_result <- train_test_split(df)
  train_data <- split_result$train_data
  test_data <- split_result$train_data

  model <- lm("Gross ~ .", data = train_data)
  
  result <- calculate_result(model, test_data)

  return(list(model = model, result = result))
}


normal_test <- function(model) {
  shapiro_re <- shapiro.test(resid(model))
  t_test_re <- t.test(resid(model), mu = 0)
}


draw_qqplot <- function(data, title) {
  qqPlot(data, main=title)
}


yeo_johnson_trans <- function (df, numeric_cols) {
  new_df <- df
  y_ <- new_df$Gross
  for(i in numeric_cols) {
    x_ <- df[[i]]
    transformed_data <- yjPower(x_, lambda = 0.5)
    new_df[[i]] <- transformed_data
  }
  return(new_df)
}

remove_outlier <- function(df, numeric_cols) {
  winsorize_column <- function(x) {
    Winsorize(x, prob = c(0.05, 0.95))
  }
  new_df <- df
  new_df <- new_df %>%
    mutate(across(all_of(numeric_cols), winsorize_column))
  return(data.frame(new_df))
}

run_vif <- function (model) {
  vif_result <- vif(model)
  print("VIF result")
  print(vif_result)
}

scale_columns <- function(df, columns) {
  df[, columns] <- scale(df[, columns])
  return(df)
}

pca_columns <- function (df, columns_for_pca) {
  # Create a subset dataframe with only selected columns
  df_subset <- df[, columns_for_pca]

  # Standardize the data (optional but recommended for PCA)
  scaled_data <- scale(df_subset)

  # Perform PCA
  pca_result <- prcomp(scaled_data)

  # Select the desired number of principal components
  # num_components_to_retain <- 1
  # pca_subset <- pca_result$x[, 1:num_components_to_retain]
  pca_subset <- pca_result$x[, 1]

  # Combine the PCA subset with the rest of the columns
  df_result <- cbind(df[, setdiff(colnames(df), columns_for_pca)], pca_subset)

  return(df_result)
}