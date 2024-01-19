load_libraries <- function() {
  suppressWarnings(suppressMessages({
    library(caret)
    library(MASS)
    library(dplyr)
    library(car)
    library(DescTools)
    library(Hmisc)

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
  df_subset <- df[, columns_for_pca]
  scaled_data <- scale(df_subset)
  pca_result <- prcomp(scaled_data)
  pca_subset <- pca_result$x[, 1]
  df_result <- cbind(df[, setdiff(colnames(df), columns_for_pca)], pca_subset)
  return(df_result)
}

mar_check <- function (data, columns_with_missing) {
  # Create an array indicating missing values
  ## 0 if No missing, 1 if there is missing data
  missing_indicator <- apply(is.na(data), 1, function(row) ifelse(any(row), 0, 1))


  p_vals <- rep(NA, length(columns_with_missing))

  for (j in 1:length(columns_with_missing)) {
    col_name <- columns_with_missing[j]
    s <- summary(glm(missing_indicator ~ unlist(data[, col_name])), family = binomial)
    p_vals[j] <- s$coefficients[2,4]
    s$coefficients[2,4]
  }

  H0_MAR <- which(p_vals >= 0.05)
  H1_MAR <- which(p_vals < 0.05)

  print("H0 means that either missing value are MCAR, or MNAR")
  Non_MAR_cols <- columns_with_missing[H0_MAR]
  print(paste("Column that are H0:", Non_MAR_cols))
  print("Column that are H0:")
  print(Non_MAR_cols)

  print("H1 means that MAR")
  MAR_cols <- columns_with_missing[H1_MAR]
  print("Column that are H1:")
  print(MAR_cols)
  return(list(no_mar=Non_MAR_cols))
}

mcar_check <- function (data, cols) {
  
  df_filtered <- data[cols]

  H0_MCAR <- list()
  H1_MCAR <- list()

  for (i in cols) {
    mcar_p_val <- mcar_test(data.frame(df_filtered[, i]))[1, 3]
    if(mcar_p_val >= 0.05) {
      H0_MCAR <- c(H0_MCAR, i)
    } else {
      H1_MCAR <- c(H1_MCAR, i)
    }
  }
  print(paste("Columns that are MCAR: ", H0_MCAR))
  print(paste("Columns that are not MCAR: ", H1_MCAR))
}

mean_impute <- function (imputed_df, cols) {
  for(i in cols) {
    mean_val <- mean(unlist(imputed_df[, i]), na.rm = TRUE)
    imputed_df[[i]] <- ifelse(is.na(imputed_df$agg_fl), mean_val, imputed_df[[i]])
  }
  return(imputed_df)
}

multiple_impute <- function (df, cols) {
  MAR_df <- df[, cols]
  imputed_data <- mice(MAR_df, m = 1, maxit = 10, method = 'cart', seed = 21)
  completed_data <- complete(imputed_data, 1)
  return(completed_data)
}