library(readxl)
library(Hmisc)
library(corrplot)
library(MASS)
library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(glmnet)
library(DescTools)
library(naniar)
library(mice)



# Read data
df <- read_excel("CSM.xlsx")

# Preprocessing
feature_df <- df %>% select(-Movie)

feature_df <- feature_df %>% 
  rename(
    agg_fl = "Aggregate Followers"
    )

## Binary encoding
feature_df <- feature_df %>%
  mutate(is_2015 = as.integer(Year == 2015))
feature_df <- feature_df %>%
  mutate(is_sequel = as.integer(Sequel > 1))

## Factorize `Genre`
feature_df$Genre_factor <- factor(feature_df$Genre)

## Remove encoded features
feature_df <- feature_df %>% select(-Genre)
feature_df <- feature_df %>% select(-Year)
feature_df <- feature_df %>% select(-Sequel)


# Train test split
set.seed(21)
splitIndex <- createDataPartition(feature_df$Genre_factor, p = 0.75, list = FALSE)
train_data <- feature_df[splitIndex, ]
test_data <- feature_df[-splitIndex, ]

# Naively remove null
remove_nulls <- function(df) {
  cleaned_df <- na.omit(df)
  return(cleaned_df)
}

train_1 <- remove_nulls(train_data)
test_1 <- remove_nulls(test_data)

## Plot num of null deleted
category_labels <- c("Train", "Test")
values1 <- c(nrow(train_data), nrow(test_data))
values2 <- c(nrow(train_1), nrow(test_1))

train_test_len <- data.frame(Category = rep(category_labels, 2),
                 Values = c(values1, values2),
                 Group = rep(c("Before", "After"), each = length(category_labels)))

plot <- ggplot(train_test_len, aes(x = Category, y = Values, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Values), position = position_dodge(width = 0.8), vjust = -0.5) +
  labs(title = "Number of rows before-after naive row removal",
       x = "Category",
       y = "Values",
       fill = "Group") +
  theme_minimal()

print(plot)


# Construct base model
lm_base <- lm("Gross ~ .", data = train_1)

## Print a summary of the model
summary(lm_base)

calculate_result <- function (model, test_data) {
    test_set <- test_data %>% select(-Gross)
    predictions <- predict(lm_base, test_set)
    residuals <- test_1$Gross - predictions
    mse <- mean(residuals^2)
    rmse <- sqrt(mse)
    l <- list(mse, rmse)
    return(l)
}

print("Results of base model is:\n")
result_lmbase <- calculate_result(lm_base, test_1)
print(result_lmbase)

# Normality test
normal_test <- function(model) {
    shapiro_re <- shapiro.test(resid(lm_base))
    t_test_re <- t.test(resid(lm_base), mu = 0)
    ncv_test_re <- ncvTest(lm_base) #H0: phương sai sai số không đổi
    l <- list(shapiro_re, t_test_re, ncv_test_re)
    return(l)
}

print(normal_test(lm_base))

draw_qqplot <- function(data, title) {
  qqPlot(data, main=title)
}

qqPlot(lm_base$residuals, main="QQ plot of base model")

numeric_cols = c("Ratings", "Budget", "Screens", "Views", "Likes", "Dislikes", "Comments", "agg_fl")
for (i in numeric_cols) {
  draw_qqplot(train_1[[i]], i)
}

# Transform the outlier before box-cox transformation
winsorize_df <- function(df, numeric_cols) {
  winsorize_column <- function(x) {
    Winsorize(x, prob = c(0.05, 0.95))
  }
  new_df <- df
  new_df <- new_df %>%
    mutate(across(all_of(numeric_cols), winsorize_column))
  return(new_df)
}

winsor_df <- winsorize_df(train_1, numeric_cols)

# Box-cox transformation

box_cox_trans <- function (df, numeric_cols) {
  new_df <- df
  y_ <- new_df$Gross
  for(i in numeric_cols) {
    x_ <- df[[i]]
    bc <- boxcox(x_ ~ y_)
    lambda <- bc$x[which.max(bc$y)]
    new_x_exact <- (x_ ^ lambda - 1) / lambda
    new_df[[i]] <- new_x_exact
  }
  return(new_df)
}


train_2 <- box_cox_trans(winsor_df, numeric_cols)

lm_2 <- lm("Gross ~ .", data = train_2)

print(normal_test(lm_2))

qqPlot(lm_2$residuals, main="QQ plot of box-cox model")


# Draw Correlation heatmap
train_no_genre <- train_2 %>% select(-Genre_factor)
corrplot(cor(train_no_genre), method = "circle")

print(alias(lm_2))

vif_result <- vif(lm_2)
print(vif_result)


# Missing data test

columns_with_missing <- colnames(feature_df)[colSums(is.na(feature_df)) > 0]

## Missing at Random (MAR) test using logreg

## Create an array indicating missing values
## 0 if No missing, 1 if there is missing data
missing_indicator <- apply(is.na(feature_df), 1, function(row) ifelse(any(row), 0, 1))


p_vals <- rep(NA, length(columns_with_missing))

for (j in 1:length(columns_with_missing)) {
  col_name <- columns_with_missing[j]
  s <- summary(glm(missing_indicator ~ unlist(feature_df[, col_name])), family = binomial)
  p_vals[j] <- s$coefficients[2,4]
  s$coefficients[2,4]
}

print(p_vals)
H0_MAR <- which(p_vals >= 0.05)
H1_MAR <- which(p_vals < 0.05)

print("H0 means that either missing value are MCAR, or MNAR")
Non_MAR_cols <- columns_with_missing[H0_MAR]
print(paste("Column that are H0:", Non_MAR_cols))

print("H1 means that MAR")
MAR_cols <- columns_with_missing[H1_MAR]
print(paste("Column that are H1:", MAR_cols))



## Missing Completely at Random (MCAR) test

df_filtered <- feature_df[Non_MAR_cols]

H0_MCAR <- list()
H1_MCAR <- list()

for (i in Non_MAR_cols) {
  mcar_p_val <- mcar_test(data.frame(df_filtered[, i]))[1, 3]
  if(mcar_p_val >= 0.05) {
    H0_MCAR <- c(H0_MCAR, i)
  } else {
    H1_MCAR <- c(H1_MCAR, i)
  }
}
print(paste("Columns that are MCAR: ", H0_MCAR))
print(paste("Columns that are not MCAR: ", H1_MCAR))

# Imputation

## MCAR imputation: Replace with mean

imputed_df <- feature_df
for(i in H0_MCAR) {
  mean_val <- mean(unlist(imputed_df[, i]), na.rm = TRUE)
  imputed_df[[i]] <- ifelse(is.na(imputed_df$agg_fl), mean_val, imputed_df[[i]])
}

## MAR imputation: Multiple imputation

MAR_df <- imputed_df[, H1_MAR]
imputed_data <- mice(MAR_df, m = 1, maxit = 10, method = 'pmm', seed = 21)
completed_data <- complete(imputed_data, 1)

imputed_df$Budget <- completed_data$Budget
imputed_df$Screens <- completed_data$Screens

