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



## => genre9 perfectly alias => completely removed
# train_2 <- train_1 %>% select(-Genre9)
# test_2 <- test_1 %>% select(-Genre9)


# df <- data.frame(
#   x1 = c(1, 2, 3, 4),
#   x2 = c(2, 4, 6, 8),
#   x3 = c(3, 6, 9, 12),
#   y = c(5, 10, 15, 20)
# )

# x_ <- train_1 %>% select(-Gross)


# # Separate predictors (x) and response (y)
# x <- as.matrix(x_)
# y <- train_1$Gross

# # Perform Ridge regression
# ridge_model <- glmnet(x, y, alpha = 0)

# # Display coefficients
# coefficients <- coef(ridge_model, s=0.1)
# print("Ridge Coefficients:")
# print(coefficients)


# lm_2 <- lm("Gross ~ .", data = train_2)
# # # Print a summary of the model
# # summary(lm_2)