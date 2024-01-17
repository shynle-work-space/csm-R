library(readxl)
library(Hmisc)
library(corrplot)
library(dplyr)
library(caret)
library(ggplot2)
library(car)

# Read data
df <- read_excel("CSM.xlsx")

# Preprocessing
feature_df <- df %>% select(-Movie)
feature_df <- feature_df %>% 
  rename(
    aggregated_followers = "Aggregate Followers"
    )
## Binary encoding
feature_df <- feature_df %>%
  mutate(is_2015 = as.integer(Year == 2015))

feature_df <- feature_df %>%
  mutate(is_sequel = as.integer(Sequel > 1))
## One-hot encoding
feature_df$Genre <- as.character(feature_df$Genre)
feature_df <- cbind(feature_df, model.matrix(~ Genre - 1, data = feature_df))

## Remove encoded features
feature_df <- feature_df %>% select(-Genre)
feature_df <- feature_df %>% select(-Year)
feature_df <- feature_df %>% select(-Sequel)


# Train test split
set.seed(21)
splitIndex <- createDataPartition(feature_df$Gross, p = 0.75, list = FALSE)
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


print(names(train_1))

lm_base <- lm("Gross ~ .", data = train_1)
# Print a summary of the model
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
print(calculate_result(lm_base, test_1))

# Normality test
normal_test <- function(model) {
    shapiro_re <- shapiro.test(resid(lm_base))
    t_test_re <- t.test(resid(lm_base), mu = 0)
    ncv_test_re <- ncvTest(lm_base) #H0: phương sai sai số không đổi
    l <- list(shapiro_re, t_test_re, ncv_test_re)
    return(l)
}

print(normal_test(lm_base))
