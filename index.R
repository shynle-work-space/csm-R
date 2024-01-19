load_libraries <- function() {
  suppressWarnings(suppressMessages({
    library(readxl)
    library(Hmisc)
    library(corrplot)
    # library(MASS)
    # library(dplyr)
    library(ggplot2)
    library(car)
    library(glmnet)
    # library(DescTools)
    library(naniar)
    library(mice)
    library(caret)
  }))
}
load_libraries()
source('lib.r')


# Read data
df <- read_excel("CSM.xlsx")

# Base model Preprocess
# ___________________________________
feature_1 <- df %>% select(-Movie)

feature_1 <- feature_1 %>% 
  rename(
    agg_fl = "Aggregate Followers"
    )

## Binary encoding
feature_1 <- feature_1 %>%
  mutate(is_2015 = as.integer(Year == 2015))
feature_1 <- feature_1 %>%
  mutate(is_sequel = as.integer(Sequel > 1))

## Factorize `Genre`
feature_1$Genre_factor <- factor(feature_1$Genre)

## Remove encoded features
feature_1 <- feature_1 %>% select(-Genre)
feature_1 <- feature_1 %>% select(-Year)
feature_1 <- feature_1 %>% select(-Sequel)

## Hard code numeric cols 
numeric_cols = c("Ratings", "Budget", "Screens", "Views", "Likes", "Dislikes", "Comments", "agg_fl")



# Remove null
# ___________________________________
feature_2 <- feature_1

## Naively remove null
feature_2 <- remove_nulls(feature_2)
print("NA removed")
print(paste("Feature 1", nrow(feature_1)))
print(paste("Feature 2", nrow(feature_2)))


# Base model
# ___________________________________
mod_feature_2 <- run_model(feature_2)
lm_base <- mod_feature_2$model
base_result <- mod_feature_2$result

print("Base model summary")
print(summary(lm_base))
print("Base model MSE")
print(base_result)
print("Base model RMSE")
print(sqrt(base_result))


# Normality test
# ___________________________________
normal_re_base <- normal_test(lm_base)
print("Normality test of Base Model")
print(normal_re_base)


qqPlot(lm_base$residuals, main="QQ plot of base model")


# Box-cox transformation
# ___________________________________

feature_3 <- feature_2

feature_3 <- yeo_johnson_trans(feature_3, numeric_cols)
feature_3 <- remove_outlier(feature_3, numeric_cols)




# Scale feature
# ___________________________________

feature_4 <- feature_3
feature_4 <- scale_columns(feature_4, numeric_cols)

lm_scale <- lm("Gross ~ .", data = feature_4)
coef_magnitudes <- coef(lm_scale)[-1]

print("Box-cox coef ranking")
print(coef_magnitudes /100000)



# Feature selection
# ___________________________________

feature_5 <- feature_4
## Detect multicollinearity
feature_no_genre <- feature_4 %>% select(-Genre_factor) # Remove genre 
corrplot(cor(feature_no_genre), method = "circle")
run_vif(lm_scale)

## Remove insignificant - collinear columns
columns_to_remove <- list("Dislikes", "Comments")

feature_5 <- feature_5[, !(names(feature_5) %in% columns_to_remove)]

## Run PCA to generate pairs 
pair_1 <- c("Budget", "Screens")
pair_2 <- c("Views", "Likes")

feature_5 <- pca_columns(feature_5, pair_1)
feature_5 <- feature_5 %>% 
  rename(
    Budget_Screens = "pca_subset"
    )

feature_5 <- pca_columns(feature_5, pair_2)
feature_5 <- feature_5 %>% 
  rename(
    Likes_Views = "pca_subset"
    )


# Linear model - feature selection
# ___________________________________

mod_feature_5 <- run_model(feature_5)
lm_2 <- mod_feature_5$model
lm2_result <- mod_feature_5$result

print("Feature Engineered model summary")
print(summary(lm_2))
print("Feature Engineered model MSE")
print(lm2_result)
print("Feature Engineered model RMSE")
print(sqrt(lm2_result))


# Imputation
# ___________________________________

## Remove unused column

feature_6 <- feature_1
feature_6 <- feature_6[, !(names(feature_6) %in% columns_to_remove)]

## Missing data categorize

columns_with_missing <- colnames(feature_6)[colSums(is.na(feature_6)) > 0]

print("Column contains missing data")
print(columns_with_missing)

no_mar <- mar_check(feature_6, columns_with_missing)$no_mar
mcar_check(feature_6, no_mar)


feature_6 <- mean_impute(feature_6, c("agg_fl"))
complete_data <- multiple_impute(feature_6, c("Budget", "Screens"))
feature_6 <- cbind(feature_6[, setdiff(colnames(feature_6), c("Budget", "Screens"))], complete_data)

print("Assert null")
print(any(is.na(feature_6)))



# Linear model - Imputation
# ___________________________________


feature_7 <- feature_6

print(names(feature_7))

feature_7 <- scale_columns(feature_7, c("Ratings", "Sentiment", "Views", "Likes", "agg_fl", "Budget", "Screens"))

## Run PCA to generate pairs 
pair_1 <- c("Budget", "Screens")
pair_2 <- c("Views", "Likes")

feature_7 <- pca_columns(feature_7, pair_1)
feature_7 <- feature_7 %>% 
  rename(
    Budget_Screens = "pca_subset"
    )

feature_7 <- pca_columns(feature_7, pair_2)
feature_7 <- feature_7 %>% 
  rename(
    Likes_Views = "pca_subset"
    )


mod_feature_7 <- run_model(feature_7)
lm_3 <- mod_feature_7$model
lm3_result <- mod_feature_7$result

print("Imputed model summary")
print(summary(lm_3))
print("Imputed model MSE")
print(lm3_result)
print("Imputed model RMSE")
print(sqrt(lm3_result))