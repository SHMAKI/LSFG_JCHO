library(tidyverse)
library(Matching)

# Define directories
DIR_m <- "../../../2_model_construction/lasso/out/MAPE/0.99/Lasso/male/tsfresh/"
DIR_f <- "../../../2_model_construction/lightGBM/out/MAPE/0.99/LGBM/female/both/"
DIRout <- "../out/"

# Read CSV files
df_m <- read_csv(paste0(DIR_m, "pred_vs_true.csv"), col_select = c("group.cmp", "Age", "Predicted_age", "SEX.男1.女0"))
df_f <- read_csv(paste0(DIR_f, "pred_vs_true.csv"), col_select = c("group.cmp", "Age", "Predicted_age", "SEX.男1.女0"))

# Combine dataframes
df <- rbind(df_m, df_f)

# Define threshold for prediction accuracy
th <- 0.1

# Create a new column 'pred_real' based on prediction accuracy
df <- df %>% mutate(pred_real = if_else((Predicted_age - Age) / Age > th, 1, if_else((Predicted_age - Age) / Age < -th, 0, -1)))

# Filter out rows with 'pred_real' less than 0
df <- df %>% filter(pred_real >= 0)

# Build propensity score model
propensityScoreModel <- glm(pred_real ~ Age + SEX.男1.女0, family = binomial(link = "logit"), data = df)

# Perform propensity score matching
propensityScoreMatching <- Match(Y = df$Age,
                                 Tr = df$pred_real,
                                 X = propensityScoreModel$fitted.values,
                                 M = 1, caliper = 0.1, ties = FALSE, replace = FALSE, estimand = "ATC")

# Combine matched control and treated data
df_matched <- rbind(df[propensityScoreMatching$index.control, ], df[propensityScoreMatching$index.treated, ])

# Save the matched dataframe to a CSV file
output_file <- paste0(DIRout, "pred_vs_true_matched_male_and_female.csv")
write.csv(df_matched, file = output_file)

# Log the action
cat("Matched data saved to", output_file, "\n")
