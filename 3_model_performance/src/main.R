library(tidyverse)

# Load custom ggplot theme
source("ggplot_theme.R")

# Define file paths
# Recursively search for CSV files that include "pred_vs_true.csv" in their name
csv_files <- list.files(path = "../../../2_model_construction", pattern = "pred_vs_true\\.csv$", recursive = TRUE, full.names = TRUE)

# Process each CSV file
for (file in csv_files) {
  # Load CSV data
  df <- read.csv(file)
  
  # Calculate MAPE and MAE
  # Note: Perform necessary error checking (e.g., handling age = 0, missing data, etc.)
  mape <- mean(abs((df$Predicted_age - df$Age) / df$Age)) * 100
  mae  <- mean(abs(df$Predicted_age - df$Age))
  
  # Create scatter plot
  p <- ggplot(df, aes(x = Age, y = Predicted_age)) +
    geom_point() +
    labs(x = "Chronological Age", y = "Predicted Age") +
    theme_bw() + ng1 +
    theme(plot.margin = margin(t = 20, unit = "pt")) +
    # Annotate MAPE and MAE at the top left of the graph (age's lower 5% and predicted_age's upper 5%)
    annotate("text", 
             x = quantile(df$Age, 0, na.rm = TRUE), 
             y = quantile(df$Predicted_age, 1, na.rm = TRUE), 
             label = paste("MAPE =", round(mape, 3), "\nMAE =", round(mae, 3)),
             hjust = 0, vjust = 1, size = 6)
  
  # Define output folder: extract the directory path and get the last 3 subdirectories
  output_folder <- file.path("../figure", str_extract(dirname(file), "[^/]+/[^/]+/[^/]+$"))
  
  # Create the output folder if it doesn't exist
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Define output file path
  output_file <- file.path(output_folder, "scatter_Figure3ABC.pdf")
  
  # Save the plot as a PDF in the same directory as the original CSV
  ggsave(output_file, plot = p, width = 6, height = 4, device = cairo_pdf)
}
