library(tidyverse)
library(ggsci)
library(VennDiagram)
library(gridExtra)

# Load custom ggplot theme
source("ggplot_theme.R")

# Define file paths
dir_f_male <- "../../../2_model_construction/lasso/out/MAPE/0.99/Lasso/male/tsfresh/feature_coefs.csv"
dir_f_female <- "../../../2_model_construction/lightGBM/out/MAPE/0.99/LGBM/female/both/feature_importance.csv"
dir_f_both <- "../../../2_model_construction/lasso/out/MAPE/0.99/Lasso/both/tsfresh/feature_coefs.csv"

# Function to safely read CSV files
safe_read_csv <- function(file) {
  if (file.exists(file)) {
    return(read_csv(file) %>% select(-`...1`))
  } else {
    stop(paste("File not found:", file))
  }
}

# Read CSV files
f_male <- safe_read_csv(dir_f_male)
f_female <- safe_read_csv(dir_f_female)
f_both <- safe_read_csv(dir_f_both)

# Define feature types
types <- c("ONH_VdivChoroid", "ONH_TdivChoroid", "ONH_VdivONH_T", "ONH_VsubChoroid", "ONH_TsubChoroid",
           "ONH_VsubONT_T", "ONH_A", "ONH_V", "ONH_T", "Choroid")

# Function to calculate feature counts
calc_features <- function(df, types) {
  counts <- sapply(types, function(type) sum(str_detect(colnames(df), paste0("^", type))))
  return(data.frame(types = types, counts = counts))
}

# Calculate feature counts
df_male <- calc_features(f_male, types)
df_female <- calc_features(f_female, types)
df_both <- calc_features(f_both, types)

# Combine the feature counts
df <- cbind(df_female, Male = df_male$counts, Combined = df_both$counts)
colnames(df)[2:4] <- c("Female", "Male", "Combined")

# Reshape data for plotting
df_gather <- df %>%
  gather(key = "sex", value = "count", -types) %>%
  mutate(origin = if_else(types %in% c("ONH_A", "ONH_V", "ONH_T", "Choroid"), "Primary", "Secondary")) %>%
  group_by(sex) %>% mutate(ratio = count / sum(count) * 100)

df_gather$sex <- factor(df_gather$sex, levels = c("Female", "Male", "Combined"))
df_gather$origin <- factor(df_gather$origin, levels = c("Primary", "Secondary"))

# Save feature lists
feature_list <- list(female = colnames(f_female), male = colnames(f_male), both = colnames(f_both))
capture.output(summary(feature_list), file = "../out/feature_list.txt")

# Create and save Venn diagram
venn.diagram <- venn.diagram(
  x = feature_list,
  filename = NULL,
  category.names = c("Female", "Male", "Combined"),
  fill = c(2, 4, 3), alpha = 0.4, scaled = TRUE,
  lty = 1, cat.cex = 1.5, fontfamily = "sans", cat.fontfamily = "sans",
  cat.pos = c(315, 45, 180), cat.dist = rep(0.08, 3),
  cex = 1.5
)

pdf(file = "../figure/figure3D.pdf", width = 6, height = 6)
grid.draw(venn.diagram)
dev.off()

# Prepare data for pie charts
df_gather_origin <- df_gather %>%
  group_by(sex, origin) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  group_by(sex) %>%
  mutate(
    percentage = count / sum(count) * 100,
    pos = cumsum(count) - 0.5 * count
  )

# Function to create a pie chart
create_pie_chart <- function(data, sex_value) {
  filtered_data <- filter(data, sex == sex_value)
  
  ggplot(filtered_data, aes(x = 1, fill = origin, weight = count)) +
    geom_bar(color = "black") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("Primary" = "#4682B4", "Secondary" = "#CD5C5C")) +
    labs(title = sex_value) +
    theme_void() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    geom_text(aes(y = count, label = paste0(origin, "\n", round(percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), size = 3.5)
}

# Generate pie charts
p1 <- create_pie_chart(df_gather_origin, "Female")
p2 <- create_pie_chart(df_gather_origin, "Male")
p3 <- create_pie_chart(df_gather_origin, "Combined")

# Save pie charts
pdf("../figure/Figure3E.pdf")
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()

# Filter primary origin data
df_primary <- df_gather %>% filter(origin == "Primary") %>%
  group_by(sex) %>% mutate(ratio = count / sum(count) * 100)
df_primary$types <- factor(df_primary$types, levels = c("Choroid", "ONH_T", "ONH_V", "ONH_A"))

# Create and save bar plot
g <- ggplot(df_primary, aes(x = types, y = ratio, fill = sex)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  theme_bw() + scale_fill_aaas() +
  ylab("Frequency of\nfeature usage (%)") + xlab("\nThe origin of MBR waveforms") + ng1 +
  theme(legend.title = element_text(size = textsize),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

ggsave(g, filename = "../figure/figure3F.pdf", width = 8, height = 5)

# Save data to CSV
write.csv(df_gather, file = "../out/df_ratio.csv", row.names = FALSE)
write.csv(df, file = "../out/df_count_features.csv", row.names = FALSE)
