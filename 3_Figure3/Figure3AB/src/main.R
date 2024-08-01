library(tidyverse)
library(ggsci)
library(VennDiagram) 

# Load custom ggplot theme
source("ggplot_theme.R")

# Define file paths
dir_f_male <- "../../../2_model_construction/lasso/out/MAPE/0.99/Lasso/male/tsfresh/feature_coefs.csv"
dir_f_female <- "../../../2_model_construction/lightGBM/out/MAPE/0.99/LGBM/female/both/feature_importance.csv"
dir_f_both <- "../../../2_model_construction/lasso/out/MAPE/0.99/Lasso/both/tsfresh/feature_coefs.csv"

# Read CSV files
f_male <- read_csv(dir_f_male) %>% select(-`...1`)
f_female <- read_csv(dir_f_female) %>% select(-`...1`)
f_both <- read_csv(dir_f_both) %>% select(-`...1`)

# Define feature types
types = c("ONH_VdivChoroid", "ONH_TdivChoroid","ONH_VdivONH_T","ONH_VsubChoroid", "ONH_TsubChoroid","ONH_VsubONT_T","ONH_A", "ONH_V", "ONH_T", "Choroid")

# Function to calculate feature counts
calc_features <- function(df, types) {
  tmp_cols <- colnames(df)
  tmp_vecotr <- rep(0, length(types))
  for (i in 1:length(types)) {
    TF <- str_detect(tmp_cols, pattern=paste0("^", types[i]))
    tmp_vecotr[i] <- sum(TF)
    tmp_cols <- tmp_cols[!TF]
  }
  DF <- data.frame(types = types, counts = tmp_vecotr)
  return(DF)
}

# Calculate feature counts for each model
df_male <- calc_features(f_male, types)
df_male$counts %>% sum()
df_female <- calc_features(f_female, types)
df_both <- calc_features(f_both, types)

# Combine the feature counts into one dataframe
df <- cbind(df_female, df_male$counts, df_both$counts)
sex <- c("Female\nmodel", "Male\nmodel", "Model for\nboth sexes")
colnames(df)[2:4] <- sex

# Gather the data for plotting
df_gather <- df %>% gather(key="sex", value=count,-types)
df_gather$types <- factor(df_gather$types, levels=c("ONH_A",
                                                    "ONH_V",
                                                    "ONH_T",
                                                    "Choroid",
                                                    "ONH_VsubONT_T",
                                                    "ONH_VdivONH_T",
                                                    "ONH_VsubChoroid",
                                                    "ONH_TsubChoroid",
                                                    "ONH_VdivChoroid",
                                                    "ONH_TdivChoroid"
                                                    ))
df_gather$sex <- factor(df_gather$sex, levels=sex)

# Define primary and secondary feature types
MBR_levels <-c("Primary", "Secondary")
df_gather <- df_gather %>% mutate(origin=if_else(types %in% c("ONH_A", "ONH_V", "ONH_T", "Choroid"), MBR_levels[1], MBR_levels[2])) %>% 
  group_by(sex) %>% mutate(ratio=count/sum(count)*100)

df_gather$origin <- factor(df_gather$origin, levels=MBR_levels)
df_gather$sex <- factor(df_gather$sex, levels=sex)

# Plot the feature usage frequencies
g <- ggplot(df_gather %>% filter(sex!="Both sexes"),aes(x=origin,y=ratio,fill = types))+
  geom_bar(stat = "identity", position = "stack")+theme_bw()+scale_fill_aaas()+
  ylab("Frequency of feature usage (%)")+xlab("\nThe origin of MBR waveforms")+
  ng1+facet_grid(.~sex)+theme(legend.title = element_text(size=textsize),
                              axis.text.x = element_text(angle = 30, hjust = 1, vjust=1))+
  labs(fill="Waveform types")

# Save the plot
ggsave(g, filename = "../figure/figure3B.pdf",w=12,h=7)

# Save dataframes to CSV files
write.csv(df_gather, file = "../out/df_ratio.csv")
write.csv(df, file = "../out/df_count_features.csv")

# Save feature lists to a text file
feature_list <- list(female = colnames(f_female), male = colnames(f_male), both = colnames(f_both))
capture.output(summary(feature_list), file = "../out/feature_list.txt")


# Create Venn diagram
list_venn <- list(females = colnames(f_female), males = colnames(f_male), both = colnames(f_both))
venn.diagram <- venn.diagram(
  list_venn,
  filename = NULL,
  category.names = sex,
  fill = c(2, 4, 3),
  alpha = 0.4,
  scaled = TRUE,
  lty = 1,
  cat.cex = 2.5,  # Set label text size
  cat.fontfamily = "sans",  # Set label font family
  cex = 2.5,
  fontfamily = "sans"  # Set font family
)

# Save Venn diagram to PDF
pdf(file = "../figure/figure3A.pdf", width = 12, height = 6)
grid.draw(venn.diagram)
dev.off()
