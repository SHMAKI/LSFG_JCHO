# Load necessary libraries
library(tidyverse)
library(ggrepel)
library(exactRankTests)

# Source additional plotting theme
source("ggplot_theme.R")

# Define helper function
se <- function(x) {sd(x) / sqrt(length(na.omit(x)))}

# Read in data
df_male <- read_csv("../out/shap_male.csv") 
df_male <- df_male %>% dplyr::select(-Age, -Predicted_age, -SEX.男1.女0,)

# Data preprocessing
df_male_matched <- df_male %>% 
  mutate(pred_real = replace(pred_real, pred_real==0, "pred_young")) %>% 
  mutate(pred_real = replace(pred_real, pred_real==1, "pred_old")) %>% 
  select(-group.cmp)

colnames(df_male_matched) <- gsub("__", "__ ", colnames(df_male_matched))

# Plot violin and boxplots
df_male_gather <- df_male_matched %>% gather(key = features, value="SHAP", -c(pred_real))

# Summary statistics
g <- ggplot(df_male_gather, aes(y=SHAP, x=pred_real))+geom_violin()+geom_boxplot(width=0.1)
g <- g + theme_bw()+ng1+
  facet_wrap(~features, scales = "free",
             labeller = label_wrap_gen())
ggsave(g, filename = "../figure/shap_male_boxplot.pdf",w=48,h=36)

df_male_summary_mean <- df_male_matched %>% group_by(pred_real) %>% summarise_all(mean)
df_male_summary_se <- df_male_matched %>% group_by(pred_real) %>% summarise_all(se)
df_t_mean <- data.frame(t(df_male_summary_mean[,-1]))
df_t_se <- data.frame(t(df_male_summary_se[,-1]))

df_t <- cbind(df_t_mean, df_t_se)
colnames(df_t) <- c("SHAP_pred_old","SHAP_pred_young",
                    "SHAP_pred_old_se","SHAP_pred_young_se"
                    )
df_t <- df_t %>% tibble::rownames_to_column("features")

# One-sided test for significant features
df_male_2 <- df_male_gather %>% 
  pivot_wider(names_from = pred_real, values_from = SHAP)
df_male_2 <- df_male_2 %>% group_by(features) %>% 
  mutate(p_value = wilcox.exact(unlist(pred_young), 
                          unlist(pred_old), 
                          alternative="less")$p.value)
df_male_2$q_value <- p.adjust(df_male_2$p_value, method="BH")
df_male_2 <- df_male_2 %>% mutate(is.significant = p_value<0.05) %>% arrange(p_value) %>% select(-c(pred_young,pred_old))

df_t <- merge(df_t,df_male_2,by = "features")
df_t <- df_t %>% mutate(L = "")
df_t$L[df_t$is.significant] <- df_t$features[df_t$is.significant]
df_t$L <- gsub("__ ", "__\n", df_t$L)
df_t$bold_labels <- ifelse(df_t$features=="Choroid__ fft_coefficient__ attr_imag__ coeff_2", "bold", "plain")

# Plot SHAP values
g2 <- ggplot(df_t, aes(x=SHAP_pred_old, y=SHAP_pred_young, label = L,color=is.significant))+
  geom_point()+
  scale_color_manual(values = c("black", "red"), guide = "none")
g2 <- g2 + geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_abline(intercept = 0, slope = 1,alpha = 0.5,linetype="dashed")
g2 <- g2 + geom_errorbar(aes(ymin = SHAP_pred_young - SHAP_pred_young_se,
                              ymax = SHAP_pred_young + SHAP_pred_young_se), alpha=0.2)
g2 <- g2 + geom_errorbarh(aes(xmin = SHAP_pred_old - SHAP_pred_old_se,
                              xmax = SHAP_pred_old + SHAP_pred_old_se), alpha=0.2)
g2 <- g2 + geom_text_repel(
  aes(fontface = bold_labels),
  size = 4,
  segment.size = 0.3,
  box.padding = unit(.25, "lines"),
  point.padding = unit(.5, "lines"),
  min.segment.length = 0.2, seed = 42,
  max.overlaps = Inf, nudge_x = -0.02, nudge_y = 0.125,
  arrow = arrow(length = unit(0.01, 'npc'),type = "closed")
  )

text_size=16
g2 <- g2+xlab("SHAP values in model-predicted older")+
  ylab("SHAP values in model-predicted younger")+
  ng1+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = text_size),
        axis.title.y = element_text(size = text_size),
        axis.text.x = element_text(size = text_size),
        axis.text.y = element_text(size = text_size),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_rect(linewidth=1)
  )+ylim(c(-.55,.55))+xlim(c(-.55,.55))
ggsave(g2, filename = "../figure/SHAP_male_point.pdf",width = 7, height = 6)