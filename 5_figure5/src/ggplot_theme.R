textsize <- 24
pointsize <- 2.5
legendsize <- 18
LINEWIDTH <- 2

ng1 <- theme(#panel.background = element_rect(fill = "white",colour = "white"),
  #panel.grid.major = element_line(colour = NA),
  #axis.line = element_line(size = 1.2, colour="black"),
  #axis.ticks.x=element_blank(),
  #             axis.ticks=element_line(color="black"),
  #panel.grid.minor = element_line(colour = NA),
  axis.text=element_text(color="black",size=textsize),
  axis.title=element_text(color="black",size=textsize),
  #axis.line = element_line(size=2),
  panel.border = element_rect(color = "black", fill = NA, size = LINEWIDTH),
  #panel.margin=unit(.05, "lines"),
  #legend.position = LEGEND_POSITION,#"right",# c(0.7, 1),   
  #legend.direction = "horizontal", #or "vertical",
  legend.background=element_rect(fill="transparent"),
  legend.key=element_rect(linetype = 0, fill = "white"),
  legend.text = element_text(size=legendsize),
  legend.title = element_blank(),legend.key.size = unit(1, "cm"),
  strip.background = element_rect(color="transparent", fill="transparent", size=LINEWIDTH),
  strip.text = element_text(size=textsize),
  plot.title = element_text(hjust = 0.5, face = "italic")
)