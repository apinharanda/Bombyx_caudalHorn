#make boxplot
library(ggplot2)

# Read in the data
fig2 <- read.table("Fig2A_values.09.13.2024.txt", header = TRUE)

# Convert 'strain' to a factor and reorder the levels to match the desired order in the plot
fig2$strain <- factor(fig2$strain, 
                      levels = c("p50", "p50xF1", "p50xsakado", "F1xF1", "Sakado"),
                      labels = c("Bmori\n(p50T)", "BC1", "F1", "F2", "B.mandarina\n(Sakado)"))

pdf("Fig2A_values.09.13.2024_corrected.pdf")
ggplot(fig2, aes(x = strain, y = length_cm)) +
  geom_jitter(size = 1, alpha = 0.2) +
  geom_boxplot(size = 1, alpha=0.1) +
  # Add a stat_summary to draw a red line at the median
  stat_summary(fun = median, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),  # Set the ymax and ymin to the median value
               width = 0.75, color = "red", size = 1.5) +  # Adjust the width and color of the line
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    aspect.ratio = 1,
    panel.border = element_rect(colour = "black", size = 0.5),
    text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(vjust = 1, face = "bold", size = 20, margin = margin(10, 20, 10, 0), colour = "black"),
    axis.title.x = element_text(vjust = 1, margin = margin(10, 20, 10, 0), face = "bold", size = 20, colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    plot.title = element_text(face = "bold")
  ) +
  labs(x = "Genotype", y = "Length (mm)")
dev.off()
