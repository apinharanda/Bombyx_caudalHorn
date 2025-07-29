# code_fig2A.r
# Author: Ana Pinharanda (2025)
#
# This script generates distribution plots of caudal horn lengths (CH) for parental strains
# (B. mori p50T and B. mandarina Sakado) and hybrids (F1, F2, BC1), as shown in Figure 2A.
# F1 hybrids: p50T ♀ × Sakado ♂
# BC1: p50T ♀ × F1 ♂
# F2: F1 ♀ × F1 ♂ (from p50T ♂ × Sakado ♀ cross)

library(ggplot2)
library(ggthemes)

# --- Load data ---
fig2 <- read.table("Fig2A_values.09.13.2024.txt", header = TRUE)

# --- Define factor level order ---
fig2$Cross <- factor(fig2$Cross, levels = c("Bmori", "Bmand", "F1", "F2", "BC1"))

# --- Plot caudal horn length distributions ---
p <- ggplot(fig2, aes(x = CH_length, fill = Cross)) +
  geom_density(alpha = 0.5, adjust = 1.5) +
  facet_wrap(~ Cross, nrow = 1, scales = "free") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  ) +
  xlab("Caudal horn length (µm)") +
  ylab("Density")

# --- Save to PDF ---
pdf("Fig2A_values.09.13.2024_corrected.pdf", width = 10, height = 3.5)
print(p)
dev.off()