# sakado_p50T_eachstage_jitter.R
# Author: Kenta TOMIHARA (2025)
#
# This script plots temporal expression of Wnt1 and Wnt6 in the A8 (caudal) segment
# of B. mori and B. mandarina during fourth instar development.
# Expression values are normalized to rp49 and scaled to B. mori A8 at day 0 = 1.
# Each point represents a pool of 10 individuals. Error bars show ±2 standard errors.

library(ggplot2)
library(ggthemes)
library(ggbeeswarm)

# --- Load normalized qPCR data ---
qPCR <- read.csv("summary_data.csv", header = T)

# --- Set factor levels for plotting order ---
qPCR$Gene <- factor(qPCR$Gene, levels = c("Wnt1", "Wnt6"))
qPCR$Time <- factor(qPCR$Time, levels = c("d0", "d1", "d2", "d3", "d4"))
qPCR$Species <- factor(qPCR$Species, levels = c("Bmori", "Bmand"))

# --- Plot with jittered replicates and ±2 SE error bars ---
p <- ggplot(qPCR, aes(x = Time, y = Mean, color = Species)) +
  geom_quasirandom(aes(fill = Species),
                   dodge.width = 0.5, shape = 21,
                   size = 2, alpha = 0.6) +
  geom_errorbar(aes(ymin = Mean - 2 * SE, ymax = Mean + 2 * SE),
                width = 0.2, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  facet_wrap(~ Gene, nrow = 1) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  xlab("Days after molt to 4th instar") +
  ylab("Relative expression\n(normalized to rp49)")

# --- Save plot ---
pdf("Wnt1_Wnt6_jitter_dodge_combined_caudal.pdf", height = 3.5, width = 5)
print(p)
dev.off()