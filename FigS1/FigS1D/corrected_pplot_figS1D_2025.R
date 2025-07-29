library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggpmisc)

# Load data
df <- read_tsv("values_measurments.txt")

# Filter and reshape
filtered_df <- df %>%
  filter(Strain %in% c("p50T", "Sakado")) %>%
  mutate(Strain = factor(Strain, levels = c("p50T", "Sakado"))) %>%
  pivot_longer(cols = c(Height_Laura, corrected_horn_length),
               names_to = "Measure",
               values_to = "Height") %>%
  mutate(Measure = factor(Measure,
                          levels = c("Height_Laura", "corrected_horn_length"),
                          labels = c("CH uncorrected (mm)", "CH corrected (mm)")))

# Linear model equation and R² in bottom-left
stat_eqn <- stat_poly_eq(
  formula = y ~ x,
  aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
  parse = TRUE,
  label.x.npc = 0.05,
  label.y.npc = 0.05,  # forces placement near bottom-left corner
  size = 3
)

# Custom colors
custom_colors <- c("p50T" = "blue", "Sakado" = "red")

# Plot
pdf("facet_plot_with_eqn_bottom_left.pdf", width = 7, height = 3.5)
ggplot(filtered_df, aes(x = Weight, y = Height, color = Strain)) +
  geom_point(size = 1, alpha=0.8) +
  geom_smooth(method = "lm", se = TRUE, size=0.5) +
  stat_eqn +
  facet_wrap(~Measure, scales = "fixed") +
  labs(x = "Weight (g)", y = NULL, color = "Strain") +
  scale_color_manual(values = custom_colors) +
  coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
dev.off()



