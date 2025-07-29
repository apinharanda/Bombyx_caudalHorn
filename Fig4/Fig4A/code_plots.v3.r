rm(list = ls())

# libs
library(dplyr)
library(ggplot2)
library(tibble)

# Read data
all_measures <- read.table("corrected_lenght.all_measurments.txt", head = TRUE)
all_se <- read.table("correct_lenght.all_se.txt", head = TRUE)

# Filter
filtered_data <- all_measures %>%
  filter(
    Strain %in% c("T04", "Wnt1del7", "Wnt6del2") &
    !(Strain == "T04" & Who == "Lin")
  )

filtered_se <- all_se %>%
  filter(
    Strain %in% c("T04","Wnt1del7", "Wnt6del2") &
    !(Strain == "T04" & Who == "Lin")
  )

strain_order <- c("T04", "Wnt1del7", "Wnt6del2")
filtered_data$Strain <- factor(filtered_data$Strain, levels = strain_order)
filtered_se$Strain <- factor(filtered_se$Strain, levels = strain_order)

# Plot
pdf("mean_sd_df_length_042025_all_2SE_final.pdf")
ggplot() +
  geom_jitter(data = filtered_data, aes(x = Strain, y = corrected_horn_length, colour = Who),
              width = 0.1, shape = 21, size = 1.5, alpha = 0.7) +
  geom_errorbar(data = filtered_se, aes(x = Strain, ymin = mean - 2 * se, ymax = mean + 2 * se),
                alpha = 0.7, width = 0.1) +
  geom_point(data = filtered_se, aes(x = Strain, y = mean), alpha = 0.5, size = 3.5) +
  theme_bw() +
  theme(
    aspect.ratio = 0.8,
    panel.border = element_rect(colour = "black", size = 0.5),
    text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(vjust = 1, face = "bold", size = 12,
                                margin = margin(10, 20, 10, 0)),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold"),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  ylab("Corrected horn length (mm)") +
  xlab("Strain") +
  ggtitle("Corrected horn lengths")
dev.off()

# Run ANOVA
aov_result <- aov(corrected_horn_length ~ Strain, data = filtered_data)
anova_summary <- summary(aov_result)

# Tukey HSD
tukey_result <- TukeyHSD(aov_result)


write.table(as.data.frame(anova_summary[[1]]), file = "anova_summary.txt", sep = "\t", quote = FALSE)
write.table(as.data.frame(tukey_result$Strain), file = "tukey_pairwise_comparisons.txt", sep = "\t", quote = FALSE)

# Save SE table as before
write.table(as.data.frame(filtered_se), file = "filtered_se.all.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)


# More precise Tukey post-hoc using multcomp
library(multcomp)

tukey_glht <- glht(aov_result, linfct = mcp(Strain = "Tukey"))
tukey_summary <- summary(tukey_glht)

# Extract matrix with estimates, CI, and p-values
tukey_df <- as.data.frame(tukey_summary$test[c("coefficients", "sigma", "tstat", "pvalues")])
tukey_df$comparison <- rownames(tukey_df)
tukey_df <- tukey_df[, c("comparison", "coefficients", "tstat", "pvalues")]

# Rename columns for clarity
colnames(tukey_df) <- c("Comparison", "Difference", "t_value", "Adjusted_P")

# Save results
write.table(tukey_df, file = "tukey_pairwise_comparisons_precise.txt", sep = "\t", quote = FALSE, row.names = FALSE)

# Save SE table as before
write.table(as.data.frame(filtered_se), file = "filtered_se.all.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)


