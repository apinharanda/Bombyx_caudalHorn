# caudal_vs_noncaudal_log2FoldChange.R
# Author: Ana Pinharanda (2025)
#
# This script analyzes allele-specific expression (ASE) patterns in caudal and non-caudal A8 tissues
# of B. mori × B. mandarina F1 hybrid caterpillars at L1 and L5 stages. It computes log2 fold-changes
# between B. mandarina and B. mori alleles from biological replicate cDNA counts, summarizes expression
# differences, and visualizes them with error bars representing ±2 standard errors.
#
# 

# Dependencies
library(dplyr)
library(tidyr)
library(ggplot2)

# --- Load data with standardized column names ---
load_expression_data <- function(path) {
  df <- read.table(path, header = TRUE, sep = "\t")
  names(df) <- gsub("(_caudal|_noncaudal|_Bmori_cDNA$|_Bmand_cDNA_caudal_Bmori_cDNA.*)", "", names(df))
  return(df)
}

# --- Compute log2 fold changes from replicates ---
compute_log2fc <- function(df, stage, segment) {
  reps <- 1:4
  log2_df <- data.frame(
    Gene = df$Gene,
    Stage = df$Stage,
    Segment = segment,
    sapply(reps, function(i) log2(df[[paste0("Bmand_cDNA_", i)]] / df[[paste0("Bmori_cDNA_", i)]]))
  )
  colnames(log2_df)[4:7] <- paste0("log2_rep", reps)
  return(log2_df)
}

# --- File paths ---
input_dir <- "input_dataPlot"
caudal_L1     <- load_expression_data(file.path(input_dir, "caudal_L1.txt"))
caudal_L5     <- load_expression_data(file.path(input_dir, "caudal_L5.txt"))
noncaudal_L1  <- load_expression_data(file.path(input_dir, "noncaudal_L1.txt"))
noncaudal_L5  <- load_expression_data(file.path(input_dir, "noncaudal_L5.txt"))
plottingPos   <- read.table(file.path(input_dir, "plottingPos_3.txt"), header = TRUE)

# --- Compute fold-change tables for L1 and L5 ---
log2_L1 <- rbind(
  compute_log2fc(caudal_L1, "L1", "caudal"),
  compute_log2fc(noncaudal_L1, "L1", "noncaudal")
)
log2_L5 <- rbind(
  compute_log2fc(caudal_L5, "L5", "caudal"),
  compute_log2fc(noncaudal_L5, "L5", "noncaudal")
)

# --- Merge and reshape for plotting ---
merge_and_pivot <- function(df) {
  merged <- merge(plottingPos, df, by = c("Gene", "Stage", "Segment"))
  pivot_longer(merged, starts_with("log2_rep"), names_to = "biol_rep", values_to = "biol_rep_mean")
}

pivot_L1 <- merge_and_pivot(log2_L1)
pivot_L5 <- merge_and_pivot(log2_L5)

# --- Summary statistics ---
summary_stats <- function(pivot_df) {
  pivot_df %>%
    group_by(Gene, Stage, Segment, PlotPos) %>%
    summarise(
      mean = mean(biol_rep_mean),
      sd = sd(biol_rep_mean),
      se = sd / sqrt(n()),
      .groups = "drop"
    )
}

data_msd_L1 <- summary_stats(pivot_L1)
data_msd_L5 <- summary_stats(pivot_L5)

# --- Plotting function ---
plot_log2 <- function(pivot_data, summary_data, title, output_path, asterisk_x = NULL, asterisk_y = NULL) {
  p <- ggplot() +
    geom_hline(aes(yintercept = 0), colour = "grey", linetype = "dotted") +
    geom_point(data = pivot_data, aes(x = PlotPos, y = biol_rep_mean, colour = Segment),
               shape = 21, size = 2, alpha = 0.5) +
    geom_errorbar(data = summary_data, aes(x = PlotPos, ymin = mean - 2 * se, ymax = mean + 2 * se, colour = Segment),
                  alpha = 0.5) +
    geom_point(data = summary_data, aes(x = PlotPos, y = mean, colour = Segment),
               size = 4, alpha = 0.5) +
    scale_x_continuous(
      breaks = c(2,8,14,20,26,32,38,44,50,56,62,68,74),
      labels = c("wnt9", "wnt1", "wnt6", "wnt10", "ninaC", "lolaI",
                 "Lip3-like", "Girdin-like", "IntB2", "IntB3", "IntB-PS-like", "Teneurin-2", "STPK26-like")
    ) +
    scale_colour_manual(values = c("black", "red")) +
    theme_bw(base_size = 10) +
    theme(
      aspect.ratio = 0.5,
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", size = 0.5),
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold", margin = margin(10, 20, 10, 0)),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    ) +
    ylab("log2(Fold Difference)\nBmand/Bmori") +
    ggtitle(title)

  if (!is.null(asterisk_x)) {
    for (i in seq_along(asterisk_x)) {
      p <- p + annotate("text", x = asterisk_x[i], y = asterisk_y[i], label = "*", size = 10)
    }
  }

  ggsave(output_path, p, width = 8, height = 4)
}

# --- Output ---
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE)

plot_log2(pivot_L1, data_msd_L1, "L1", file.path(out_dir, "log2_L1_PlotPos_pivot_2SE.pdf"), c(10, 16), c(2.5, 2.5))
plot_log2(pivot_L5, data_msd_L5, "L5", file.path(out_dir, "log2_L5_PlotPos_pivot_2SE.pdf"), c(9, 15), c(2.5, 2.5))

write.table(pivot_L1, file = file.path(out_dir, "log2_L1_PlotPos_pivot.txt"),
            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(pivot_L5, file = file.path(out_dir, "log2_L5_PlotPos_pivot.txt"),
            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(data_msd_L1, file = file.path(out_dir, "data_msd_L1.txt"),
            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(data_msd_L5, file = file.path(out_dir, "data_msd_L5.txt"),
            sep = "\t", quote = FALSE, row.names = FALSE)