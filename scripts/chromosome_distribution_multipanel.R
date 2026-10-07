# ============================================================
# chromosome_distribution_multipanel.R
# Multipanel 2-panel: A = core fraction histogram, B = edge vs center
# (Chromosome tracks from original Fig1 left as standalone file)
# Output: figures/Fig_chromosome_distribution_multipanel.pdf / .png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/chromosome_distribution")

# ---- Load ----
bins <- read.csv("chromosome_bins.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(bins), "bins\n")

for (col in c("n_core","n_soft_core","n_dispensable","n_private","n_singleton")) {
  bins[[col]][is.na(bins[[col]])] <- 0
}

bins <- bins %>%
  mutate(
    total_classified = n_core + n_soft_core + n_dispensable + n_private + n_singleton,
    pct_core         = 100 * n_core        / pmax(total_classified, 1),
    pct_dispensable  = 100 * n_dispensable / pmax(total_classified, 1),
    pct_private      = 100 * n_private     / pmax(total_classified, 1)
  ) %>%
  filter(total_classified >= 5)

cat("After filtering:", nrow(bins), "bins\n")

# ---- Unified theme ----
theme_mp <- function(base = 15) {
  theme_classic(base_size = base) +
    theme(
      plot.title         = element_text(face = "bold", size = base + 3, hjust = 0),
      plot.subtitle      = element_text(size = base - 3, color = "gray30", hjust = 0),
      axis.title         = element_text(face = "bold", size = base),
      axis.text          = element_text(size = base - 2, color = "black"),
      legend.title       = element_text(face = "bold", size = base - 2),
      legend.text        = element_text(size = base - 2),
      legend.key.size    = unit(0.5, "cm"),
      plot.margin        = margin(20, 14, 14, 14),
      axis.line          = element_line(linewidth = 0.6, color = "black"),
      axis.ticks         = element_line(linewidth = 0.6, color = "black")
    )
}

type_colors <- c(
  Core        = "#1B5E20",
  Dispensable = "#E65100",
  Private     = "#6A1B9A"
)

# ============================================================
# PANEL A — Core fraction histogram
# ============================================================
panelA_inner <- ggplot(bins, aes(x = pct_core)) +
  geom_histogram(bins = 40, fill = "#1B5E20", color = "black",
                 alpha = 0.95, linewidth = 0.2) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  labs(x = "% of genes classified as core",
       y = "Number of bins") +
  theme_mp()

# ============================================================
# PANEL B — Edge vs center bar chart
# ============================================================
edge_center <- bins %>%
  group_by(accession_name, contig_name) %>%
  mutate(
    contig_max = max(bin_mb, na.rm = TRUE),
    position   = bin_mb / pmax(contig_max, 1),
    zone       = case_when(
      position < 0.2 ~ "Start (0-20%)",
      position > 0.8 ~ "End (80-100%)",
      TRUE           ~ "Center (20-80%)"
    )
  ) %>%
  ungroup() %>%
  filter(n_total >= 10) %>%
  group_by(zone) %>%
  summarise(
    mean_pct_core        = mean(pct_core, na.rm = TRUE),
    mean_pct_dispensable = mean(pct_dispensable, na.rm = TRUE),
    mean_pct_private     = mean(pct_private, na.rm = TRUE),
    n_bins = n(),
    .groups = "drop"
  )

edge_center_long <- edge_center %>%
  select(zone, mean_pct_core, mean_pct_dispensable, mean_pct_private) %>%
  pivot_longer(cols = starts_with("mean_pct_"),
               names_to = "type", values_to = "mean_pct") %>%
  mutate(type = recode(type,
                       mean_pct_core        = "Core",
                       mean_pct_dispensable = "Dispensable",
                       mean_pct_private     = "Private"))

# Order zones: Start, Center, End
edge_center_long$zone <- factor(edge_center_long$zone,
                                levels = c("Start (0-20%)",
                                           "Center (20-80%)",
                                           "End (80-100%)"))

panelB_inner <- ggplot(edge_center_long,
                       aes(x = zone, y = mean_pct, fill = type)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.72, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = type_colors, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Mean % of genes") +
  theme_mp() +
  theme(axis.text.x = element_text(face = "bold"),
        legend.position = "top")

# ============================================================
# Assemble side by side
# ============================================================
composite <- (panelA_inner | panelB_inner) +
  plot_layout(widths = c(1, 1.15)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 34))
  )

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_chromosome_distribution_multipanel.pdf",
       composite, width = 16, height = 7, device = cairo_pdf)
ggsave("figures/Fig_chromosome_distribution_multipanel.png",
       composite, width = 16, height = 7, dpi = 300)

cat("Multipanel figure saved:\n")
cat("  figures/Fig_chromosome_distribution_multipanel.pdf\n")
cat("  figures/Fig_chromosome_distribution_multipanel.png\n")