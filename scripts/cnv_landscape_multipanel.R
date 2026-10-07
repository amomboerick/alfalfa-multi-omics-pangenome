# ============================================================
# cnv_landscape_multipanel.R
# Multipanel 2-panel: A = CV distribution, B = CV violin by type
# (Panel C from the original script is dropped; chromosome detail
#  is available via the source Fig3 output)
# Output: figures/Fig_cnv_landscape_multipanel.pdf / .png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/cnv_landscape")

# ---- Load ----
cnv <- read.csv("cluster_cnv.csv", stringsAsFactors = FALSE)
chrom <- read.csv("cluster_chromosomes.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(cnv), "clusters\n")

cnv <- cnv %>% left_join(chrom, by = "cluster_id")

cnv_f <- cnv %>%
  filter(!is.na(cv), gene_count >= 3, n_accessions_present >= 3)

cat("After filtering:", nrow(cnv_f), "clusters\n")

type_levels <- c("core", "soft_core", "dispensable", "private", "singleton")
cnv_f$cluster_type <- factor(cnv_f$cluster_type, levels = type_levels)
cnv_f <- cnv_f %>% filter(!is.na(cluster_type))

# ---- Shared palette ----
type_colors <- c(
  core        = "#1B5E20",
  soft_core   = "#43A047",
  dispensable = "#E65100",
  private     = "#6A1B9A",
  singleton   = "#546E7A"
)

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

# ============================================================
# PANEL A — CV distribution histogram
# ============================================================
panelA_inner <- ggplot(cnv_f, aes(x = cv, fill = cluster_type)) +
  geom_histogram(bins = 60, alpha = 0.95, color = "black",
                 linewidth = 0.2, position = "stack") +
  scale_fill_manual(values = type_colors, name = "Cluster type") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
  labs(x = "Coefficient of variation (CV)",
       y = "Number of clusters") +
  theme_mp() +
  theme(legend.position = "top")

# ============================================================
# PANEL B — CV violin by cluster type
# ============================================================
panelB_inner <- ggplot(cnv_f, aes(x = cluster_type, y = cv, fill = cluster_type)) +
  geom_violin(alpha = 0.9, trim = FALSE, color = "black", linewidth = 0.5,
              scale = "width") +
  geom_boxplot(width = 0.14, fill = "white", color = "black",
               outlier.shape = NA, linewidth = 0.5) +
  scale_fill_manual(values = type_colors, guide = "none") +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
  labs(x = NULL, y = "Coefficient of variation") +
  theme_mp() +
  theme(axis.text.x = element_text(face = "bold"))

# ============================================================
# Assemble side by side
# ============================================================
composite <- (panelA_inner | panelB_inner) +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 34))
  )

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_cnv_landscape_multipanel.pdf",
       composite, width = 16, height = 7, device = cairo_pdf)
ggsave("figures/Fig_cnv_landscape_multipanel.png",
       composite, width = 16, height = 7, dpi = 300)

cat("Multipanel figure saved:\n")
cat("  figures/Fig_cnv_landscape_multipanel.pdf\n")
cat("  figures/Fig_cnv_landscape_multipanel.png\n")