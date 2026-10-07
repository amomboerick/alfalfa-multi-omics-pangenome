# ============================================================
# homeolog_candidates_multipanel.R
# Multipanel: A = ratio distribution, B = top 50 candidates
# Unified theme, higher contrast, aligned panels
# Output: figures/Fig_homeolog_candidates_multipanel.pdf / .png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/homeolog_candidates")

# ---- Load ----
df <- read.csv("homeolog_candidates.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "candidate clusters\n")

df <- df %>% filter(!is.na(tetra_diploid_ratio), tetra_diploid_ratio > 0)

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

# ---- Shared palette ----
type_colors <- c(
  core        = "#1B5E20",
  soft_core   = "#43A047",
  dispensable = "#E65100",
  private     = "#6A1B9A",
  singleton   = "#546E7A"
)

# ============================================================
# PANEL A — Distribution of ratios
# ============================================================
panelA_inner <- ggplot(df, aes(x = tetra_diploid_ratio, fill = cluster_type)) +
  geom_histogram(bins = 60, alpha = 0.95, color = "black",
                 linewidth = 0.2, position = "stack") +
  scale_fill_manual(values = type_colors, name = "Cluster type") +
  scale_x_log10(breaks = c(1, 2, 5, 10, 50, 100, 300)) +
  geom_vline(xintercept = 2, linetype = "dashed",
             color = "#000000", linewidth = 0.9) +
  annotate("text", x = 2.5, y = 155,
           label = "Autotetraploid expectation (2x)",
           hjust = 0, vjust = 0, color = "black",
           size = 4.0, fontface = "bold") +
  labs(x = "Tetraploid / diploid gene count ratio (log scale)",
       y = "Number of clusters") +
  theme_mp() +
  theme(legend.position = "top")

# ============================================================
# PANEL B — Top 50 candidates
# ============================================================
top50 <- df %>% arrange(desc(tetra_diploid_ratio)) %>% head(50)
top50$label <- paste0(top50$cluster_name, "  (", top50$cluster_type, ")")

panelB_inner <- ggplot(top50, aes(x = tetra_diploid_ratio,
                                   y = reorder(label, tetra_diploid_ratio),
                                   fill = cluster_type)) +
  geom_col(alpha = 0.98, color = "black", linewidth = 0.3, width = 0.78) +
  geom_text(aes(label = paste0(gene_count, " / ", diploid_total)),
            hjust = -0.08, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = type_colors, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  coord_cartesian(clip = "off") +
  labs(x = "Tetraploid / diploid ratio", y = NULL) +
  theme_mp() +
  theme(axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(20, 90, 14, 14))

# ============================================================
# Assemble
# ============================================================
composite <- (panelA_inner | panelB_inner) +
  plot_layout(widths = c(1, 1.7)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 34))
  )

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_homeolog_candidates_multipanel.pdf",
       composite, width = 16, height = 10, device = cairo_pdf)
ggsave("figures/Fig_homeolog_candidates_multipanel.png",
       composite, width = 16, height = 10, dpi = 300)

cat("Multipanel figure saved:\n")
cat("  figures/Fig_homeolog_candidates_multipanel.pdf\n")
cat("  figures/Fig_homeolog_candidates_multipanel.png\n")