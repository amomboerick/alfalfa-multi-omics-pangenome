# scripts/homeolog_candidates.R
# Homeolog retention candidates -- figures for paper

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/homeolog_candidates")

# ---- Load ----
df <- read.csv("homeolog_candidates.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "candidate clusters\n")

# Filter to sensible ratios (avoid log-scale issues)
df <- df %>% filter(!is.na(tetra_diploid_ratio), tetra_diploid_ratio > 0)

# ---- Figure 1: Distribution of tetra/diploid ratios ----
p1 <- ggplot(df, aes(x = tetra_diploid_ratio, fill = cluster_type)) +
  geom_histogram(bins = 60, alpha = 0.85, color = "white", position = "stack") +
  scale_fill_manual(values = c(
    core        = "#1a4d38",
    soft_core   = "#2b7a5e",
    dispensable = "#d48c1a",
    private     = "#8b5e9b",
    singleton   = "#5a7a8a"
  )) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 50, 100, 300)) +
  geom_vline(xintercept = 2, linetype = "dashed",
             color = "#c0392b", size = 0.9) +
  annotate("text", x = 2.2, y = Inf, label = "Pure autotetraploid\nexpectation (2x)",
           vjust = 2, hjust = 0, color = "#c0392b", size = 3.5, fontface = "bold") +
  labs(
    title = "Homeolog retention candidates",
    subtitle = paste0("Tetraploid/diploid gene count ratio across ", nrow(df),
                      " clusters with ≥3 genes in each tetraploid accession"),
    x = "Tetraploid / diploid gene count ratio (log scale)",
    y = "Number of clusters",
    fill = "Cluster type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig1_ratio_distribution.pdf", p1, width = 10, height = 6,
       device = cairo_pdf)
ggsave("figures/Fig1_ratio_distribution.png", p1, width = 10, height = 6,
       dpi = 300)
cat("Figure 1 saved\n")

# ---- Figure 2: Top 50 candidates as horizontal bars ----
top50 <- df %>% arrange(desc(tetra_diploid_ratio)) %>% head(50)
top50$label <- paste0(top50$cluster_name, "  (", top50$cluster_type, ")")

p2 <- ggplot(top50, aes(x = tetra_diploid_ratio,
                        y = reorder(label, tetra_diploid_ratio),
                        fill = cluster_type)) +
  geom_col(alpha = 0.9, color = "white") +
  geom_text(aes(label = paste0(gene_count, " / ", diploid_total)),
            hjust = -0.1, size = 3, fontface = "bold") +
  scale_fill_manual(values = c(
    core        = "#1a4d38",
    soft_core   = "#2b7a5e",
    dispensable = "#d48c1a",
    private     = "#8b5e9b"
  )) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Top 50 homeolog-retained clusters",
    subtitle = "Bars labeled as: total tetraploid genes / diploid genes",
    x = "Tetraploid / diploid ratio",
    y = NULL,
    fill = "Cluster type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "top",
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 80, 10, 10)
  )

ggsave("figures/Fig2_top50_candidates.pdf", p2, width = 10, height = 12,
       device = cairo_pdf)
ggsave("figures/Fig2_top50_candidates.png", p2, width = 10, height = 12,
       dpi = 300)
cat("Figure 2 saved\n")

# ---- Supplementary table ----
write.csv(df, "figures/TableS1_homeolog_candidates.csv", row.names = FALSE)

# Summary table
summary_tbl <- df %>%
  group_by(cluster_type) %>%
  summarise(
    n_candidates = n(),
    median_ratio = round(median(tetra_diploid_ratio, na.rm = TRUE), 2),
    mean_gene_count = round(mean(gene_count, na.rm = TRUE), 1),
    .groups = "drop"
  )
write.csv(summary_tbl, "figures/TableS2_summary.csv", row.names = FALSE)
print(summary_tbl)

cat("\nAll done.\n")