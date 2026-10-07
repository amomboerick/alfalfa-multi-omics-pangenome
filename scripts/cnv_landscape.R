# scripts/cnv_landscape.R
# Copy number variation landscape -- Figures for paper

library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/cnv_landscape")

# ---- Load data ----
cnv <- read.csv("cluster_cnv.csv", stringsAsFactors = FALSE)
chrom <- read.csv("cluster_chromosomes.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(cnv), "clusters\n")

# Merge chromosome info
cnv <- cnv %>% left_join(chrom, by = "cluster_id")

# Filter: require at least 2 genes and present in >= 3 accessions (to have meaningful CV)
cnv_f <- cnv %>%
  filter(!is.na(cv), gene_count >= 3, n_accessions_present >= 3)

cat("After filtering:", nrow(cnv_f), "clusters\n")

# ---- Figure 1: Distribution of CV across the pan-genome ----
p1 <- ggplot(cnv_f, aes(x = cv, fill = cluster_type)) +
  geom_histogram(bins = 60, alpha = 0.85, color = "white", position = "stack") +
  scale_fill_manual(values = c(
    core        = "#1a4d38",
    soft_core   = "#2b7a5e",
    dispensable = "#d48c1a",
    private     = "#8b5e9b",
    singleton   = "#5a7a8a"
  )) +
  scale_x_continuous(limits = c(0, 4)) +
  labs(
    title = "Copy number variation across the pan-genome",
    subtitle = paste0("Coefficient of variation (SD / mean) per cluster. ",
                      "N = ", format(nrow(cnv_f), big.mark = ","), " clusters"),
    x = "Coefficient of variation (CV)",
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

ggsave("figures/Fig1_cv_distribution.pdf", p1, width = 10, height = 6,
       device = cairo_pdf)
ggsave("figures/Fig1_cv_distribution.png", p1, width = 10, height = 6,
       dpi = 300)
cat("Figure 1 saved\n")

# ---- Figure 2: CV by cluster type (violin) ----
p2 <- ggplot(cnv_f, aes(x = cluster_type, y = cv, fill = cluster_type)) +
  geom_violin(alpha = 0.8, trim = FALSE) +
  geom_boxplot(width = 0.15, fill = "white", color = "black",
               outlier.shape = NA) +
  scale_fill_manual(values = c(
    core        = "#1a4d38",
    soft_core   = "#2b7a5e",
    dispensable = "#d48c1a",
    private     = "#8b5e9b",
    singleton   = "#5a7a8a"
  )) +
  scale_y_continuous(limits = c(0, 3)) +
  labs(
    title = "Coefficient of variation by cluster type",
    subtitle = "Higher CV = more variable copy number across accessions",
    x = NULL, y = "Coefficient of variation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig2_cv_by_type.pdf", p2, width = 9, height = 6,
       device = cairo_pdf)
ggsave("figures/Fig2_cv_by_type.png", p2, width = 9, height = 6,
       dpi = 300)
cat("Figure 2 saved\n")

# ---- Figure 3: CNV density across chromosomes ----
# Filter top 200 most variable clusters, plot their positions by chromosome
top_cnv <- cnv_f %>%
  filter(!is.na(dominant_contig)) %>%
  arrange(desc(cv)) %>%
  head(200)

p3 <- ggplot(top_cnv, aes(x = start_pos / 1e6, y = cv, color = cluster_type)) +
  geom_point(size = 1.5, alpha = 0.7) +
  facet_wrap(~ dominant_contig, scales = "free_x", ncol = 4) +
  scale_color_manual(values = c(
    core        = "#1a4d38",
    soft_core   = "#2b7a5e",
    dispensable = "#d48c1a",
    private     = "#8b5e9b",
    singleton   = "#5a7a8a"
  )) +
  labs(
    title = "Top 200 high-CNV clusters by chromosome position",
    subtitle = "Points = clusters with highest copy number variation; x-axis = position (Mb)",
    x = "Position (Mb)", y = "Coefficient of variation",
    color = "Cluster type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    legend.position = "top",
    strip.text = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig3_cnv_chromosomes.pdf", p3, width = 14, height = 8,
       device = cairo_pdf)
ggsave("figures/Fig3_cnv_chromosomes.png", p3, width = 14, height = 8,
       dpi = 300)
cat("Figure 3 saved\n")

# ---- Supplementary tables ----
# Top 100 highest-CNV clusters
top100 <- cnv_f %>% arrange(desc(cv)) %>% head(100)
write.csv(top100, "figures/TableS1_top100_high_cnv.csv", row.names = FALSE)

# Summary per cluster type
summary_tbl <- cnv_f %>%
  group_by(cluster_type) %>%
  summarise(
    n_clusters = n(),
    mean_cv = round(mean(cv, na.rm = TRUE), 3),
    median_cv = round(median(cv, na.rm = TRUE), 3),
    pct_cv_gt_1 = round(100 * mean(cv > 1, na.rm = TRUE), 2),
    .groups = "drop"
  )
write.csv(summary_tbl, "figures/TableS2_cv_summary.csv", row.names = FALSE)

cat("\nSummary of CV by cluster type:\n")
print(summary_tbl)

cat("\nAll done.\n")