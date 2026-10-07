# scripts/crispr_conservation.R
# CRISPR targetability vs. pan-genome conservation -- Figures 1 and 2

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/crispr_conservation")

# ---- Load data ----
df <- read.csv("cluster_crispr.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "clusters\n")

# Convert NAs from LEFT JOIN to 0
df$total_guides[is.na(df$total_guides)] <- 0
df$unique_guides[is.na(df$unique_guides)] <- 0
df$multi_guides[is.na(df$multi_guides)] <- 0
df$genes_with_unique[is.na(df$genes_with_unique)] <- 0
df$pct_genes_with_unique[is.na(df$pct_genes_with_unique)] <- 0

# Keep only clusters with genes and guides (exclude very small noise clusters)
df_clean <- df %>%
  filter(!is.na(cluster_type), n_genes >= 3, total_guides > 0)

cat("Clusters with guides and >= 3 genes:", nrow(df_clean), "\n")

# Order cluster types
type_levels <- c("core", "soft_core", "dispensable", "private", "singleton")
df_clean$cluster_type <- factor(df_clean$cluster_type, levels = type_levels)
df_clean <- df_clean %>% filter(!is.na(cluster_type))

# ---- Summary table ----
summary_tbl <- df_clean %>%
  group_by(cluster_type) %>%
  summarise(
    n_clusters = n(),
    n_genes_total = sum(n_genes),
    mean_pct_unique = round(mean(pct_genes_with_unique, na.rm = TRUE), 2),
    median_pct_unique = round(median(pct_genes_with_unique, na.rm = TRUE), 2),
    mean_guides_per_gene = round(sum(total_guides) / sum(n_genes), 2),
    mean_unique_per_gene = round(sum(unique_guides) / sum(n_genes), 2),
    .groups = "drop"
  )

print(summary_tbl)
write.csv(summary_tbl, "figures/TableS1_crispr_summary.csv", row.names = FALSE)

# ---- Kruskal-Wallis test across cluster types ----
kw <- kruskal.test(pct_genes_with_unique ~ cluster_type, data = df_clean)
cat("\nKruskal-Wallis p-value:", format(kw$p.value, scientific = TRUE), "\n")

# Pairwise vs core
core_pct <- df_clean$pct_genes_with_unique[df_clean$cluster_type == "core"]
for (t in setdiff(type_levels, "core")) {
  sub <- df_clean$pct_genes_with_unique[df_clean$cluster_type == t]
  if (length(sub) > 0) {
    wt <- wilcox.test(core_pct, sub)
    cat(sprintf("  core vs %-12s : p = %s\n", t,
                format(wt$p.value, scientific = TRUE)))
  }
}

# ---- Figure 1: Violin + boxplot of % targetable genes ----
p1 <- ggplot(df_clean, aes(x = cluster_type, y = pct_genes_with_unique,
                            fill = cluster_type)) +
  geom_violin(alpha = 0.75, trim = FALSE, scale = "width") +
  geom_boxplot(width = 0.15, fill = "white", color = "black",
               outlier.shape = NA, alpha = 0.9) +
  stat_summary(fun = median, geom = "point", shape = 23,
               size = 3, fill = "#f5b342", color = "black") +
  scale_fill_manual(values = c(
    core        = "#1a4d38",
    soft_core   = "#2b7a5e",
    dispensable = "#d48c1a",
    private     = "#8b5e9b",
    singleton   = "#5a7a8a"
  )) +
  labs(
    title = "CRISPR targetability by pan-genome conservation",
    subtitle = paste0("Percentage of genes per cluster with at least one UNIQUE guide. ",
                      "Kruskal-Wallis p = ", format(kw$p.value, scientific = TRUE, digits = 3)),
    x = "Cluster type",
    y = "% of genes with UNIQUE guide"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig1_targetability_by_type.pdf", p1, width = 9, height = 6,
       device = cairo_pdf)
ggsave("figures/Fig1_targetability_by_type.png", p1, width = 9, height = 6,
       dpi = 300)
cat("\nFigure 1 saved\n")

# ---- Figure 2: UNIQUE vs MULTI guides by cluster type ----
df_long <- df_clean %>%
  select(cluster_type, unique_guides, multi_guides) %>%
  pivot_longer(cols = c(unique_guides, multi_guides),
               names_to = "guide_class", values_to = "count")

p2 <- ggplot(df_long, aes(x = cluster_type, y = count, fill = guide_class)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
  scale_fill_manual(values = c(unique_guides = "#1a4d38",
                                multi_guides  = "#c0392b"),
                    labels = c("UNIQUE (safe)", "MULTI (off-target risk)")) +
  scale_y_log10() +
  labs(
    title = "Guide RNA classes by cluster type",
    subtitle = "Count of UNIQUE and MULTI guides per cluster (log10 scale)",
    x = "Cluster type",
    y = "Guides per cluster (log10)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    axis.text.x = element_text(size = 11, face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig2_unique_vs_multi.pdf", p2, width = 9, height = 6,
       device = cairo_pdf)
ggsave("figures/Fig2_unique_vs_multi.png", p2, width = 9, height = 6,
       dpi = 300)
cat("Figure 2 saved\n")

# ---- Per-type distribution table ----
dist_tbl <- df_clean %>%
  group_by(cluster_type) %>%
  summarise(
    n = n(),
    q25_pct_unique = quantile(pct_genes_with_unique, 0.25, na.rm = TRUE),
    median_pct_unique = median(pct_genes_with_unique, na.rm = TRUE),
    q75_pct_unique = quantile(pct_genes_with_unique, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(dist_tbl, "figures/TableS2_pct_unique_quartiles.csv", row.names = FALSE)

cat("\nAll done.\n")