# ============================================================
# crispr_conservation_multipanel.R
# Multipanel: A = targetability by cluster type, B = UNIQUE vs MULTI
# Unified theme, higher contrast, aligned panels
# Output: figures/Fig_crispr_conservation_multipanel.pdf / .png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/crispr_conservation")

# ---- Load data ----
df <- read.csv("cluster_crispr.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "clusters\n")

df$total_guides[is.na(df$total_guides)] <- 0
df$unique_guides[is.na(df$unique_guides)] <- 0
df$multi_guides[is.na(df$multi_guides)] <- 0
df$genes_with_unique[is.na(df$genes_with_unique)] <- 0
df$pct_genes_with_unique[is.na(df$pct_genes_with_unique)] <- 0

df_clean <- df %>%
  filter(!is.na(cluster_type), n_genes >= 3, total_guides > 0)

cat("Clusters with guides and >= 3 genes:", nrow(df_clean), "\n")

type_levels <- c("core", "soft_core", "dispensable", "private", "singleton")
df_clean$cluster_type <- factor(df_clean$cluster_type, levels = type_levels)
df_clean <- df_clean %>% filter(!is.na(cluster_type))

# ---- Kruskal-Wallis test ----
kw <- kruskal.test(pct_genes_with_unique ~ cluster_type, data = df_clean)
cat("\nKruskal-Wallis p-value:", format(kw$p.value, scientific = TRUE), "\n")

# ---- Unified theme ----
theme_mp <- function(base = 15) {
  theme_classic(base_size = base) +
    theme(
      plot.title         = element_text(face = "bold", size = base + 3, hjust = 0),
      plot.subtitle      = element_text(size = base - 3, color = "gray30", hjust = 0),
      axis.title         = element_text(face = "bold", size = base),
      axis.text          = element_text(size = base - 1, color = "black"),
      legend.title       = element_text(face = "bold", size = base - 2),
      legend.text        = element_text(size = base - 2),
      legend.key.size    = unit(0.5, "cm"),
      plot.margin        = margin(20, 14, 14, 14),
      axis.line          = element_line(linewidth = 0.6, color = "black"),
      axis.ticks         = element_line(linewidth = 0.6, color = "black")
    )
}

# ---- Consistent palette ----
type_colors <- c(
  core        = "#1B5E20",
  soft_core   = "#43A047",
  dispensable = "#E65100",
  private     = "#6A1B9A",
  singleton   = "#546E7A"
)

# ============================================================
# PANEL A — Targetability violin + box
# ============================================================
panelA_inner <- ggplot(df_clean,
                        aes(x = cluster_type, y = pct_genes_with_unique,
                            fill = cluster_type)) +
  geom_violin(alpha = 0.9, trim = FALSE, scale = "width",
              color = "black", linewidth = 0.5) +
  geom_boxplot(width = 0.14, fill = "white", color = "black",
               outlier.shape = NA, linewidth = 0.5) +
  scale_fill_manual(values = type_colors, guide = "none") +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
  labs(x = NULL, y = "% of genes with UNIQUE guide") +
  theme_mp() +
  theme(axis.text.x = element_text(face = "bold"))

# ============================================================
# PANEL B — UNIQUE vs MULTI boxplots
# ============================================================
df_long <- df_clean %>%
  select(cluster_type, unique_guides, multi_guides) %>%
  pivot_longer(cols = c(unique_guides, multi_guides),
               names_to = "guide_class", values_to = "count")

# Pretty legend labels
df_long$guide_class <- factor(df_long$guide_class,
                              levels = c("unique_guides", "multi_guides"),
                              labels = c("UNIQUE (safe)", "MULTI (off-target risk)"))

panelB_inner <- ggplot(df_long, aes(x = cluster_type, y = count,
                                     fill = guide_class)) +
  geom_boxplot(outlier.size = 0.6, alpha = 0.95, linewidth = 0.5,
               color = "black") +
  scale_fill_manual(values = c("UNIQUE (safe)" = "#1B5E20",
                                "MULTI (off-target risk)" = "#B71C1C"),
                    name = NULL) +
  scale_y_log10(breaks = c(1, 10, 100, 1000)) +
  labs(x = NULL, y = "Guides per cluster (log10)") +
  theme_mp() +
  theme(axis.text.x = element_text(face = "bold"),
        legend.position = "top")

# ============================================================
# Assemble
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
ggsave("figures/Fig_crispr_conservation_multipanel.pdf",
       composite, width = 16, height = 7, device = cairo_pdf)
ggsave("figures/Fig_crispr_conservation_multipanel.png",
       composite, width = 16, height = 7, dpi = 300)

cat("Multipanel figure saved:\n")
cat("  figures/Fig_crispr_conservation_multipanel.pdf\n")
cat("  figures/Fig_crispr_conservation_multipanel.png\n")