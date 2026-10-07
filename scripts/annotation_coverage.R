# ============================================================
# annotation_coverage.R
# Analysis #5: Annotation coverage bias across pan-gene cluster types
# Input:  analysis/annotation_coverage/Table1_annotation_coverage_stats.csv
# Output: analysis/annotation_coverage/figures/Fig1_annotation_coverage.pdf / .png
#         analysis/annotation_coverage/TableS1_annotation_coverage.csv
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/annotation_coverage")

# ---- Read data ----
df <- read.csv("Table1_annotation_coverage_stats.csv", stringsAsFactors = FALSE)

# Order cluster types from most conserved to least
df$cluster_type <- factor(df$cluster_type,
                          levels = c("core", "soft_core", "dispensable", "private"))

# Reshape to long format for grouped bars
long <- df %>%
  select(cluster_type, pct_go, pct_kegg, pct_kog) %>%
  pivot_longer(cols = c(pct_go, pct_kegg, pct_kog),
               names_to = "annotation",
               values_to = "percent")

long$annotation <- factor(long$annotation,
                          levels = c("pct_go", "pct_kegg", "pct_kog"),
                          labels = c("GO", "KEGG KO", "KOG"))

# ---- Colors: one per annotation layer, high contrast ----
ann_cols <- c("GO" = "#1F78B4", "KEGG KO" = "#E66101", "KOG" = "#33A02C")

# ---- Figure ----
p <- ggplot(long, aes(x = cluster_type, y = percent, fill = annotation)) +
  geom_col(position = position_dodge(width = 0.85),
           width = 0.75,
           color = "black", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f", percent)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, size = 5.2, fontface = "bold") +
  scale_fill_manual(values = ann_cols, name = "Annotation layer") +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  labs(x = "Pan-gene cluster type",
       y = "% of genes with at least one annotation",
       title = "Annotation coverage across pan-gene cluster types",
       subtitle = "In Medicago pan-genome (548,164 clustered genes; singleton clusters excluded)") +
  theme_classic(base_size = 18) +
  theme(
    plot.title      = element_text(face = "bold", size = 22, hjust = 0),
    plot.subtitle   = element_text(size = 14, color = "#444444", hjust = 0),
    axis.title      = element_text(face = "bold", size = 18),
    axis.text       = element_text(size = 16, color = "black"),
    axis.text.x     = element_text(face = "bold"),
    legend.title    = element_text(face = "bold", size = 15),
    legend.text     = element_text(size = 14),
    legend.position = "top",
    legend.key.size = unit(0.8, "cm"),
    panel.grid.major.y = element_line(color = "#EEEEEE", linewidth = 0.4),
    plot.margin     = margin(15, 20, 15, 15)
  )

# ---- Save PDF ----
ggsave("figures/Fig1_annotation_coverage.pdf", p, width = 10, height = 7.5, device = cairo_pdf)

# ---- Save PNG ----
ggsave("figures/Fig1_annotation_coverage.png", p, width = 10, height = 7.5, dpi = 300)

# ---- Save TableS1 (clean long format for supplement) ----
write.csv(long, "TableS1_annotation_coverage.csv", row.names = FALSE)

cat("Done. Files written:\n")
cat("  figures/Fig1_annotation_coverage.pdf\n")
cat("  figures/Fig1_annotation_coverage.png\n")
cat("  TableS1_annotation_coverage.csv\n")