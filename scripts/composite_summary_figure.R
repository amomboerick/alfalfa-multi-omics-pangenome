# ============================================================
# composite_summary_figure.R  (v2)
# 2x2 composite: A = phylogeny, B = CNV, C = annotation, D = API
# Letters only, no titles. Larger letters. Higher contrast.
# ============================================================

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis")
dir.create("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/composite_summary",
           showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(ape)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(cowplot)
  library(grid)
  library(gridGraphics)
})

# ============================================================
# Shared theme
# ============================================================
theme_composite <- function(base = 15) {
  theme_classic(base_size = base) +
    theme(
      plot.title         = element_blank(),
      plot.subtitle      = element_blank(),
      axis.title         = element_text(face = "bold", size = base),
      axis.text          = element_text(size = base - 2, color = "black"),
      legend.title       = element_text(face = "bold", size = base - 2),
      legend.text        = element_text(size = base - 3, color = "black"),
      legend.key.size    = unit(0.5, "cm"),
      plot.margin        = margin(20, 16, 12, 12),
      panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.4),
      axis.line          = element_line(linewidth = 0.6, color = "black"),
      axis.ticks         = element_line(linewidth = 0.6, color = "black")
    )
}

# ============================================================
# PANEL A — Phylogeny (no title, no legend, fills whole panel)
# ============================================================
setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/phylogenetics")
tree_ml <- read.tree("alfalfa_tree_sub.treefile")

color_for <- function(label) {
  if (grepl("XJDY", label))              return("#D95F02")
  if (grepl("ZM1",  label))              return("#E41A1C")
  if (grepl("ZM4",  label))              return("#FF7F00")
  if (grepl("caerulea_landa", label))    return("#1F78B4")
  if (grepl("caerulea_long",  label))    return("#00A0E0")
  if (grepl("ruthenica_landa",    label))return("#00A651")
  if (grepl("ruthenica_zhiwusuo", label))return("#006400")
  if (grepl("A17",   label))             return("#7B2FBE")
  if (grepl("HM078", label))             return("#C77CFF")
  if (grepl("arabica",    label))        return("#F768A1")
  if (grepl("lupulina",   label))        return("#00CED1")
  if (grepl("polymorpha", label))        return("#FFC300")
  return("#000000")
}
tip_cols_ml <- sapply(tree_ml$tip.label, color_for)

plot_phylo <- function() {
  par(mar = c(1, 0, 2, 1), family = "sans")
  plot(tree_ml,
       cex          = 1.9,
       tip.color    = tip_cols_ml,
       label.offset = 0.0005,
       main         = "",
       edge.width   = 2.8,
       edge.color   = "black",
       x.lim        = c(0, max(node.depth.edgelength(tree_ml)) * 1.35))
  add.scale.bar(length = 0.005, cex = 1.2, lwd = 2.5)
}

panelA <- ggdraw() +
  draw_plot(plot_phylo, x = 0.02, y = 0.02, width = 0.96, height = 0.96) +
  draw_label("A", x = 0.015, y = 0.98,
             hjust = 0, vjust = 1,
             fontface = "bold", size = 34, color = "black")

# ============================================================
# PANEL B — CNV
# ============================================================
setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/cnv_landscape")
cnv <- read.csv("cluster_cnv.csv", stringsAsFactors = FALSE)
chrom <- read.csv("cluster_chromosomes.csv", stringsAsFactors = FALSE)
cnv <- cnv %>% left_join(chrom, by = "cluster_id")
cnv_f <- cnv %>%
  filter(!is.na(cv), gene_count >= 3, n_accessions_present >= 3)

cnv_f$cluster_type <- factor(cnv_f$cluster_type,
                             levels = c("core", "soft_core", "dispensable", "private"))

panelB_inner <- ggplot(cnv_f, aes(x = cluster_type, y = cv, fill = cluster_type)) +
  geom_violin(trim = FALSE, alpha = 1.0, color = "black", linewidth = 0.7) +
  geom_boxplot(width = 0.14, fill = "white", color = "black",
               outlier.shape = NA, linewidth = 0.6) +
  scale_fill_manual(values = c("core" = "#1B5E20",
                                "soft_core" = "#43A047",
                                "dispensable" = "#F57C00",
                                "private" = "#6A1B9A"),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  labs(x = NULL, y = "CV of gene count") +
  theme_composite()

panelB <- ggdraw() +
  draw_plot(panelB_inner, x = 0, y = 0, width = 1, height = 1) +
  draw_label("B", x = 0.015, y = 0.98,
             hjust = 0, vjust = 1,
             fontface = "bold", size = 34, color = "black")

# ============================================================
# PANEL C — Annotation coverage
# ============================================================
setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/annotation_coverage")
ann <- read.csv("Table1_annotation_coverage_stats.csv", stringsAsFactors = FALSE)
ann$cluster_type <- factor(ann$cluster_type,
                           levels = c("core", "soft_core", "dispensable", "private"))

ann_long <- ann %>%
  select(cluster_type, pct_go, pct_kegg, pct_kog) %>%
  pivot_longer(cols = c(pct_go, pct_kegg, pct_kog),
               names_to = "annotation", values_to = "percent")
ann_long$annotation <- factor(ann_long$annotation,
                              levels = c("pct_go", "pct_kegg", "pct_kog"),
                              labels = c("GO", "KEGG KO", "KOG"))

panelC_inner <- ggplot(ann_long, aes(x = cluster_type, y = percent, fill = annotation)) +
  geom_col(position = position_dodge(width = 0.85),
           width = 0.75, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("GO" = "#0D47A1",
                                "KEGG KO" = "#E65100",
                                "KOG" = "#1B5E20"),
                    name = "Annotation") +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
  labs(x = NULL, y = "% annotated") +
  theme_composite() +
  theme(legend.position = "top")

panelC <- ggdraw() +
  draw_plot(panelC_inner, x = 0, y = 0, width = 1, height = 1) +
  draw_label("C", x = 0.015, y = 0.98,
             hjust = 0, vjust = 1,
             fontface = "bold", size = 34, color = "black")

# ============================================================
# PANEL D — Adaptive Priority Index
# ============================================================
setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/adaptive_index")
df <- read.csv("cluster_scores.csv", stringsAsFactors = FALSE)
df <- df %>% filter(gene_count >= 3, !is.na(cluster_type))

max_unique <- max(df$n_unique_guides, na.rm = TRUE)
df <- df %>%
  mutate(
    specificity = 1 - (n_accessions_present / 12),
    stress      = has_stress_go,
    editability = log10(1 + n_unique_guides) / log10(1 + max_unique),
    API         = 0.4 * specificity + 0.4 * stress + 0.2 * editability
  )
df_top <- df %>% filter(API > 0.3)
df_top$stress_label <- ifelse(df_top$has_stress_go == 1,
                              "Stress/defense GO", "No stress GO")

panelD_inner <- ggplot(df_top, aes(x = n_accessions_present,
                                   y = n_unique_guides,
                                   color = stress_label,
                                   size = API)) +
  geom_point(alpha = 0.75) +
  scale_color_manual(values = c("No stress GO" = "#6BAED6",
                                 "Stress/defense GO" = "#B71C1C"),
                     name = NULL) +
  scale_size_continuous(range = c(1.5, 7), name = "API",
                        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8)) +
  scale_y_log10() +
  labs(x = "Number of accessions where cluster is present",
       y = "UNIQUE CRISPR guides (log10)") +
  theme_composite() +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

panelD <- ggdraw() +
  draw_plot(panelD_inner, x = 0, y = 0, width = 1, height = 1) +
  draw_label("D", x = 0.015, y = 0.98,
             hjust = 0, vjust = 1,
             fontface = "bold", size = 34, color = "black")

# ============================================================
# Assemble 2x2, no overall title
# ============================================================
composite <- (panelA | panelB) / (panelC | panelD)

# ============================================================
# Save
# ============================================================
setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/composite_summary")

ggsave("Fig1_composite_summary.pdf", composite,
       width = 16, height = 12, device = cairo_pdf)
ggsave("Fig1_composite_summary.png", composite,
       width = 16, height = 12, dpi = 200)

cat("Composite figure saved to analysis/composite_summary/\n")