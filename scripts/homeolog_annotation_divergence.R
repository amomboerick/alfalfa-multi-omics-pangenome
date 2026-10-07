# ============================================================
# homeolog_annotation_divergence.R
# Computes pairwise Jaccard of GO terms AND KOG letters
# for genes within each homeolog-retained cluster.
# Output: figures/Fig_homeolog_annotation_multipanel.pdf / .png
#         TableS1_homeolog_annotation.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/homeolog_annotation")
dir.create("figures", showWarnings = FALSE)

# ---- Load ----
go  <- read.csv("homeolog_go_terms.csv", stringsAsFactors = FALSE)
kog <- read.csv("homeolog_kog_letters.csv", stringsAsFactors = FALSE)
cat("GO rows:",  nrow(go),  "\n")
cat("KOG rows:", nrow(kog), "\n")

# ============================================================
# Helper: compute mean pairwise Jaccard per cluster
# ============================================================
jaccard_pair <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  u <- length(union(a, b))
  if (u == 0) return(NA_real_)
  length(intersect(a, b)) / u
}

compute_cluster_jaccard <- function(cluster_genes) {
  n <- nrow(cluster_genes)
  if (n < 3) return(NULL)
  vals <- numeric(0)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      jv <- jaccard_pair(cluster_genes$terms[[i]], cluster_genes$terms[[j]])
      if (!is.na(jv)) vals <- c(vals, jv)
    }
  }
  if (length(vals) == 0) return(NULL)
  data.frame(
    cluster_id     = cluster_genes$cluster_id[1],
    cluster_type   = cluster_genes$cluster_type[1],
    n_genes        = n,
    n_pairs        = length(vals),
    mean_jaccard   = mean(vals),
    median_jaccard = median(vals),
    min_jaccard    = min(vals),
    max_jaccard    = max(vals),
    frac_identical = mean(vals == 1),
    frac_zero      = mean(vals == 0)
  )
}

# ---- GO ----
gene_go <- go %>%
  group_by(cluster_id, cluster_type, gene_id) %>%
  summarise(terms = list(unique(go_term)), .groups = "drop")

cat("\nComputing GO Jaccard per cluster...\n")
clusters_go <- split(gene_go, gene_go$cluster_id)
results_go  <- do.call(rbind, lapply(clusters_go, compute_cluster_jaccard))
names(results_go)[names(results_go) == "mean_jaccard"]   <- "mean_jaccard_go"
names(results_go)[names(results_go) == "median_jaccard"] <- "median_jaccard_go"
cat("GO clusters:", nrow(results_go), "\n")

# ---- KOG ----
gene_kog <- kog %>%
  group_by(cluster_id, cluster_type, gene_id) %>%
  summarise(terms = list(unique(cog_letter)), .groups = "drop")

cat("\nComputing KOG Jaccard per cluster...\n")
clusters_kog <- split(gene_kog, gene_kog$cluster_id)
results_kog  <- do.call(rbind, lapply(clusters_kog, compute_cluster_jaccard))
names(results_kog)[names(results_kog) == "mean_jaccard"]   <- "mean_jaccard_kog"
names(results_kog)[names(results_kog) == "median_jaccard"] <- "median_jaccard_kog"
cat("KOG clusters:", nrow(results_kog), "\n")

# ---- Merge ----
merged <- full_join(
  results_go  %>% select(cluster_id, cluster_type, n_genes,
                          mean_jaccard_go,  median_jaccard_go),
  results_kog %>% select(cluster_id, n_genes_kog = n_genes,
                          mean_jaccard_kog, median_jaccard_kog),
  by = "cluster_id"
)
merged$cluster_type <- factor(merged$cluster_type,
                               levels = c("core", "soft_core", "dispensable"))

cat("\nMerged clusters with both GO and KOG:", 
    sum(!is.na(merged$mean_jaccard_go) & !is.na(merged$mean_jaccard_kog)), "\n")

write.csv(merged, "TableS1_homeolog_annotation.csv", row.names = FALSE)

# ---- Summary ----
summary_tbl <- merged %>%
  group_by(cluster_type) %>%
  summarise(
    n = n(),
    mean_jaccard_go  = round(mean(mean_jaccard_go,  na.rm = TRUE), 3),
    mean_jaccard_kog = round(mean(mean_jaccard_kog, na.rm = TRUE), 3),
    pct_go_divergent  = round(100 * mean(mean_jaccard_go  < 0.5, na.rm = TRUE), 1),
    pct_kog_divergent = round(100 * mean(mean_jaccard_kog < 0.5, na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\n=== Summary ===\n")
print(summary_tbl)

# ---- Shared palette ----
type_colors <- c(
  core        = "#1B5E20",
  soft_core   = "#43A047",
  dispensable = "#E65100"
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
# PANEL A — GO Jaccard histogram
# ============================================================
panelA_inner <- ggplot(merged, aes(x = mean_jaccard_go)) +
  geom_histogram(bins = 40, fill = "#1B5E20", color = "black",
                 alpha = 0.95, linewidth = 0.2) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Mean pairwise Jaccard similarity (GO terms)",
       y = "Number of homeolog clusters") +
  theme_mp()

# ============================================================
# PANEL B — KOG Jaccard histogram
# ============================================================
panelB_inner <- ggplot(merged, aes(x = mean_jaccard_kog)) +
  geom_histogram(bins = 20, fill = "#B71C1C", color = "black",
                 alpha = 0.95, linewidth = 0.2) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Mean pairwise Jaccard similarity (KOG letters)",
       y = "Number of homeolog clusters") +
  theme_mp()

# ============================================================
# PANEL C — Scatter: GO vs KOG Jaccard
# ============================================================
panelC_inner <- ggplot(merged, aes(x = mean_jaccard_go,
                                    y = mean_jaccard_kog,
                                    color = cluster_type)) +
  geom_point(alpha = 0.5, size = 1.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray40", linewidth = 0.6) +
  scale_color_manual(values = type_colors, name = "Cluster type") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Mean Jaccard — GO terms",
       y = "Mean Jaccard — KOG letters") +
  theme_mp() +
  theme(legend.position = "bottom")

# ============================================================
# Assemble (A | B) / C
# ============================================================
composite <- (panelA_inner | panelB_inner) / panelC_inner +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 34))
  )

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_homeolog_annotation_multipanel.pdf",
       composite, width = 14, height = 12, device = cairo_pdf)
ggsave("figures/Fig_homeolog_annotation_multipanel.png",
       composite, width = 14, height = 12, dpi = 300)

cat("\nMultipanel figure saved:\n")
cat("  figures/Fig_homeolog_annotation_multipanel.pdf\n")
cat("  figures/Fig_homeolog_annotation_multipanel.png\n")