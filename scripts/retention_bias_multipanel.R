# ============================================================
# retention_bias_multipanel.R
# Multipanel 3-panel: A = retention histogram,
#                     B = expanded GO, C = lost GO
# Layout: A on top (full width), (B | C) below
# Output: figures/Fig_retention_bias_multipanel.pdf / .png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/retention_bias")

# ---- Load retention data ----
ret <- read.csv("cluster_retention.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(ret), "clusters\n")
ret_plot <- ret %>% filter(!is.na(retention_ratio))

# ---- Load GO counts ----
go <- read.csv("go_counts_by_bin.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(go), "GO rows\n")

go_wide <- go %>%
  pivot_wider(names_from = bin, values_from = n_genes,
              values_fill = list(n_genes = 0))
if (!"expanded"    %in% colnames(go_wide)) go_wide$expanded    <- 0
if (!"lost"        %in% colnames(go_wide)) go_wide$lost        <- 0
if (!"background"  %in% colnames(go_wide)) go_wide$background  <- 0

N_expanded    <- 308408
N_lost        <- 4421
N_background  <- 823838

# ---- Fisher's exact test helper ----
test_go <- function(bin_name, N_bin) {
  df <- go_wide %>%
    filter(.data[[bin_name]] >= 5) %>%
    mutate(
      a = .data[[bin_name]],
      b = N_bin - .data[[bin_name]],
      c = background - .data[[bin_name]],
      d = N_background - N_bin - (background - .data[[bin_name]]),
      pval = mapply(function(x1, x2, x3, x4)
        fisher.test(matrix(c(x1, x2, x3, x4), nrow = 2))$p.value,
        a, b, c, d)
    ) %>%
    mutate(
      FDR = p.adjust(pval, method = "BH"),
      fold_enrichment = (a / N_bin) / (background / N_background),
      neg_log10_FDR = -log10(FDR + 1e-300)
    ) %>%
    arrange(FDR)
  return(df)
}

expanded_df <- test_go("expanded", N_expanded)
lost_df     <- test_go("lost",     N_lost)
top_expanded <- expanded_df %>% slice_head(n = 20)
top_lost     <- lost_df %>% slice_head(n = 20)

# ---- Unified theme ----
theme_mp <- function(base = 14) {
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
# PANEL A — Retention ratio histogram
# ============================================================
panelA_inner <- ggplot(ret_plot, aes(x = retention_ratio)) +
  geom_histogram(bins = 60, fill = "#1B5E20", color = "black",
                 alpha = 0.95, linewidth = 0.2) +
  geom_vline(xintercept = c(0.5, 1.5, 3.0),
             linetype = c("dashed","solid","dashed"),
             color = c("#B71C1C","#FFA000","#6A1B9A"), linewidth = 0.9) +
  scale_x_continuous(limits = c(0, 8), breaks = 0:8) +
  annotate("text", x = 0.25, y = Inf, label = "Lost\n(<0.5)",
           vjust = 2, hjust = 0.5, color = "#B71C1C", size = 4, fontface = "bold") +
  annotate("text", x = 1.0, y = Inf, label = "Neutral\n(0.5-1.5)",
           vjust = 2, hjust = 0.5, color = "#FFA000", size = 4, fontface = "bold") +
  annotate("text", x = 2.25, y = Inf, label = "Expanded\n(1.5-3)",
           vjust = 2, hjust = 0.5, color = "#1B5E20", size = 4, fontface = "bold") +
  annotate("text", x = 5.0, y = Inf, label = "Highly expanded\n(>3)",
           vjust = 2, hjust = 0.5, color = "#6A1B9A", size = 4, fontface = "bold") +
  labs(x = "Retention ratio (tetraploid / diploid)",
       y = "Number of clusters") +
  theme_mp()

# ============================================================
# PANEL B — Expanded GO enrichment bubbles
# ============================================================
panelB_inner <- ggplot(top_expanded,
                        aes(x = fold_enrichment,
                            y = reorder(go_term, fold_enrichment),
                            size = a,
                            color = neg_log10_FDR)) +
  geom_point(alpha = 0.9) +
  scale_color_gradient(low = "#FFA000", high = "#1B5E20",
                       name = "-log10(FDR)") +
  scale_size_continuous(range = c(3, 12), name = "Genes") +
  labs(x = "Fold enrichment", y = NULL) +
  theme_mp() +
  theme(axis.text.y = element_text(size = 10))

# ============================================================
# PANEL C — Lost GO enrichment bubbles
# ============================================================
panelC_inner <- ggplot(top_lost,
                        aes(x = fold_enrichment,
                            y = reorder(go_term, fold_enrichment),
                            size = a,
                            color = neg_log10_FDR)) +
  geom_point(alpha = 0.9) +
  scale_color_gradient(low = "#FFA000", high = "#B71C1C",
                       name = "-log10(FDR)") +
  scale_size_continuous(range = c(3, 12), name = "Genes") +
  labs(x = "Fold enrichment", y = NULL) +
  theme_mp() +
  theme(axis.text.y = element_text(size = 10))

# ============================================================
# Assemble: A on top, (B | C) below
# ============================================================
top_row    <- panelA_inner
bottom_row <- (panelB_inner | panelC_inner)

composite <- top_row / bottom_row +
  plot_layout(heights = c(1, 1.6)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 34))
  )

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_retention_bias_multipanel.pdf",
       composite, width = 16, height = 14, device = cairo_pdf)
ggsave("figures/Fig_retention_bias_multipanel.png",
       composite, width = 16, height = 14, dpi = 300)

cat("Multipanel figure saved:\n")
cat("  figures/Fig_retention_bias_multipanel.pdf\n")
cat("  figures/Fig_retention_bias_multipanel.png\n")