# scripts/retention_go_enrichment.R
# Fisher's exact test for GO enrichment in expanded vs lost bins.
# Figures 2 and 3 for the polyploidy retention paper.

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Configuration ----
setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/retention_bias")

# Total gene counts per set (from SQL queries)
N_expanded    <- 308408
N_lost        <- 4421
N_background  <- 823838

# ---- Load GO counts ----
go <- read.csv("go_counts_by_bin.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(go), "rows\n")

# ---- Pivot to wide format (one row per GO term) ----
go_wide <- go %>%
  pivot_wider(names_from = bin, values_from = n_genes,
              values_fill = list(n_genes = 0))

# Make sure all three columns exist
if (!"expanded"    %in% colnames(go_wide)) go_wide$expanded    <- 0
if (!"lost"        %in% colnames(go_wide)) go_wide$lost        <- 0
if (!"background"  %in% colnames(go_wide)) go_wide$background  <- 0

# ---- Filter: only test terms with >= 5 genes in the bin ----
# (avoids noise from singleton terms)
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

cat("Expanded significant terms (FDR < 0.05):",
    sum(expanded_df$FDR < 0.05), "\n")
cat("Lost significant terms (FDR < 0.05):",
    sum(lost_df$FDR < 0.05), "\n")

# ---- Top 20 for each figure ----
top_expanded <- expanded_df %>% slice_head(n = 20)
top_lost     <- lost_df %>% slice_head(n = 20)

# ---- Figure 2: Expanded enrichment ----
p2 <- ggplot(top_expanded,
             aes(x = fold_enrichment,
                 y = reorder(go_term, fold_enrichment),
                 size = a,
                 color = neg_log10_FDR)) +
  geom_point(alpha = 0.85) +
  scale_color_gradient(low = "#f5b342", high = "#1a4d38") +
  scale_size_continuous(range = c(3, 12)) +
  labs(
    title = "GO enrichment in expanded clusters (tetraploid > diploid)",
    subtitle = paste0("Top 20 terms, Fisher's exact test, BH-FDR corrected. ",
                      "Total expanded genes: ", format(N_expanded, big.mark = ",")),
    x = "Fold enrichment",
    y = NULL,
    size = "Genes",
    color = "-log10(FDR)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig2_expanded_GO_enrichment.pdf", p2,
       width = 10, height = 7, device = cairo_pdf)
ggsave("figures/Fig2_expanded_GO_enrichment.png", p2,
       width = 10, height = 7, dpi = 300)

cat("Figure 2 saved\n")

# ---- Figure 3: Lost enrichment ----
p3 <- ggplot(top_lost,
             aes(x = fold_enrichment,
                 y = reorder(go_term, fold_enrichment),
                 size = a,
                 color = neg_log10_FDR)) +
  geom_point(alpha = 0.85) +
  scale_color_gradient(low = "#f5b342", high = "#c0392b") +
  scale_size_continuous(range = c(3, 12)) +
  labs(
    title = "GO enrichment in lost clusters (diploid > tetraploid)",
    subtitle = paste0("Top 20 terms, Fisher's exact test, BH-FDR corrected. ",
                      "Total lost genes: ", format(N_lost, big.mark = ",")),
    x = "Fold enrichment",
    y = NULL,
    size = "Genes",
    color = "-log10(FDR)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig3_lost_GO_enrichment.pdf", p3,
       width = 10, height = 7, device = cairo_pdf)
ggsave("figures/Fig3_lost_GO_enrichment.png", p3,
       width = 10, height = 7, dpi = 300)

cat("Figure 3 saved\n")

# ---- Save the full enrichment tables as CSV for supplementary ----
write.csv(expanded_df, "figures/TableS2_expanded_GO_full.csv", row.names = FALSE)
write.csv(lost_df,     "figures/TableS3_lost_GO_full.csv",     row.names = FALSE)

cat("Supplementary tables saved\n")
cat("All done.\n")