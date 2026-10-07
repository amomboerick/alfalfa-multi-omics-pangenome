# scripts/chromosome_distribution.R
# Chromosomal distribution of core vs. dispensable genes

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/chromosome_distribution")

# ---- Load data ----
bins <- read.csv("chromosome_bins.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(bins), "bins\n")

# Fill NAs
for (col in c("n_core","n_soft_core","n_dispensable","n_private","n_singleton")) {
  bins[[col]][is.na(bins[[col]])] <- 0
}

# ---- Compute proportions per bin ----
bins <- bins %>%
  mutate(
    total_classified = n_core + n_soft_core + n_dispensable + n_private + n_singleton,
    pct_core        = 100 * n_core / pmax(total_classified, 1),
    pct_dispensable = 100 * n_dispensable / pmax(total_classified, 1),
    pct_private     = 100 * n_private / pmax(total_classified, 1)
  ) %>%
  filter(total_classified >= 5)  # only bins with enough genes

cat("After filtering:", nrow(bins), "bins\n")

# ---- Figure 1: Core vs Dispensable fraction per chromosome bin (one accession) ----
# Pick M_sativa_ZM1 as representative (largest chromosome set)
rep_acc <- "M_sativa_ZM1"
bins_rep <- bins %>% filter(accession_name == rep_acc)

if (nrow(bins_rep) == 0) {
  rep_acc <- bins$accession_name[1]
  bins_rep <- bins %>% filter(accession_name == rep_acc)
  cat("Using", rep_acc, "as representative accession\n")
}

# Long format
bins_rep_long <- bins_rep %>%
  select(contig_name, bin_mb, pct_core, pct_dispensable, pct_private) %>%
  pivot_longer(cols = starts_with("pct_"),
               names_to = "type", values_to = "pct") %>%
  mutate(type = recode(type,
                       pct_core = "Core",
                       pct_dispensable = "Dispensable",
                       pct_private = "Private"))

# Get top 10 largest contigs only (chromosome-scale)
top_contigs <- bins_rep %>%
  group_by(contig_name) %>%
  summarise(n = sum(total_classified), .groups = "drop") %>%
  arrange(desc(n)) %>%
  head(10) %>%
  pull(contig_name)

bins_rep_long <- bins_rep_long %>% filter(contig_name %in% top_contigs)

p1 <- ggplot(bins_rep_long, aes(x = bin_mb, y = pct, color = type)) +
  geom_line(size = 0.7, alpha = 0.9) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~ contig_name, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c(
    Core        = "#1a4d38",
    Dispensable = "#d48c1a",
    Private     = "#8b5e9b"
  )) +
  labs(
    title = paste0("Chromosomal distribution of gene classes — ", rep_acc),
    subtitle = paste0("Core vs. dispensable vs. private gene fraction per 1 Mb bin. ",
                      "Only bins with >=5 classified genes shown."),
    x = "Position (Mb)", y = "% of genes in bin",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    legend.position = "top",
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig1_chromosome_tracks.pdf", p1, width = 12, height = 8,
       device = cairo_pdf)
ggsave("figures/Fig1_chromosome_tracks.png", p1, width = 12, height = 8,
       dpi = 300)
cat("Figure 1 saved\n")

# ---- Figure 2: Histogram of core fraction vs. dispensable fraction ----
p2 <- ggplot(bins, aes(x = pct_core)) +
  geom_histogram(bins = 40, fill = "#1a4d38", color = "white", alpha = 0.85) +
  labs(
    title = "Distribution of core gene fraction across chromosome bins",
    subtitle = paste0("Each bin is a 1 Mb window in a specific accession. ",
                      "N = ", nrow(bins), " bins"),
    x = "% of genes classified as core", y = "Number of bins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig2_core_fraction_hist.pdf", p2, width = 10, height = 5,
       device = cairo_pdf)
ggsave("figures/Fig2_core_fraction_hist.png", p2, width = 10, height = 5,
       dpi = 300)
cat("Figure 2 saved\n")

# ---- Figure 3: Chromosome-edge vs center comparison ----
# For each contig, split bins into edges (bottom 10% and top 10%) vs center (middle)
edge_center <- bins %>%
  group_by(accession_name, contig_name) %>%
  mutate(
    contig_max = max(bin_mb, na.rm = TRUE),
    position = bin_mb / pmax(contig_max, 1),
    zone = case_when(
      position < 0.2 ~ "Start (0-20%)",
      position > 0.8 ~ "End (80-100%)",
      TRUE ~ "Center (20-80%)"
    )
  ) %>%
  ungroup() %>%
  filter(n_total >= 10) %>%
  group_by(zone) %>%
  summarise(
    mean_pct_core        = mean(pct_core, na.rm = TRUE),
    mean_pct_dispensable = mean(pct_dispensable, na.rm = TRUE),
    mean_pct_private     = mean(pct_private, na.rm = TRUE),
    n_bins = n(),
    .groups = "drop"
  )

print(edge_center)

edge_center_long <- edge_center %>%
  select(zone, mean_pct_core, mean_pct_dispensable, mean_pct_private) %>%
  pivot_longer(cols = starts_with("mean_pct_"),
               names_to = "type", values_to = "mean_pct") %>%
  mutate(type = recode(type,
                       mean_pct_core = "Core",
                       mean_pct_dispensable = "Dispensable",
                       mean_pct_private = "Private"))

p3 <- ggplot(edge_center_long, aes(x = zone, y = mean_pct, fill = type)) +
  geom_col(position = "dodge", alpha = 0.9, color = "white") +
  scale_fill_manual(values = c(
    Core        = "#1a4d38",
    Dispensable = "#d48c1a",
    Private     = "#8b5e9b"
  )) +
  labs(
    title = "Gene class composition across chromosome compartments",
    subtitle = "Chromosome edges (0-20%, 80-100%) vs center (20-80%)",
    x = NULL, y = "Mean % of genes",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "top",
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig3_edge_vs_center.pdf", p3, width = 9, height = 5,
       device = cairo_pdf)
ggsave("figures/Fig3_edge_vs_center.png", p3, width = 9, height = 5,
       dpi = 300)
cat("Figure 3 saved\n")

# ---- Tables ----
write.csv(edge_center, "figures/TableS1_edge_center_summary.csv", row.names = FALSE)
write.csv(bins, "figures/TableS2_chromosome_bins.csv", row.names = FALSE)

cat("\nAll done.\n")