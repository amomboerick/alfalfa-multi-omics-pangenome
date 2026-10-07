# scripts/retention_enrichment.R
# Polyploidy retention bias analysis -- figures for paper
# Uses only cluster_retention.csv + reads DB for enrichment background

library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)

# ---- Configuration ----
setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/retention_bias")

DB <- list(host="localhost", port=5433, dbname="alfalfa_pangenome",
           user="postgres", password="postgres")

# ---- Load retention data ----
ret <- read.csv("cluster_retention.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(ret), "clusters\n")

# ---- Figure 1: Histogram of retention ratios ----
ret_plot <- ret %>% filter(!is.na(retention_ratio))

p1 <- ggplot(ret_plot, aes(x = retention_ratio)) +
  geom_histogram(bins = 60, fill = "#1a4d38", color = "white", alpha = 0.85) +
  geom_vline(xintercept = c(0.5, 1.5, 3.0),
             linetype = c("dashed","solid","dashed"),
             color = c("#c0392b","#f5b342","#8b5e9b"), size = 1) +
  scale_x_continuous(limits = c(0, 8), breaks = 0:8) +
  labs(
    title = "Retention ratio of pan-gene clusters (tetraploid / diploid)",
    x = "Retention ratio (M. sativa tetraploid / M. caerulea diploid)",
    y = "Number of clusters"
  ) +
  annotate("text", x = 0.25, y = Inf, label = "Lost\n(< 0.5)",
           vjust = 2, hjust = 0.5, color = "#c0392b", size = 4, fontface = "bold") +
  annotate("text", x = 1.0, y = Inf, label = "Neutral\n(0.5-1.5)",
           vjust = 2, hjust = 0.5, color = "#d48c1a", size = 4, fontface = "bold") +
  annotate("text", x = 2.25, y = Inf, label = "Expanded\n(1.5-3)",
           vjust = 2, hjust = 0.5, color = "#2b7a5e", size = 4, fontface = "bold") +
  annotate("text", x = 5.0, y = Inf, label = "Highly expanded\n(> 3)",
           vjust = 2, hjust = 0.5, color = "#8b5e9b", size = 4, fontface = "bold") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("figures/Fig1_retention_histogram.pdf", p1, width = 9, height = 6, device = cairo_pdf)
ggsave("figures/Fig1_retention_histogram.png", p1, width = 9, height = 6, dpi = 300)

cat("Figure 1 saved\n")

# ---- Summarise bin counts for Table 1 ----
bin_counts <- ret %>%
  mutate(bin = case_when(
    is.na(retention_ratio) & diploid_genes == 0 & tetraploid_genes == 0 ~ "absent_both",
    is.na(retention_ratio) & diploid_genes == 0 & tetraploid_genes > 0 ~ "tetraploid_only",
    retention_ratio < 0.5 ~ "lost",
    retention_ratio < 1.5 ~ "neutral",
    retention_ratio < 3.0 ~ "expanded",
    TRUE ~ "highly_expanded"
  )) %>%
  count(bin, sort = TRUE)

write.csv(bin_counts, "figures/Table1_bin_counts.csv", row.names = FALSE)
print(bin_counts)

cat("\nAll done.\n")