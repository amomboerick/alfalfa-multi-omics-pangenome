# scripts/species_expansion.R
# Species-specific gene family expansions -- Figures 1 and 2

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/species_expansion")

# ---- Load data ----
df <- read.csv("cluster_species_means.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "clusters\n")

species_cols <- c("sativa_tetraploid","sativa_diploid","ruthenica",
                  "truncatula","arabica","lupulina","polymorpha")

# ---- Compute mean and CV across species ----
df$pan_mean <- rowMeans(df[, species_cols], na.rm = TRUE)
df$pan_sd   <- apply(df[, species_cols], 1, sd, na.rm = TRUE)
df$cv       <- ifelse(df$pan_mean > 0, df$pan_sd / df$pan_mean, NA)

# Max/min ratio (only when all species have > 0)
df$min_val <- apply(df[, species_cols], 1, min, na.rm = TRUE)
df$max_val <- apply(df[, species_cols], 1, max, na.rm = TRUE)
df$max_min_ratio <- ifelse(df$min_val > 0, df$max_val / df$min_val, NA)

# ---- Filter to informative clusters ----
# Keep clusters with cv > 0.5 AND max_val >= 3 (real genes, not singleton noise)
informative <- df %>%
  filter(!is.na(cv), cv > 0.5, max_val >= 3) %>%
  arrange(desc(cv))

cat("Informative clusters (cv > 0.5, max >= 3):", nrow(informative), "\n")

# ---- Figure 1: Heatmap of top 100 most variable clusters ----
top100 <- head(informative, 100)
mat <- as.matrix(top100[, species_cols])
rownames(mat) <- top100$cluster_name

# Scale per row (so each cluster is z-scored across species)
mat_scaled <- t(scale(t(mat)))

melted <- melt(mat_scaled)
colnames(melted) <- c("cluster","species","zscore")

p1 <- ggplot(melted, aes(x = species, y = reorder(cluster, zscore, FUN = mean),
                          fill = zscore)) +
  geom_tile(color = "white", size = 0.3) +
  scale_fill_gradient2(low = "#8b5e9b", mid = "white", high = "#1a4d38",
                       midpoint = 0, name = "Z-score") +
  labs(
    title = "Top 100 most variable gene families across Medicago species",
    subtitle = "Each row is a pan-gene cluster, z-scored across species groups",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

ggsave("figures/Fig1_species_heatmap.pdf", p1, width = 9, height = 12,
       device = cairo_pdf)
ggsave("figures/Fig1_species_heatmap.png", p1, width = 9, height = 12,
       dpi = 300)
cat("Figure 1 saved\n")

# ---- Figure 2: Species-specific expansion counts ----
# For each species, count clusters where that species has max > 3x pan mean
expansion_counts <- data.frame(species = species_cols, n = 0)

for (sp in species_cols) {
  # species gene count > 3x pan mean AND pan_mean >= 1
  mask <- (df[[sp]] >= 3 * df$pan_mean) & (df$pan_mean >= 1) & (df[[sp]] >= 3)
  expansion_counts$n[expansion_counts$species == sp] <- sum(mask, na.rm = TRUE)
}

cat("Species expansion counts:\n")
print(expansion_counts)

p2 <- ggplot(expansion_counts, aes(x = reorder(species, n), y = n, fill = species)) +
  geom_col(alpha = 0.85, color = "white") +
  geom_text(aes(label = n), hjust = -0.2, size = 4, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c(
    sativa_tetraploid = "#1a4d38",
    sativa_diploid    = "#2b7a5e",
    ruthenica         = "#d48c1a",
    truncatula        = "#f5b342",
    arabica           = "#8b5e9b",
    lupulina          = "#5a9b7e",
    polymorpha        = "#b36b0e"
  )) +
  labs(
    title = "Species-expanded gene families",
    subtitle = "Clusters with gene count >= 3x the pan-genome mean in that species",
    x = NULL, y = "Number of expanded clusters"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "none",
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("figures/Fig2_species_expansion_counts.pdf", p2, width = 9, height = 5,
       device = cairo_pdf)
ggsave("figures/Fig2_species_expansion_counts.png", p2, width = 9, height = 5,
       dpi = 300)
cat("Figure 2 saved\n")

# ---- Save the full informative table as CSV ----
write.csv(informative, "figures/TableS1_informative_clusters.csv",
          row.names = FALSE)
cat("Table S1 saved\n")

cat("\nAll done.\n")