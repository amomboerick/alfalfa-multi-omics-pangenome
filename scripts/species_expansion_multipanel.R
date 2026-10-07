# ============================================================
# species_expansion_multipanel.R
# Multipanel: A = top 100 heatmap, B = expansion counts
# Unified theme, higher contrast, clean label layout
# Output: figures/Fig_species_expansion_multipanel.pdf / .png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(reshape2)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/species_expansion")

# ---- Load data ----
df <- read.csv("cluster_species_means.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "clusters\n")

species_cols <- c("sativa_tetraploid","sativa_diploid","ruthenica",
                  "truncatula","arabica","lupulina","polymorpha")

# ---- Compute metrics ----
df$pan_mean <- rowMeans(df[, species_cols], na.rm = TRUE)
df$pan_sd   <- apply(df[, species_cols], 1, sd, na.rm = TRUE)
df$cv       <- ifelse(df$pan_mean > 0, df$pan_sd / df$pan_mean, NA)
df$min_val  <- apply(df[, species_cols], 1, min, na.rm = TRUE)
df$max_val  <- apply(df[, species_cols], 1, max, na.rm = TRUE)

informative <- df %>%
  filter(!is.na(cv), cv > 0.5, max_val >= 3) %>%
  arrange(desc(cv))

cat("Informative clusters:", nrow(informative), "\n")

# ---- Pretty labels for species ----
label_map <- c(
  sativa_tetraploid = "M. sativa\ntetraploid",
  sativa_diploid    = "M. sativa\ndiploid",
  ruthenica         = "M. ruthenica",
  truncatula        = "M. truncatula",
  arabica           = "M. arabica",
  lupulina          = "M. lupulina",
  polymorpha        = "M. polymorpha"
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
      legend.text        = element_text(size = base - 3),
      legend.key.size    = unit(0.5, "cm"),
      plot.margin        = margin(14, 14, 12, 14),
      axis.line          = element_line(linewidth = 0.6, color = "black"),
      axis.ticks         = element_line(linewidth = 0.6, color = "black")
    )
}

# ============================================================
# PANEL A — Heatmap of top 100 clusters
# ============================================================
top100 <- head(informative, 100)
mat <- as.matrix(top100[, species_cols])
rownames(mat) <- top100$cluster_name
mat_scaled <- t(scale(t(mat)))

melted <- melt(mat_scaled)
colnames(melted) <- c("cluster","species","zscore")

# Apply pretty labels while preserving order
melted$species <- factor(
  label_map[as.character(melted$species)],
  levels = unname(label_map[species_cols])
)

panelA_inner <- ggplot(melted,
                        aes(x = species,
                            y = reorder(cluster, zscore, FUN = mean),
                            fill = zscore)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#6A1B9A", mid = "white", high = "#1B5E20",
                       midpoint = 0, name = "Z-score",
                       guide = guide_colorbar(barwidth = 0.8, barheight = 8)) +
  labs(x = NULL, y = NULL) +
  theme_mp() +
  theme(
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank(),
    axis.line.y     = element_blank(),
    axis.text.x     = element_text(angle = 30, hjust = 1, face = "bold", size = 13),
    legend.position = "right",
    plot.margin     = margin(20, 12, 30, 14)
  )

# ============================================================
# PANEL B — Species expansion counts
# ============================================================
expansion_counts <- data.frame(species = species_cols, n = 0)
for (sp in species_cols) {
  mask <- (df[[sp]] >= 3 * df$pan_mean) & (df$pan_mean >= 1) & (df[[sp]] >= 3)
  expansion_counts$n[expansion_counts$species == sp] <- sum(mask, na.rm = TRUE)
}

expansion_counts$label <- label_map[expansion_counts$species]
expansion_counts$label <- gsub("\n", " ", expansion_counts$label)
expansion_counts$label <- factor(expansion_counts$label,
                                 levels = expansion_counts$label[order(expansion_counts$n)])

panelB_inner <- ggplot(expansion_counts,
                       aes(x = label, y = n, fill = species)) +
  geom_col(color = "black", linewidth = 0.5, width = 0.72) +
  geom_text(aes(label = n), hjust = -0.25, size = 6, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c(
    sativa_tetraploid = "#1B5E20",
    sativa_diploid    = "#43A047",
    ruthenica         = "#00A651",
    truncatula        = "#7B2FBE",
    arabica           = "#F768A1",
    lupulina          = "#00CED1",
    polymorpha        = "#FFC300"
  ), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Number of expanded clusters") +
  theme_mp() +
  theme(
    axis.text.y = element_text(face = "bold", size = 15),
    plot.margin = margin(20, 20, 30, 12)
  )

# ============================================================
# Assemble
# ============================================================
composite <- (panelA_inner | panelB_inner) +
  plot_layout(widths = c(1, 1.15)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 34))
  )

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_species_expansion_multipanel.pdf",
       composite, width = 16, height = 8.5, device = cairo_pdf)
ggsave("figures/Fig_species_expansion_multipanel.png",
       composite, width = 16, height = 8.5, dpi = 300)

cat("Multipanel figure saved:\n")
cat("  figures/Fig_species_expansion_multipanel.pdf\n")
cat("  figures/Fig_species_expansion_multipanel.png\n")