# scripts/adaptive_index.R
# Composite Adaptive Priority Index (API) -- top candidates for functional studies

library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/adaptive_index")

# ---- Load data ----
df <- read.csv("cluster_scores.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "clusters\n")

# ---- Filter: only clusters with >= 3 genes to exclude noise ----
df <- df %>% filter(gene_count >= 3, !is.na(cluster_type))
cat("After gene_count >= 3 filter:", nrow(df), "\n")

# ---- Compute components ----
max_unique <- max(df$n_unique_guides, na.rm = TRUE)

df <- df %>%
  mutate(
    # Specificity: 1 when present in 1 accession, 0 when present in 12
    specificity = 1 - (n_accessions_present / 12),
    
    # Stress relevance
    stress = has_stress_go,
    
    # Editability: log-normalized
    editability = log10(1 + n_unique_guides) / log10(1 + max_unique),
    
    # Composite API
    API = 0.4 * specificity + 0.4 * stress + 0.2 * editability
  ) %>%
  arrange(desc(API))

# ---- Summary ----
cat("\nTop 5 by API:\n")
print(head(df[, c("cluster_id","cluster_name","cluster_type",
                   "gene_count","n_accessions_present","has_stress_go",
                   "n_unique_guides","API")], 10))

# Save the full ranked table
write.csv(df, "figures/Table1_ranked_adaptive_index.csv", row.names = FALSE)

# Top 100 for the paper
write.csv(head(df, 100), "figures/TableS1_top100_API.csv", row.names = FALSE)

cat("\nRanked table saved\n")

# ---- Figure 1: Composite API vs. specificity with stress highlight ----
df_top <- df %>% filter(API > 0.3)

p1 <- ggplot(df_top, aes(x = n_accessions_present,
                          y = n_unique_guides,
                          color = factor(has_stress_go),
                          size = API)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("0" = "#8fa7b3", "1" = "#c0392b"),
                     labels = c("No stress GO", "Stress/defense GO"),
                     name = NULL) +
  scale_size_continuous(range = c(1, 6), name = "API") +
  scale_x_continuous(breaks = 1:12) +
  scale_y_log10() +
  labs(
    title = "Composite Adaptive Priority Index",
    subtitle = paste0("Clusters with high API = few accessions + stress annotation + many UNIQUE guides. ",
                      "N = ", nrow(df_top), " clusters"),
    x = "Number of accessions where cluster is present",
    y = "UNIQUE CRISPR guides per cluster (log10)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig1_API_scatter.pdf", p1, width = 10, height = 6,
       device = cairo_pdf)
ggsave("figures/Fig1_API_scatter.png", p1, width = 10, height = 6,
       dpi = 300)
cat("Figure 1 saved\n")

# ---- Figure 2: Top 30 ranked clusters with component breakdown ----
top30 <- head(df, 30)

top30_long <- top30 %>%
  select(cluster_name, specificity, stress, editability, API) %>%
  pivot_longer(cols = c(specificity, stress, editability),
               names_to = "component", values_to = "value")

p2 <- ggplot(top30_long, aes(x = reorder(cluster_name, API),
                              y = value, fill = component)) +
  geom_col(position = "dodge", alpha = 0.9, color = "white") +
  coord_flip() +
  scale_fill_manual(values = c(specificity = "#1a4d38",
                                stress      = "#c0392b",
                                editability = "#f5b342"),
                    labels = c("Editability", "Specificity", "Stress GO")) +
  labs(
    title = "Top 30 adaptive candidates by composite index",
    subtitle = "Component contributions to the API score",
    x = NULL, y = "Component value (0-1)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "top",
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

ggsave("figures/Fig2_top30_candidates.pdf", p2, width = 10, height = 9,
       device = cairo_pdf)
ggsave("figures/Fig2_top30_candidates.png", p2, width = 10, height = 9,
       dpi = 300)
cat("Figure 2 saved\n")

# ---- Distribution summary ----
cat("\nTop API scores summary:\n")
print(summary(df$API))
cat("\nNumber with API > 0.5:", sum(df$API > 0.5, na.rm = TRUE), "\n")
cat("Number with API > 0.7:", sum(df$API > 0.7, na.rm = TRUE), "\n")
cat("Number with API > 0.9:", sum(df$API > 0.9, na.rm = TRUE), "\n")

cat("\nAll done.\n")