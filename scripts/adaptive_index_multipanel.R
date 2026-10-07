# ============================================================
# adaptive_index_multipanel.R
# Multipanel: A = API scatter, B = top 30 candidates breakdown
# Unified theme, higher contrast, aligned panels
# Output: figures/Fig_adaptive_index_multipanel.pdf / .png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(patchwork)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/adaptive_index")

# ---- Load data ----
df <- read.csv("cluster_scores.csv", stringsAsFactors = FALSE)
cat("Loaded", nrow(df), "clusters\n")

df <- df %>% filter(gene_count >= 3, !is.na(cluster_type))

# ---- Compute components ----
max_unique <- max(df$n_unique_guides, na.rm = TRUE)

df <- df %>%
  mutate(
    specificity = 1 - (n_accessions_present / 12),
    stress      = has_stress_go,
    editability = log10(1 + n_unique_guides) / log10(1 + max_unique),
    API         = 0.4 * specificity + 0.4 * stress + 0.2 * editability
  ) %>%
  arrange(desc(API))

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
# PANEL A — API scatter
# ============================================================
df_top <- df %>% filter(API > 0.3)

# Pretty color label
df_top$stress_label <- ifelse(df_top$has_stress_go == 1,
                              "Stress/defense GO", "No stress GO")

panelA_inner <- ggplot(df_top, aes(x = n_accessions_present,
                                    y = n_unique_guides,
                                    color = stress_label,
                                    size = API)) +
  geom_point(alpha = 0.75) +
  scale_color_manual(values = c("No stress GO" = "#6BAED6",
                                 "Stress/defense GO" = "#B71C1C"),
                     name = NULL) +
  scale_size_continuous(range = c(1.5, 7), name = "API",
                        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8)) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_y_log10() +
  labs(x = "Number of accessions where cluster is present",
       y = "UNIQUE CRISPR guides (log10)") +
  theme_mp() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0))

# ============================================================
# PANEL B — Top 30 breakdown
# ============================================================
top30 <- head(df, 30)
top30_long <- top30 %>%
  select(cluster_name, specificity, stress, editability, API) %>%
  pivot_longer(cols = c(specificity, stress, editability),
               names_to = "component", values_to = "value")

top30_long$component <- factor(top30_long$component,
                               levels = c("specificity", "stress", "editability"),
                               labels = c("Specificity", "Stress GO", "Editability"))

panelB_inner <- ggplot(top30_long, aes(x = reorder(cluster_name, API),
                                        y = value, fill = component)) +
  geom_col(position = position_dodge(width = 0.85),
           alpha = 0.95, color = "black", linewidth = 0.3, width = 0.78) +
  coord_flip() +
  scale_fill_manual(values = c("Specificity" = "#1B5E20",
                                "Stress GO"   = "#B71C1C",
                                "Editability" = "#FFA000"),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25)) +
  labs(x = NULL, y = "Component value (0-1)") +
  theme_mp() +
  theme(axis.text.y = element_text(size = 9, face = "plain"),
        legend.position = "top")

# ============================================================
# Assemble
# ============================================================
composite <- (panelA_inner | panelB_inner) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 34))
  )

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_adaptive_index_multipanel.pdf",
       composite, width = 16, height = 9, device = cairo_pdf)
ggsave("figures/Fig_adaptive_index_multipanel.png",
       composite, width = 16, height = 9, dpi = 300)

cat("Multipanel figure saved:\n")
cat("  figures/Fig_adaptive_index_multipanel.pdf\n")
cat("  figures/Fig_adaptive_index_multipanel.png\n")