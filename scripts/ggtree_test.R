# ============================================================
# ggtree_test.R
# Quick test render of the Medicago phylogeny using ggtree
# Output: analysis/phylogenetics/figures/ggtree_test.png
# ============================================================

suppressPackageStartupMessages({
  library(ggtree)
  library(treeio)
  library(ggplot2)
  library(dplyr)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/phylogenetics")
dir.create("figures", showWarnings = FALSE)

# ---- Read tree ----
tree <- read.tree("alfalfa_tree_sub.treefile")

# ---- Tip metadata: color per accession (12-color scheme) ----
color_for <- function(label) {
  if (grepl("XJDY", label))              return("#E6550D")
  if (grepl("ZM1",  label))              return("#D62728")
  if (grepl("ZM4",  label))              return("#FF7F00")
  if (grepl("caerulea_landa", label))    return("#1F78B4")
  if (grepl("caerulea_long",  label))    return("#00B0F0")
  if (grepl("ruthenica_landa",    label))return("#00A651")
  if (grepl("ruthenica_zhiwusuo", label))return("#006400")
  if (grepl("A17",   label))             return("#7B2FBE")
  if (grepl("HM078", label))             return("#C77CFF")
  if (grepl("arabica",    label))        return("#F768A1")
  if (grepl("lupulina",   label))        return("#00CED1")
  if (grepl("polymorpha", label))        return("#FFC300")
  return("#000000")
}

tip_df <- data.frame(
  label = tree$tip.label,
  color = sapply(tree$tip.label, color_for),
  stringsAsFactors = FALSE
)

# ---- Pretty labels ----
pretty_label <- function(x) {
  x <- gsub("_", " ", x)
  x <- gsub("^M sativa ssp caerulea landa$", "M. sativa ssp. caerulea landa", x)
  x <- gsub("^M sativa ssp caerulea long$",  "M. sativa ssp. caerulea long",  x)
  x <- gsub("^M sativa XJDY$",         "M. sativa XJDY",         x)
  x <- gsub("^M sativa ZM1$",          "M. sativa ZM1",          x)
  x <- gsub("^M sativa ZM4 hap4$",     "M. sativa ZM4_hap4",     x)
  x <- gsub("^M truncatula A17$",      "M. truncatula A17",      x)
  x <- gsub("^M truncatula HM078$",    "M. truncatula HM078",    x)
  x <- gsub("^M ruthenica landa$",     "M. ruthenica landa",     x)
  x <- gsub("^M ruthenica zhiwusuo$",  "M. ruthenica zhiwusuo",  x)
  x <- gsub("^M arabica$",             "M. arabica",             x)
  x <- gsub("^M lupulina$",            "M. lupulina",            x)
  x <- gsub("^M polymorpha$",          "M. polymorpha",          x)
  x
}

tip_df$pretty <- sapply(tip_df$label, pretty_label)
rownames(tip_df) <- tip_df$label

# ---- ggtree plot ----
p <- ggtree(tree, layout = "rectangular", linewidth = 1.0, color = "#333333") %<+% tip_df +
  geom_tiplab(aes(color = label), size = 5, fontface = "bold",
              align = FALSE, offset = 0.002, show.legend = FALSE) +
  geom_tippoint(aes(color = label), size = 4, show.legend = FALSE) +
  scale_color_manual(values = setNames(tip_df$color, tip_df$label)) +
  geom_nodepoint(color = "#B71C1C", size = 1.5, alpha = 0.7) +
  geom_treescale(x = 0, y = -1, width = 0.005, fontsize = 4, linesize = 0.8,
                 color = "black") +
  theme_tree2() +
  theme(
    plot.margin = margin(20, 60, 20, 20),
    plot.title  = element_text(face = "bold", size = 18, hjust = 0)
  ) +
  ggtitle("Medicago phylogeny — ggtree test render")

# ---- Save ----
ggsave("figures/ggtree_test.png", p, width = 12, height = 8, dpi = 200)

cat("Test figure saved: figures/ggtree_test.png\n")
cat("Look at it and tell me:\n")
cat("  - Do you like the rectangular layout?\n")
cat("  - Are the tip labels readable?\n")
cat("  - Should we add clade background shading?\n")
cat("  - Any changes you want before the full figure?\n")