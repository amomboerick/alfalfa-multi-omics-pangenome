# ============================================================
# plot_tree_multipanel.R
# Multipanel phylogeny figure: A = ML tree, B = bootstrap consensus
# 12-accession color scheme, side by side, shared legend
# v2: doubled fonts, higher contrast
# ============================================================

library(ape)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/phylogenetics")
dir.create("figures", showWarnings = FALSE)

# ---- Read both trees ----
tree_ml  <- read.tree("alfalfa_tree_sub.treefile")
tree_con <- read.tree("alfalfa_tree_sub.contree")

cat("ML tree tips:", length(tree_ml$tip.label), "\n")
cat("Consensus tree tips:", length(tree_con$tip.label), "\n")

# ---- 12-accession color function (brighter palette) ----
color_for <- function(label) {
  if (grepl("XJDY", label))            return("#E6550D")  # bright orange
  if (grepl("ZM1",  label))            return("#D62728")  # strong red
  if (grepl("ZM4",  label))            return("#FF7F00")  # vivid orange

  if (grepl("caerulea_landa", label))  return("#1F78B4")  # strong blue
  if (grepl("caerulea_long",  label))  return("#00B0F0")  # sky blue

  if (grepl("ruthenica_landa",    label)) return("#00A651")  # vivid green
  if (grepl("ruthenica_zhiwusuo", label)) return("#006400")  # dark green

  if (grepl("A17",   label))           return("#7B2FBE")  # strong purple
  if (grepl("HM078", label))           return("#C77CFF")  # light violet

  if (grepl("arabica",    label))      return("#F768A1")  # hot pink
  if (grepl("lupulina",   label))      return("#00CED1")  # bright teal
  if (grepl("polymorpha", label))      return("#FFC300")  # bright gold

  return("#000000")
}

tip_cols_ml  <- sapply(tree_ml$tip.label,  color_for)
tip_cols_con <- sapply(tree_con$tip.label, color_for)

# ---- Shared legend data ----
legend_labels <- c(
  "M. sativa tetraploid — XJDY",
  "M. sativa tetraploid — ZM1",
  "M. sativa tetraploid — ZM4_hap4",
  "M. sativa diploid — caerulea landa",
  "M. sativa diploid — caerulea long",
  "M. ruthenica — landa",
  "M. ruthenica — zhiwusuo",
  "M. truncatula — A17",
  "M. truncatula — HM078",
  "M. arabica",
  "M. lupulina",
  "M. polymorpha"
)
legend_cols <- c(
  "#E6550D", "#D62728", "#FF7F00",
  "#1F78B4", "#00B0F0",
  "#00A651", "#006400",
  "#7B2FBE", "#C77CFF",
  "#F768A1", "#00CED1", "#FFC300"
)

# ============================================================
# Draw both PDF and PNG using the same drawing function
# ============================================================
draw_panels <- function() {
  layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE),
         heights = c(7, 2.4))
  par(family = "sans")

  # ---- Panel A: ML tree ----
  par(mar = c(2, 2, 4, 2))
  plot(tree_ml,
       cex          = 2.0,           # doubled
       tip.color    = tip_cols_ml,
       label.offset = 0.001,
       main         = "A  Maximum-likelihood tree\n(561 single-copy orthologs; 105,518 aa)",
       col.main     = "#000000", font.main = 2, cex.main = 2.2,
       edge.width   = 2.6,           # thicker = more contrast
       edge.color   = "#000000")
  add.scale.bar(length = 0.005, cex = 1.8, lwd = 2.5)

  # ---- Panel B: Bootstrap consensus tree ----
  par(mar = c(2, 2, 4, 2))
  plot(tree_con,
       cex          = 2.0,
       tip.color    = tip_cols_con,
       label.offset = 0.001,
       main         = "B  Bootstrap consensus\n(1,000 replicates; node labels = % support)",
       col.main     = "#000000", font.main = 2, cex.main = 2.2,
       edge.width   = 2.6,
       edge.color   = "#000000")
  add.scale.bar(length = 0.005, cex = 1.8, lwd = 2.5)
  nodelabels(tree_con$node.label, cex = 1.5, frame = "none",
             col = "#B22222", font = 2)

  # ---- Shared legend (bottom strip) ----
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center",
         legend    = legend_labels,
         col       = legend_cols,
         pch       = 19,
         pt.cex    = 3.2,          # bigger dots
         cex       = 1.9,          # bigger text
         ncol      = 4,
         bty       = "n",
         x.intersp = 0.7,
         y.intersp = 1.3)
}

# ---- Export PDF ----
pdf("figures/Fig_phylogeny_multipanel.pdf", width = 18, height = 11)
draw_panels()
dev.off()

# ---- Export PNG ----
png("figures/Fig_phylogeny_multipanel.png", width = 3600, height = 2200, res = 200)
draw_panels()
dev.off()

cat("Multipanel figures saved in figures/\n")