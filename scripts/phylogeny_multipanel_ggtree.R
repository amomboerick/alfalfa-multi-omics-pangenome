# ============================================================
# phylogeny_multipanel_ggtree.R
# TPJ-style 2-panel phylogeny using ggtree + cowplot
# A = ML tree, B = bootstrap consensus
# ============================================================

suppressPackageStartupMessages({
  library(ggtree)
  library(treeio)
  library(ape)
  library(ggplot2)
  library(dplyr)
  library(cowplot)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/phylogenetics")
dir.create("figures", showWarnings = FALSE)

# ---- Read both trees ----
tree_ml  <- read.tree("alfalfa_tree_sub.treefile")
tree_con <- read.tree("alfalfa_tree_sub.contree")

cat("ML tree tips:", length(tree_ml$tip.label), "\n")
cat("Consensus tree tips:", length(tree_con$tip.label), "\n")

# ---- Pretty labels ----
pretty_map <- c(
  "M_arabica"                    = "M. arabica",
  "M_lupulina"                   = "M. lupulina",
  "M_polymorpha"                 = "M. polymorpha",
  "M_ruthenica_landa"            = "M. ruthenica landa",
  "M_ruthenica_zhiwusuo"         = "M. ruthenica zhiwusuo",
  "M_sativa_XJDY"                = "M. sativa XJDY",
  "M_sativa_ZM1"                 = "M. sativa ZM1",
  "M_sativa_ZM4_hap4"            = "M. sativa ZM4_hap4",
  "M_sativa_ssp_caerulea_landa"  = "M. sativa ssp. caerulea landa",
  "M_sativa_ssp_caerulea_long"   = "M. sativa ssp. caerulea long",
  "M_truncatula_A17"             = "M. truncatula A17",
  "M_truncatula_HM078"           = "M. truncatula HM078"
)

# ---- Tip colors ----
color_map <- c(
  "M_sativa_XJDY"                = "#E6550D",
  "M_sativa_ZM1"                 = "#D62728",
  "M_sativa_ZM4_hap4"            = "#FF7F00",
  "M_sativa_ssp_caerulea_landa"  = "#1F78B4",
  "M_sativa_ssp_caerulea_long"   = "#0086C0",
  "M_ruthenica_landa"            = "#00793E",
  "M_ruthenica_zhiwusuo"         = "#006400",
  "M_truncatula_A17"             = "#7B2FBE",
  "M_truncatula_HM078"           = "#9C4DCC",
  "M_arabica"                    = "#D6336C",
  "M_lupulina"                   = "#0097A7",
  "M_polymorpha"                 = "#C79100"
)

# ---- Clade highlight colors ----
clade_fills <- list(
  sativa_tetra = "#FFD9CC",
  sativa_dip   = "#CCE5FF",
  ruthenica    = "#CCFFCC",
  truncatula   = "#E5CCFF"
)

# ---- Find MRCA node for a clade ----
find_mrca <- function(tree, tips) {
  tips <- tips[tips %in% tree$tip.label]
  if (length(tips) < 2) return(NA)
  ape::getMRCA(tree, tips)
}

# ============================================================
# Build one tree panel
# ============================================================
build_panel <- function(tree, panel_label, panel_title, show_support = FALSE) {

  # Tip metadata
  tip_df <- data.frame(
    label  = tree$tip.label,
    pretty = pretty_map[tree$tip.label],
    color  = color_map[tree$tip.label],
    stringsAsFactors = FALSE
  )
  rownames(tip_df) <- tip_df$label

  # Base tree
  p <- ggtree(tree, layout = "rectangular",
              linewidth = 0.9, color = "#222222") %<+% tip_df

  # Clade highlights
  tetra <- find_mrca(tree, c("M_sativa_XJDY","M_sativa_ZM1","M_sativa_ZM4_hap4"))
  if (!is.na(tetra)) {
    p <- p + geom_hilight(node = tetra, fill = clade_fills$sativa_tetra,
                          alpha = 0.5, extend = 0.15)
  }
  dip <- find_mrca(tree, c("M_sativa_ssp_caerulea_landa","M_sativa_ssp_caerulea_long"))
  if (!is.na(dip)) {
    p <- p + geom_hilight(node = dip, fill = clade_fills$sativa_dip,
                          alpha = 0.5, extend = 0.15)
  }
  rut <- find_mrca(tree, c("M_ruthenica_landa","M_ruthenica_zhiwusuo"))
  if (!is.na(rut)) {
    p <- p + geom_hilight(node = rut, fill = clade_fills$ruthenica,
                          alpha = 0.5, extend = 0.15)
  }
  tru <- find_mrca(tree, c("M_truncatula_A17","M_truncatula_HM078"))
  if (!is.na(tru)) {
    p <- p + geom_hilight(node = tru, fill = clade_fills$truncatula,
                          alpha = 0.5, extend = 0.15)
  }

  # Tip labels and points
  p <- p +
    geom_tippoint(aes(color = label), size = 4, show.legend = FALSE) +
    geom_tiplab(aes(label = pretty, color = label),
                size = 6.5, fontface = "bold",
                offset = 0.004, align = FALSE, show.legend = FALSE) +
    scale_color_manual(values = color_map)

  # Node points
  if (show_support) {
    p <- p +
      geom_nodepoint(aes(size = as.numeric(label)),
                     color = "#B71C1C", alpha = 0.9, show.legend = FALSE) +
      scale_size_continuous(range = c(1.5, 5))
  } else {
    p <- p + geom_nodepoint(color = "#333333", size = 1.6, alpha = 0.8)
  }

  # Scale bar and theme
  p <- p +
    geom_treescale(x = 0.4, y = -1.5, width = 0.005,
                   fontsize = 5, linesize = 1.2, color = "black") +
    theme_tree2() +
    theme(
      plot.title      = element_text(face = "bold", size = 14, hjust = 0),
      plot.margin     = margin(15, 140, 15, 20),
      legend.position = "none",
      axis.line.x     = element_line(linewidth = 0.6),
      axis.text.x     = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()
    ) +
    ggtitle(paste0(panel_label, "  ", panel_title))

  return(p)
}

# ============================================================
# Build both panels
# ============================================================
pA <- build_panel(tree_ml,
                  "A",
                  "Maximum-likelihood tree (561 single-copy orthologs; 105,518 aa)",
                  show_support = FALSE)

pB <- build_panel(tree_con,
                  "B",
                  "Bootstrap consensus (1,000 replicates; node labels = % support)",
                  show_support = TRUE)

# ============================================================
# Combine with cowplot (NOT patchwork)
# ============================================================
composite <- cowplot::plot_grid(pA, pB, ncol = 2, rel_widths = c(1, 1))

# ============================================================
# Save
# ============================================================
ggsave("figures/Fig_phylogeny_ggtree_multipanel.pdf",
       composite, width = 18, height = 8, device = cairo_pdf)
ggsave("figures/Fig_phylogeny_ggtree_multipanel.png",
       composite, width = 18, height = 8, dpi = 300)

cat("\nMultipanel figure saved:\n")
cat("  figures/Fig_phylogeny_ggtree_multipanel.pdf\n")
cat("  figures/Fig_phylogeny_ggtree_multipanel.png\n")
