# scripts/plot_tree.R
# Plot the alfalfa phylogeny with species-colored tips

library(ape)

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/analysis/phylogenetics")
dir.create("figures", showWarnings = FALSE)

# ---- Read the ML tree ----
tree <- read.tree("alfalfa_tree_sub.treefile")
cat("Tree has", length(tree$tip.label), "tips\n")
cat("Tip labels:\n"); print(tree$tip.label)

# ---- Assign colors by species group ----
color_for <- function(label) {
  if (grepl("^M_sativa_(XJDY|ZM1|ZM4)", label))       return("#1a4d38") # tetraploid sativa
  if (grepl("^M_sativa_ssp_caerulea", label))         return("#2b7a5e") # diploid caerulea
  if (grepl("^M_ruthenica", label))                   return("#d48c1a") # ruthenica
  if (grepl("^M_truncatula", label))                  return("#f5b342") # truncatula
  if (grepl("^M_arabica", label))                     return("#8b5e9b") # arabica
  if (grepl("^M_lupulina", label))                    return("#5a9b7e") # lupulina
  if (grepl("^M_polymorpha", label))                  return("#b36b0e") # polymorpha
  return("#000000")
}

tip_cols <- sapply(tree$tip.label, color_for)

# ---- Draw the tree ----
pdf("figures/Fig_phylogeny.pdf", width = 10, height = 7)
par(mar = c(2, 2, 3, 2), family = "sans")
plot(tree,
     cex = 1.2,
     tip.color = tip_cols,
     label.offset = 0.001,
     main = "Medicago phylogeny (561 single-copy orthologs, 105,518 aa)",
     col.main = "#0B0B0B", font.main = 2, cex.main = 1.2)
add.scale.bar(length = 0.005, cex = 0.9)
legend("bottomleft",
       legend = c("M. sativa tetraploid", "M. sativa diploid", "M. ruthenica",
                  "M. truncatula", "M. arabica", "M. lupulina", "M. polymorpha"),
       col    = c("#1a4d38", "#2b7a5e", "#d48c1a", "#f5b342",
                  "#8b5e9b", "#5a9b7e", "#b36b0e"),
       pch = 19, bty = "n", cex = 1.0)
dev.off()

# ---- PNG version ----
png("figures/Fig_phylogeny.png", width = 2400, height = 1700, res = 200)
par(mar = c(2, 2, 3, 2), family = "sans")
plot(tree,
     cex = 1.2,
     tip.color = tip_cols,
     label.offset = 0.001,
     main = "Medicago phylogeny (561 single-copy orthologs)",
     col.main = "#0B0B0B", font.main = 2, cex.main = 1.2)
add.scale.bar(length = 0.005, cex = 0.9)
legend("bottomleft",
       legend = c("M. sativa tetraploid", "M. sativa diploid", "M. ruthenica",
                  "M. truncatula", "M. arabica", "M. lupulina", "M. polymorpha"),
       col    = c("#1a4d38", "#2b7a5e", "#d48c1a", "#f5b342",
                  "#8b5e9b", "#5a9b7e", "#b36b0e"),
       pch = 19, bty = "n", cex = 1.0)
dev.off()

# ---- Also plot with bootstrap support from the consensus tree ----
tree2 <- read.tree("alfalfa_tree_sub.contree")

pdf("figures/Fig_phylogeny_bootstrap.pdf", width = 10, height = 7)
par(mar = c(2, 2, 3, 2), family = "sans")
plot(tree2,
     cex = 1.2,
     tip.color = tip_cols,
     label.offset = 0.001,
     main = "Medicago phylogeny with bootstrap support (1,000 replicates)",
     col.main = "#0B0B0B", font.main = 2, cex.main = 1.2)
add.scale.bar(length = 0.005, cex = 0.9)
nodelabels(tree2$node.label, cex = 0.7, frame = "none", col = "#c0392b")
legend("bottomleft",
       legend = c("M. sativa tetraploid", "M. sativa diploid", "M. ruthenica",
                  "M. truncatula", "M. arabica", "M. lupulina", "M. polymorpha"),
       col    = c("#1a4d38", "#2b7a5e", "#d48c1a", "#f5b342",
                  "#8b5e9b", "#5a9b7e", "#b36b0e"),
       pch = 19, bty = "n", cex = 1.0)
dev.off()

png("figures/Fig_phylogeny_bootstrap.png", width = 2400, height = 1700, res = 200)
par(mar = c(2, 2, 3, 2), family = "sans")
plot(tree2,
     cex = 1.2,
     tip.color = tip_cols,
     label.offset = 0.001,
     main = "Medicago phylogeny with bootstrap support",
     col.main = "#0B0B0B", font.main = 2, cex.main = 1.2)
add.scale.bar(length = 0.005, cex = 0.9)
nodelabels(tree2$node.label, cex = 0.7, frame = "none", col = "#c0392b")
legend("bottomleft",
       legend = c("M. sativa tetraploid", "M. sativa diploid", "M. ruthenica",
                  "M. truncatula", "M. arabica", "M. lupulina", "M. polymorpha"),
       col    = c("#1a4d38", "#2b7a5e", "#d48c1a", "#f5b342",
                  "#8b5e9b", "#5a9b7e", "#b36b0e"),
       pch = 19, bty = "n", cex = 1.0)
dev.off()

cat("Figures saved in figures/\n")