# scripts/fix_fig1_ac.R
# Panel A: pie centered, legend right beside it.
# Panel C: legend outside the plot area to the right.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# ============================================================
# PANEL A
# ============================================================
# 1. Restore standard right margin (was 12, now 2)
a_margin <- grep("^\\s*par\\(mar = c\\(4, 4, 4, 12\\)\\)\\s*$", x)
if (length(a_margin) > 0) {
  x[a_margin[1]] <- '    par(mar = c(4, 4, 4, 2))'
}

# 2. Reduce pie radius and keep labels NA
pie_idx <- grep("^\\s*pie\\(dfA\\$n, labels = NA", x)
if (length(pie_idx) > 0) {
  x[pie_idx[1]] <- '    pie(dfA$n, labels = NA, col = palA[seq_len(nrow(dfA))], border = "white",'
  # the next line has radius; replace it
  if (grepl("radius", x[pie_idx[1] + 1])) {
    x[pie_idx[1] + 1] <- '        radius = 0.7, main = "A. Cluster type distribution",'
  }
}

# 3. Move legend from x=1.55 to x=1.35 (just past the pie radius)
legend_line <- grep("^\\s*legend\\(x = 1\\.55, y = 0\\.5, legend = legend_labels_A", x)
if (length(legend_line) > 0) {
  x[legend_line[1]] <- '    legend(x = 1.30, y = 0.5, legend = legend_labels_A, fill = palA[seq_len(nrow(dfA))],'
}

# ============================================================
# PANEL C
# ============================================================
# Push the C legend outside the plot box (to the right)
c_legend <- grep('^\\s*legend\\("topright", legend = colnames\\(tab\\), fill = rainbow\\(ncol\\(tab\\)\\),', x)
if (length(c_legend) > 0) {
  x[c_legend[1]] <- '      legend(x = ncol(tab) + 1.5, y = max(rowSums(tab)), legend = colnames(tab), fill = rainbow(ncol(tab)),'
  # next line has cex, bg, etc -- update inset to xpd
  if (grepl("cex", x[c_legend[1] + 1])) {
    x[c_legend[1] + 1] <- '             cex = 0.95, bg = "white", bty = "n", xpd = TRUE)'
  }
}

writeLines(x, p, useBytes = TRUE)
cat("DONE. Panel A and C adjustments applied.\n")