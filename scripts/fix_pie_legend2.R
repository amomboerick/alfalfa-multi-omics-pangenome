# scripts/fix_pie_legend2.R
# Move pie legend fully to the right side of panel A.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# --- 1. Add wider right margin just before the pie call (line 1153) ---
pie_idx <- grep('^\\s*pie\\(dfA\\$n, labels = NA, col = palA', x)
if (length(pie_idx) == 0) stop("Could not find pie call")
pie_idx <- pie_idx[1]

x <- c(x[1:(pie_idx - 1)],
       '    par(mar = c(4, 4, 4, 12))',
       x[pie_idx:length(x)])

# --- 2. Recompute legend index after insertion ---
legend_idx <- grep('^\\s*legend\\("right", legend = legend_labels_A', x)
if (length(legend_idx) == 0) stop("Could not find legend line")
legend_idx <- legend_idx[1]

# --- 3. Replace the legend call with one placed at a specific x/y inside the panel ---
x[legend_idx] <- '    legend(x = 1.55, y = 0.5, legend = legend_labels_A, fill = palA[seq_len(nrow(dfA))],'
legend_idx2 <- legend_idx + 1  # the continuation line
if (grepl("cex", x[legend_idx2])) {
  x[legend_idx2] <- '           cex = 1.1, bg = "white", bty = "n", xpd = NA)'
}

writeLines(x, p, useBytes = TRUE)
cat("DONE. Pie legend moved to the right side.\n")