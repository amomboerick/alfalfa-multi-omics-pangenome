# scripts/fix_pie_legend.R
# Panel A: pie centered on the left of its panel, legend to the right.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find the pie call
pie_idx <- grep("pie\\(dfA\\$n", x)
if (length(pie_idx) == 0) stop("Could not find pie call")
pie_idx <- pie_idx[1]

# Find the legend line right after the pie
legend_idx <- grep("legend\\(\"topright\", legend = legend_labels_A", x)
if (length(legend_idx) == 0) stop("Could not find pie legend line")
legend_idx <- legend_idx[1]

# Replace pie call (add xlim so pie sits in left half of panel)
x[pie_idx] <- '    pie(dfA$n, labels = NA, col = palA[seq_len(nrow(dfA))], border = "white",'

# Insert a line BEFORE pie to set wider right margin
x <- c(x[1:(pie_idx - 1)],
       '    par(mar = c(4, 4, 4, 12))',
       x[pie_idx:length(x)])

# Recompute indices after insertion
pie_idx <- pie_idx + 1
legend_idx <- legend_idx + 1

# Replace the legend line -- place at (12, 0.5) which is right side of plot area
x[legend_idx] <- paste0(
  '    legend("center", x = 1.55, y = 0.5, ',
  'legend = legend_labels_A, fill = palA[seq_len(nrow(dfA))], ',
  'cex = 1.1, bg = "white", bty = "n", xpd = NA)'
)

writeLines(x, p, useBytes = TRUE)
cat("DONE. Pie legend moved to the right side.\n")