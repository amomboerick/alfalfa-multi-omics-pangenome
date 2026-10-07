# scripts/fix_panel_d.R
# Panel D: left labels shifted right, bottom labels shifted up, no clipping.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# The Panel D block we want to replace.
old_D <- c(
'    par(mar = c(6, 17, 4, 2))',
'    plot(NA, xlim = c(0, ncol(mat)), ylim = c(0, nrow(mat)),',
'         xaxt = "n", yaxt = "n", xlab = "", ylab = "",',
'         main = "D. Presence/absence of top 20 core clusters",',
'         bty = "n", col.main = FONT_COLOR, font.main = FONT_BOLD, cex.main = 1.5)',
'    for (i in 1:nrow(mat)) for (j in 1:ncol(mat)) {',
'      rect(j-1, i-1, j, i, col = if (mat[i,j] == 1) "#1a4d38" else "#ffffff",',
'           border = "#cccccc", lwd = 0.5)',
'    }',
'    axis(2, at = (1:nrow(mat)) - 0.5, labels = clu$cluster_name, las = 2,',
'         cex.axis = 0.85, tick = FALSE, col.axis = FONT_COLOR, font.axis = FONT_BOLD)',
'    axis(1, at = (1:ncol(mat)) - 0.5, labels = acc$accession_name, las = 2,',
'         cex.axis = 1.0, tick = FALSE, col.axis = FONT_COLOR, font.axis = FONT_BOLD)'
)

new_D <- c(
'    par(mar = c(10, 12, 4, 2))',
'    plot(NA, xlim = c(0, ncol(mat)), ylim = c(0, nrow(mat)),',
'         xaxt = "n", yaxt = "n", xlab = "", ylab = "",',
'         main = "D. Presence/absence of top 20 core clusters",',
'         bty = "n", col.main = FONT_COLOR, font.main = FONT_BOLD, cex.main = 1.5)',
'    for (i in 1:nrow(mat)) for (j in 1:ncol(mat)) {',
'      rect(j-1, i-1, j, i, col = if (mat[i,j] == 1) "#1a4d38" else "#ffffff",',
'           border = "#cccccc", lwd = 0.5)',
'    }',
'    axis(2, at = (1:nrow(mat)) - 0.5, labels = clu$cluster_name, las = 2,',
'         cex.axis = 0.85, tick = FALSE, col.axis = FONT_COLOR, font.axis = FONT_BOLD,',
'         line = -0.5)',
'    axis(1, at = (1:ncol(mat)) - 0.5, labels = acc$accession_name, las = 2,',
'         cex.axis = 1.0, tick = FALSE, col.axis = FONT_COLOR, font.axis = FONT_BOLD,',
'         line = -0.5)'
)

# Locate and replace
d_start <- which(x == old_D[1])
if (length(d_start) == 0) stop("Could not find panel D -- paste lines around 1240-1270")
d_start <- d_start[1]
d_end <- d_start + length(old_D) - 1

if (!all(x[d_start:d_end] == old_D)) stop("Panel D block does not match exactly -- paste lines around 1240-1270")

new <- c(x[seq_len(d_start - 1)], new_D, x[(d_end + 1):length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Panel D adjusted.\n")