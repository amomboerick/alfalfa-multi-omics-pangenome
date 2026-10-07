# scripts/fix_fig1_fonts.R
# Bigger fonts in Figure 1, panel A gets a legend instead of slice labels.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# --- Find and replace the hero_figure block ---
start <- grep("output\\$hero_figure <- renderPlot", x)
if (length(start) == 0) stop("Could not find hero_figure")
start <- start[1]

depth <- 0
end <- NA
for (i in start:length(x)) {
  line <- x[i]
  depth <- depth + lengths(regmatches(line, gregexpr("\\{", line)))
  depth <- depth - lengths(regmatches(line, gregexpr("\\}", line)))
  if (depth == 0 && i > start) { end <- i; break }
}
if (is.na(end)) stop("Could not find end of hero_figure block")

replacement <- c(
'  output$hero_figure <- renderPlot({',
'    con <- connect_db(); on.exit(dbDisconnect(con))',
'    dfA <- dbGetQuery(con, "SELECT cluster_type, COUNT(*)::text AS n FROM pan_gene_clusters GROUP BY cluster_type ORDER BY COUNT(*) DESC")',
'    dfA$n <- as.numeric(dfA$n)',
'    dfB <- dbGetQuery(con, "SELECT accession_name, species, gene_count::text AS n FROM accessions ORDER BY gene_count DESC")',
'    dfB$n <- as.numeric(dfB$n)',
'    dfC <- dbGetQuery(con, "SELECT c.contig_name, a.accession_name, COUNT(*)::text AS n FROM genes g JOIN contigs c ON g.contig_id=c.contig_id JOIN accessions a ON g.accession_id=a.accession_id GROUP BY c.contig_name, a.accession_name")',
'    dfC$n <- as.numeric(dfC$n)',
'    clu <- dbGetQuery(con, "SELECT cluster_id, cluster_name FROM pan_gene_clusters WHERE cluster_type = \'core\' ORDER BY gene_count DESC LIMIT 20")',
'    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")',
'    pa  <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present = TRUE")',
'    mat <- matrix(0, nrow = nrow(clu), ncol = nrow(acc))',
'    for (i in seq_len(nrow(pa))) {',
'      r <- which(clu$cluster_id == pa$cluster_id[i]); cc <- which(acc$accession_id == pa$accession_id[i])',
'      if (length(r) == 1 && length(cc) == 1) mat[r, cc] <- 1',
'    }',
'',
'    par(mfrow = c(2, 2), mar = c(6, 4, 4, 2), oma = c(2, 2, 4, 2),',
'        col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,',
'        fg = FONT_COLOR, cex.axis = 1.35, font.axis = FONT_BOLD,',
'        font.lab = FONT_BOLD, font.main = FONT_BOLD, cex.main = 1.5)',
'',
'    # ---- A. Pie with legend (no slice labels) ----',
'    palA <- c("#1a4d38", "#f5b342", "#d48c1a", "#8b5e9b", "#2b7a5e")',
'    legend_labels_A <- paste0(dfA$cluster_type, " (", format(dfA$n, big.mark = ","), ")")',
'    pie(dfA$n, labels = NA, col = palA[seq_len(nrow(dfA))], border = "white",',
'        radius = 0.9, main = "A. Cluster type distribution",',
'        col.main = FONT_COLOR, font.main = FONT_BOLD, cex.main = 1.5)',
'    legend("right", legend = legend_labels_A, fill = palA[seq_len(nrow(dfA))],',
'           cex = 1.1, bg = "white", bty = "n", inset = c(-0.05, 0))',
'',
'    # ---- B. Horizontal barplot: genes per accession ----',
'    species_colors <- c("Medicago sativa" = "#1a4d38", "Medicago truncatula" = "#f5b342",',
'                        "Medicago ruthenica" = "#d48c1a", "Medicago arabica" = "#8b5e9b",',
'                        "Medicago polymorpha" = "#2b7a5e", "Medicago lupulina" = "#5a9b7e")',
'    bc <- species_colors[dfB$species]; bc[is.na(bc)] <- "#8fa7b3"',
'    par(mar = c(5, 15, 4, 2))',
'    barplot(dfB$n, names.arg = dfB$accession_name, las = 1, horiz = TRUE, col = bc,',
'            border = "white", main = "B. Genes per accession", xlab = "Gene count",',
'            cex.names = 1.15, font = 2,',
'            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,',
'            font.axis = FONT_BOLD, font.lab = FONT_BOLD,',
'            font.main = FONT_BOLD, cex.axis = 1.35, cex.lab = 1.4, cex.main = 1.5)',
'',
'    # ---- C. Stacked barplot: colored by accession with big legend ----',
'    if (nrow(dfC) > 0) {',
'      tab <- xtabs(n ~ contig_name + accession_name, data = dfC)',
'      par(mar = c(5, 5, 4, 3))',
'      barplot(t(tab), col = rainbow(ncol(tab)), border = NA,',
'              main = "C. Genes per chromosome", xlab = "", ylab = "Gene count",',
'              las = 1, xaxt = "n",',
'              col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,',
'              font.axis = FONT_BOLD, font.lab = FONT_BOLD,',
'              font.main = FONT_BOLD, cex.axis = 1.35, cex.lab = 1.4, cex.main = 1.5)',
'      legend("topright", legend = colnames(tab), fill = rainbow(ncol(tab)),',
'             cex = 0.95, bg = "white", bty = "n", inset = c(-0.02, 0))',
'    }',
'',
'    # ---- D. Presence/absence grid, larger labels ----',
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
'         cex.axis = 1.0, tick = FALSE, col.axis = FONT_COLOR, font.axis = FONT_BOLD)',
'',
'    mtext("Figure 1 - Alfalfa Multi-Omics Pan-Genome Overview",',
'          outer = TRUE, cex = 1.7, font = 2, col = FONT_COLOR, line = 1)',
'  }, height = 900)'
)

new <- c(x[seq_len(start - 1)], replacement, x[(end + 1):length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Figure 1 fonts enlarged.\n")