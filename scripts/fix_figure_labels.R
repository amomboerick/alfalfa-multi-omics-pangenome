# ============================================================
# fix_figure_labels.R
# Fix four figure label issues in AlfaPan17.docx
# Reads AlfaPan17.docx, writes AlfaPan18_fixed.docx
# ============================================================

suppressPackageStartupMessages({
  library(officer)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/manuscript")

doc <- read_docx("AlfaPan17.docx")

# ---- The four fixes ----
fixes <- list(
  list(
    old = "Chromosomal distribution of gene classes in the Medicago pan-genome.",
    new = "Figure S2. Chromosomal distribution of gene classes in the Medicago pan-genome."
  ),
  list(
    old = "Figure S4. Homeolog retention candidates in tetraploid M. sativa.",
    new = "Figure S3. Homeolog retention candidates in tetraploid M. sativa."
  ),
  list(
    old = "Annotation coverage across the four populated pan-gene cluster types (core, soft_core, dispensable, private), showing",
    new = "Figure 3. Annotation coverage across the four populated pan-gene cluster types (core, soft_core, dispensable, private), showing"
  ),
  list(
    old = "For each of the four populated cluster types (core, soft_core, dispensable, private), bars show the percentage of member genes with at least one annotation in each of three functional layers: Gene Ontology (GO; blue), KEGG Orthology (KO; orange), and euKaryotic Orthologous Groups (KOG; green).",
    new = "Figure 3. For each of the four populated cluster types (core, soft_core, dispensable, private), bars show the percentage of member genes with at least one annotation in each of three functional layers: Gene Ontology (GO; blue), KEGG Orthology (KO; orange), and euKaryotic Orthologous Groups (KOG; green)."
  )
)

# ---- Apply each fix ----
for (k in 1:length(fixes)) {
  old_txt <- fixes[[k]]$old
  new_txt <- fixes[[k]]$new
  # Check whether the old text exists
  txt <- docx_summary(doc)
  found <- any(grepl(old_txt, txt$text, fixed = TRUE))
  if (found) {
    doc <- body_replace_all_text(doc,
                                 old_value = old_txt,
                                 new_value = new_txt,
                                 only_at_cursor = FALSE)
    cat(sprintf("[OK] Fix %d applied\n", k))
  } else {
    cat(sprintf("[SKIP] Fix %d not applied - target not found:\n       %s\n", k, old_txt))
  }
}

# ---- Save to new file ----
print(doc, target = "AlfaPan18_fixed.docx")
cat("\n========================================\n")
cat("New file written: AlfaPan18_fixed.docx\n")
cat("Original AlfaPan17.docx is unchanged.\n")
cat("========================================\n")