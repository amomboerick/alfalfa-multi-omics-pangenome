# ============================================================
# clean_manuscript.R
# Reads AlfaPan17.docx, applies corrections, outputs AlfaPan_v3.docx
# Fixes: LaTeX fragments, stripped underscores, typos, ordering
# ============================================================

suppressPackageStartupMessages({
  library(officer)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/manuscript")

# ---- Read paragraphs from current docx ----
doc_in <- read_docx("AlfaPan17.docx")
txt <- docx_summary(doc_in)

# ---- Cleanup function ----
clean_text <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)

  # 1. LaTeX math fragments -> plain text
  #    Pattern: \( ... \)  -> strip the \( and \)
  x <- gsub("\\\\\\(", "", x)
  x <- gsub("\\\\\\)", "", x)
  x <- gsub("\\\\%", "%", x)
  x <- gsub("\\\\times", "x", x)
  x <- gsub("\\\\geq", ">=", x)
  x <- gsub("\\\\leq", "<=", x)
  x <- gsub("\\\\mathrm\\{([^}]*)\\}", "\\1", x)
  x <- gsub("\\\\text\\{([^}]*)\\}", "\\1", x)
  x <- gsub("\\\\log_\\{10\\}", "log10", x)
  x <- gsub("\\\\%", "%", x)
  x <- gsub("\\$", "", x)
  # clean up leftover double spaces
  x <- gsub("  +", " ", x)
  x <- trimws(x)

  # 2. Specific typos
  x <- gsub("\\bAlfPan\\b", "AlfaPan", x)
  x <- gsub("\\bEmm and Kelly\\b", "Emms and Kelly", x)
  x <- gsub("\\bEmmes\\b", "Emms", x)
  x <- gsub("Kruskal-Walli's", "Kruskal-Wallis", x)
  x <- gsub("Kruskal Wallis", "Kruskal-Wallis", x)
  x <- gsub("homoeologs", "homeologs", x)
  x <- gsub("homoeolog", "homeolog", x)
  x <- gsub("softcore", "soft_core", x)
  x <- gsub("soft-core", "soft_core", x)
  x <- gsub("soft core", "soft_core", x)

  # 3. Database identifiers (specific phrases from schema)
  x <- gsub("\\bpangen clusters table\\b", "pan_gene_clusters table", x, ignore.case = TRUE)
  x <- gsub("\\bpan gene clusters\\b", "pan_gene_clusters", x, ignore.case = TRUE)
  x <- gsub("\\bcluster membership\\b", "cluster_membership", x, ignore.case = TRUE)
  x <- gsub("\\bgene presence/absence tables\\b", "gene_presence_absence tables", x, ignore.case = TRUE)
  x <- gsub("\\bsativa tetraploid\\b", "sativa_tetraploid", x, ignore.case = TRUE)
  x <- gsub("\\bsativa diploid\\b", "sativa_diploid", x, ignore.case = TRUE)
  x <- gsub("\\bcaerulea landa\\b", "caerulea_landa", x, ignore.case = TRUE)
  x <- gsub("\\bcaerulea long\\b", "caerulea_long", x, ignore.case = TRUE)
  x <- gsub("\\bZM4 hap4\\b", "ZM4_hap4", x, ignore.case = TRUE)
  x <- gsub("\\bZM4_hap4\\b", "ZM4_hap4", x)

  # 4. Range dash consistency (use en-dash)
  x <- gsub("([0-9])- ?([0-9])", "\\1\u2013\\2", x)   # 1-2 -> 1–2
  x <- gsub(" \u2013 ", "\u2013", x)

  return(x)
}

# ---- Apply cleanup to text ----
txt$text <- sapply(txt$text, clean_text)

# ---- Print a summary of what changed ----
n_latex_before <- sum(grepl("\\\\\\(", docx_summary(doc_in)$text))
cat("LaTeX fragments in original:", n_latex_before, "\n")
cat("Total paragraphs processed:", nrow(txt), "\n\n")

# ---- Write clean text to a temp CSV (for reference) ----
write.csv(data.frame(style=txt$style_name, text=txt$text),
          "manuscript_cleaned_preview.csv", row.names = FALSE)
cat("Preview saved: manuscript_cleaned_preview.csv\n")
cat("Review it, then run the rebuild step.\n")