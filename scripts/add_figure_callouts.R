# ============================================================
# add_figure_callouts.R
# Insert (Figure N) callouts into AlfaPan17.docx at the ends of
# specific target sentences.
# Output: AlfaPan18_with_callouts.docx  (original untouched)
# ============================================================

suppressPackageStartupMessages({
  library(officer)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/manuscript")

# ---- Read source ----
doc <- read_docx("AlfaPan17.docx")
txt <- docx_summary(doc)

cat("Total paragraphs:", nrow(txt), "\n\n")

# ---- Target sentences and the callout to append ----
# Only match on the FIRST occurrence (the Results-section mention).
# If a target appears at multiple paragraphs, take the smallest index.
targets <- data.frame(
  id       = 1:7,
  pattern  = c(
    "beyond the simple duplication expected from tetraploidy",
    "balance biological priority against experimental tractability",
    "immediately actionable for genome editing experiments",
    "copy number variation even for conserved genes",
    "confirmation of this pattern in Medicago",
    "prerequisite for observing expression asymmetry",
    "enhance genomic stability and environmental sensing"
  ),
  callout  = c(
    " (Figure 4).",
    " (Figure 5).",
    " (Figure 6).",
    " (Figure S1).",
    " (Figure S2).",
    " (Figure S3).",
    " (Figure S4)."
  ),
  stringsAsFactors = FALSE
)

# ---- Build a modified copy of the docx ----
# Approach: iterate the paragraphs, and when a target is matched,
# append the callout to the paragraph text.

# officer doesn't easily do in-place text edits; instead, we rebuild
# by iterating blocks and using body_replace_all_text on the whole doc.

for (k in 1:nrow(targets)) {
  pat <- targets$pattern[k]
  cal <- targets$callout[k]

  # Check what would match
  hits <- grep(pat, txt$text, fixed = TRUE)
  if (length(hits) == 0) {
    cat(sprintf("[SKIP] Target %d not found: '%s'\n", k, pat))
    next
  }

  first_hit <- min(hits)
  original  <- txt$text[first_hit]

  # Build the replacement: append callout after the pattern,
  # before the final period if there is one.
  if (grepl("\\.\\s*$", original)) {
    # Original ends with a period — insert callout before it
    replacement <- sub("\\.\\s*$", cal, original)
  } else {
    replacement <- paste0(original, cal)
  }

  # Apply the change to the doc
  doc <- body_replace_all_text(
    doc,
    old_value = original,
    new_value = replacement,
    only_at_cursor = FALSE
  )

  cat(sprintf("[OK]   Target %d -> appended '%s' at para %d\n", k, cal, first_hit))
}

# ---- Save to new file ----
print(doc, target = "AlfaPan18_with_callouts.docx")
cat("\n========================================\n")
cat("New file written: AlfaPan18_with_callouts.docx\n")
cat("Original AlfaPan17.docx is unchanged.\n")
cat("========================================\n")