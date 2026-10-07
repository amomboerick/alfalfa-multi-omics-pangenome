library(officer)
doc <- read_docx("C:/Users/Erick.Amombo/alfalfa_pangenome/manuscript/AlfaPan.docx")
txt <- docx_summary(doc)

# Usage: Rscript print_draft.R START END
args <- commandArgs(trailingOnly = TRUE)
start <- as.integer(args[1])
end   <- as.integer(args[2])

if (is.na(start)) start <- 1
if (is.na(end))   end   <- nrow(txt)
end <- min(end, nrow(txt))

for (i in start:end) {
  style <- txt$style_name[i]
  text  <- txt$text[i]
  if (is.na(style)) style <- "NA"
  if (is.na(text))  text  <- ""
  cat(sprintf("\n--- [%d | %s] ---\n%s\n", i, style, text))
}