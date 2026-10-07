# scripts/bump_fonts.R
# Bump UI fonts a bit and prevent chart axis labels from overlapping.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# --- 1. Body font 16 -> 18 ---
x <- gsub("font-size: 16px !important;",  "font-size: 18px !important;", x, fixed = TRUE)
x <- gsub("font-size: 17px !important;",  "font-size: 18px !important;", x, fixed = TRUE)

# --- 2. Table cells 1.15rem -> 1.05rem (keeps table rows from getting too tall) ---
x <- gsub("font-size: 1.15rem !important;", "font-size: 1.05rem !important;", x, fixed = TRUE)

# --- 3. Chart axis cex: bump sizes but keep them from overlapping ---
x <- gsub("AXIS_CEX   <- [0-9.]+",   "AXIS_CEX   <- 1.35", x)
x <- gsub("TITLE_CEX  <- [0-9.]+",   "TITLE_CEX  <- 1.55", x)
x <- gsub("LABEL_CEX  <- [0-9.]+",   "LABEL_CEX  <- 1.35", x)

writeLines(x, p, useBytes = TRUE)
cat("DONE. Fonts bumped.\n")