# scripts/fix_simple.R
# Simple, targeted fixes with no CSS insertion.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

before <- x

# 1. Fix the 100%% typo anywhere it occurs
x <- gsub("100%%", "100%", x, fixed = TRUE)

# 2. Enlarge plot fonts
x <- gsub("AXIS_CEX   <- 0.95", "AXIS_CEX   <- 1.30", x, fixed = TRUE)
x <- gsub("TITLE_CEX  <- 1.15", "TITLE_CEX  <- 1.50", x, fixed = TRUE)
x <- gsub("LABEL_CEX  <- 1.05", "LABEL_CEX  <- 1.30", x, fixed = TRUE)

# 3. Heatmap colors: white background + red for the first image() call
x <- gsub('col = c("#e6eae8", "#1a4d38")',
          'col = c("#ffffff", "#d62728")',
          x, fixed = TRUE)

changed <- sum(x != before)
writeLines(x, p, useBytes = TRUE)
cat("DONE. Lines changed:", changed, "\n")