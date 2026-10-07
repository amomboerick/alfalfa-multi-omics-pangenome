# scripts/fix_css.R
# Fix the 100%% typo and add font/heatmap/axis fixes.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# --- 1. Fix the 100%% typo on line 275 (and any other 100%% occurrences) ---
x <- gsub("100%%", "100%", x, fixed = TRUE)

# --- 2. Enlarge R plot font sizes ---
x <- sub("AXIS_CEX   <- 0.95", "AXIS_CEX   <- 1.30", x, fixed = TRUE)
x <- sub("TITLE_CEX  <- 1.15", "TITLE_CEX  <- 1.50", x, fixed = TRUE)
x <- sub("LABEL_CEX  <- 1.05", "LABEL_CEX  <- 1.30", x, fixed = TRUE)

# --- 3. Change heatmap cells to white bg + colored fill ---
x <- sub('col = c("#e6eae8", "#1a4d38")',
         'col = c("#ffffff", "#d62728")',
         x, fixed = TRUE)

# --- 4. Insert additional CSS for fonts and white plot backgrounds ---
# We insert just before line 371, right before `    )))` (which closes tags$style)
insert_at <- grep("^\\s*\\)\\)\\),\n?", x)
if (length(insert_at) == 0) {
  insert_at <- grep("^\\s*\\)\\)\\)", x)
}
if (length(insert_at) == 0) stop("Could not find close of style block")
insert_at <- insert_at[1]

css_add <- c(
'    /* ==== Fonts ==== */',
'    body, .content-wrapper {',
'      font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif !important;',
'      font-size: 15px !important;',
'      line-height: 1.55 !important;',
'    }',
'    h1, h2, h3, h4 { font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif !important; }',
'    h3 { font-size: 1.55rem !important; }',
'    h4 { font-size: 1.20rem !important; }',
'    /* ==== White plot backgrounds ==== */',
'    .shiny-plot-output, img.shiny-plot-output {',
'      background: #ffffff !important;',
'    }',
'    /* ==== Table fills tab width ==== */',
'    .dataTables_wrapper,',
'    .dataTables_wrapper .dataTables_scroll,',
'    .dataTables_wrapper .dataTables_scrollHead,',
'    .dataTables_wrapper .dataTables_scrollBody {',
'      width: 100% !important;',
'    }',
'    .tab-content .container-fluid {',
'      max-width: 100% !important;',
'      width: 100% !important;',
'    }',
''
)

new <- c(x[seq_len(insert_at - 1)],
         css_add,
         x[insert_at:length(x)])

writeLines(new, p, useBytes = TRUE)
cat("DONE. CSS fix applied. Inserted at line", insert_at, "\n")