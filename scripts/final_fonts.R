# scripts/final_fonts.R
# Final font sizing -- set clear sizes, remove conflicting !important overrides.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# --- 1. Remove the tiny !important table font rules entirely ---
# These are lines 266-267: the small overrides
x <- x[!grepl("table\\.dataTable tbody td \\{ color: #0c1f17", x)]
x <- x[!grepl("table\\.dataTable thead th \\{ color: #ffffff", x)]

# --- 2. Body: 18px, comfortable ---
x <- gsub("font-size: 18px !important;", "font-size: 17px !important;", x, fixed = TRUE)

# --- 3. Stat boxes: bigger is fine, they look good ---
x <- gsub("font-size: 2.4rem !important;", "font-size: 2.4rem !important;", x, fixed = TRUE)

# --- 4. Remove my old smaller override if present ---
x <- gsub("font-size: 1.15rem !important;", "font-size: 1.05rem !important;", x, fixed = TRUE)

# --- 5. Append a definitive, comfortable font block just before </style> ---
close_idx <- which(grepl('^\\s*"\\)\\)\\),\\s*$', x) | grepl('^\\s*\\)\\)\\),\\s*$', x))
if (length(close_idx) == 0) stop("Could not find close of style block")
close_idx <- close_idx[1]

override <- c(
'',
'    /* ==== DEFINITIVE READABLE FONT SIZES ==== */',
'    body, .content-wrapper {',
'      font-size: 17px !important;',
'      line-height: 1.6 !important;',
'    }',
'    table.dataTable tbody td,',
'    table.dataTable tbody th {',
'      font-size: 16px !important;',
'      padding: 10px 12px !important;',
'      font-weight: 500 !important;',
'      color: #0c1f17 !important;',
'    }',
'    table.dataTable thead th {',
'      font-size: 16px !important;',
'      padding: 12px !important;',
'      font-weight: 700 !important;',
'      color: #ffffff !important;',
'      background-color: #1a4d38 !important;',
'    }',
'    .stat-box h2 { font-size: 2.4rem !important; }',
'    .stat-box p { font-size: 1.1rem !important; font-weight: 600 !important; }',
'    h3 { font-size: 1.7rem !important; }',
'    h4 { font-size: 1.3rem !important; }',
'    p, li, label { font-size: 1.05rem !important; }',
'    .quick-card h4 { font-size: 1.35rem !important; }',
'    .quick-card p { font-size: 1rem !important; }',
'    .nav-tabs > li > a { font-size: 16px !important; padding: 12px 18px !important; }',
'    .btn { font-size: 16px !important; padding: 8px 20px !important; }',
'    input, select, textarea, .form-control { font-size: 16px !important; padding: 8px 12px !important; }',
'    .selectize-input { font-size: 16px !important; padding: 8px 12px !important; }',
'    /* Heatmap & image plots: white background */',
'    .shiny-plot-output, img.shiny-plot-output {',
'      background: #ffffff !important;',
'    }',
''
)

new <- c(x[1:(close_idx - 1)], override, x[close_idx:length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Final font sizes applied.\n")