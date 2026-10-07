# scripts/fix_visuals.R
# Fix tables, axis labels, heatmap colors, and fonts.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# ============================================================
# 1. R-side: enlarge plot fonts (axes, titles, legends)
# ============================================================
x <- sub('FONT_COLOR <- "#0B0B0B"',
         'FONT_COLOR <- "#0B0B0B"', x)   # no-op guard

x <- sub('AXIS_CEX   <- 0.95',
         'AXIS_CEX   <- 1.25', x, fixed = TRUE)

x <- sub('TITLE_CEX  <- 1.15',
         'TITLE_CEX  <- 1.45', x, fixed = TRUE)

x <- sub('LABEL_CEX  <- 1.05',
         'LABEL_CEX  <- 1.30', x, fixed = TRUE)

# ============================================================
# 2. R-side: white background + colored cells for the two image() heatmaps
# ============================================================
# Cross-Accession Cluster Sharing
x <- sub('col = c("#e6eae8", "#1a4d38"),',
         'col = c("#ffffff", "#d62728"),',
         x, fixed = TRUE)

# Presence/Absence Heatmap
x <- sub('col = c("#e6eae8", "#1a4d38"),',
         'col = c("#ffffff", "#1a4d38"),',
         x, fixed = TRUE)

# ============================================================
# 3. CSS: white plot backgrounds, centered containers, tab strip wrap
# ============================================================
# Find the </style> close -- we insert before the LAST line of the CSS block
# The CSS block ends at a line containing only `))),`
close_idx <- grep('^\\s*\\)\\)\\),', x)
if (length(close_idx) == 0) stop("Could not find close of style block")
insert_at <- close_idx[1] - 1

css_add <- c(
'    /* ==== Tab content width + centering ==== */',
'    .tab-content > .tab-pane {',
'      padding-left: 12px !important;',
'      padding-right: 12px !important;',
'    }',
'    .tab-content .container-fluid {',
'      max-width: 100% !important;',
'      width: 100% !important;',
'      margin: 0 !important;',
'      padding-left: 16px !important;',
'      padding-right: 16px !important;',
'    }',
'    /* Tables span full width */',
'    .dataTables_wrapper,',
'    .dataTables_wrapper .dataTables_scroll,',
'    .dataTables_wrapper .dataTables_scrollHead,',
'    .dataTables_wrapper .dataTables_scrollBody {',
'      width: 100% !important;',
'    }',
'    /* Tab strip wraps normally instead of horizontal scroll */',
'    .tabbable > .nav-tabs {',
'      display: flex !important;',
'      flex-wrap: wrap !important;',
'      overflow: visible !important;',
'      border-bottom: 3px solid #1a4d38;',
'    }',
'    .tabbable > .nav-tabs > li {',
'      float: none !important;',
'      margin-right: 4px;',
'    }',
'    .tabbable > .nav-tabs > li > a {',
'      font-size: 15px !important;',
'      padding: 8px 14px !important;',
'      color: #0c1f17 !important;',
'      font-weight: 600;',
'    }',
'    .tabbable > .nav-tabs > li.active > a {',
'      background: #1a4d38 !important;',
'      color: #ffffff !important;',
'      border-radius: 6px 6px 0 0;',
'    }',
'    /* White plot backgrounds */',
'    .shiny-plot-output {',
'      background: #ffffff !important;',
'    }',
'    img.shiny-plot-output {',
'      background: #ffffff !important;',
'    }',
'    /* Global body font */',
'    body, .content-wrapper, .main-header {',
'      font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif !important;',
'      font-size: 15px !important;',
'      line-height: 1.55 !important;',
'    }',
'    h1, h2, h3, h4, h5 {',
'      font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif !important;',
'      letter-spacing: 0.2px;',
'    }',
'    h3 { font-size: 1.55rem !important; }',
'    h4 { font-size: 1.2rem !important; }',
''
)

new <- c(x[seq_len(insert_at)],
         css_add,
         x[(insert_at + 1):length(x)])

writeLines(new, p, useBytes = TRUE)
cat("DONE. Applied all visual fixes.\n")