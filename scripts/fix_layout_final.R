# scripts/fix_layout_final.R
# Remove the broken CSS tail and append one clean override block.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# 1. Remove unfinished "Global font" comment and any broken font-family lines
keep <- !grepl("==== Global font ====", x, fixed = TRUE)
keep <- keep & !grepl('font-family: "Segoe', x, fixed = TRUE)
keep <- keep & !grepl('font-family: "Helvetica', x, fixed = TRUE)
x <- x[keep]

# 2. Find the line that closes tags$style() -- the line with only  ))),
close_idx <- which(grepl('^\\s*"\\)\\)\\),\\s*$', x) | grepl('^\\s*\\)\\)\\),\\s*$', x))
if (length(close_idx) == 0) stop("Could not find the closing of tags$style")
close_idx <- close_idx[1]

# 3. Build the override CSS (all fonts quoted with single quotes -- safe in R)
override <- c(
'',
'    /* ================================================',
'       FINAL LAYOUT OVERRIDE -- full width, readable fonts',
'       ================================================ */',
'',
'    /* Header uses the full window */',
'    .masthead-inner {',
'      max-width: 100% !important;',
'      margin: 0 !important;',
'      padding-left: 32px !important;',
'      padding-right: 32px !important;',
'    }',
'',
'    /* Tab content uses the full window */',
'    .container-fluid {',
'      max-width: 100% !important;',
'      width: 100% !important;',
'      padding-left: 32px !important;',
'      padding-right: 32px !important;',
'    }',
'',
'    /* Tables fill the whole tab, not a narrow column */',
'    .dataTables_wrapper {',
'      width: 100% !important;',
'    }',
'    .dataTables_wrapper .dataTables_scroll,',
'    .dataTables_wrapper .dataTables_scrollHead,',
'    .dataTables_wrapper .dataTables_scrollBody,',
'    table.dataTable {',
'      width: 100% !important;',
'    }',
'',
'    /* Readable fonts everywhere -- single-quoted names are safe in R */',
'    body, .content-wrapper, h1, h2, h3, h4, h5, p, li, td, th, a, label {',
'      font-family: Segoe UI, Helvetica Neue, Arial, sans-serif !important;',
'    }',
'    body {',
'      font-size: 17px !important;',
'      line-height: 1.6 !important;',
'    }',
'    h3 { font-size: 1.65rem !important; }',
'    h4 { font-size: 1.25rem !important; }',
'    p, li, td, th, label { font-size: 1rem !important; }',
'',
'    /* Bigger stat boxes */',
'    .stat-box h2 {',
'      font-size: 2.4rem !important;',
'      margin: 0 0 6px 0 !important;',
'    }',
'    .stat-box p { font-size: 1.05rem !important; }',
'',
'    /* Charts: white background so axis labels are always visible */',
'    .shiny-plot-output, img.shiny-plot-output {',
'      background: #ffffff !important;',
'    }',
''
)

# 4. Insert override just before the closing of tags$style
new <- c(x[1:(close_idx - 1)], override, x[close_idx:length(x)])

writeLines(new, p, useBytes = TRUE)
cat("DONE. Override inserted before line", close_idx, "\n")