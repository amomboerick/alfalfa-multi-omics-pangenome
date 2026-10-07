# scripts/fix_fonts.R
# Insert font + table-width CSS before line 340 in app.R.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Sanity: confirm line 340 closes the style block
if (!grepl('^\\s*"\\)\\)\\)', x[340])) {
  stop("Line 340 is not the style close -- abort")
}

css_add <- c(
'    /* ==== Global font ==== */',
'    body, .content-wrapper {',
'      font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif !important;',
'      font-size: 15px !important;',
'      line-height: 1.55 !important;',
'    }',
'    h1, h2, h3, h4, h5 {',
'      font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif !important;',
'    }',
'    h3 { font-size: 1.55rem !important; }',
'    h4 { font-size: 1.20rem !important; }',
'    /* ==== White plot backgrounds ==== */',
'    .shiny-plot-output, img.shiny-plot-output {',
'      background: #ffffff !important;',
'    }',
'    /* ==== Tables fill tab width ==== */',
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
'    /* ==== Container fills window ==== */',
'    .container-fluid {',
'      max-width: 100% !important;',
'      width: 100% !important;',
'      padding-left: 24px !important;',
'      padding-right: 24px !important;',
'    }',
''
)

new <- c(x[1:339], css_add, x[340:length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Inserted", length(css_add), "CSS lines before line 340\n")