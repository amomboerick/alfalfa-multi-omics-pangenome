# scripts/fix_layout.R
# Fix tab wrapping, stat-box heights, and container width.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# ---- Locate the closing </style> tag inside tags$style(HTML(" ... ")) ----
close_idx <- grep("^\\s*\\)\\)\\),", x)
if (length(close_idx) == 0) {
  close_idx <- grep("\\\"\\)\\)\\),", x)
}
# The style block ends right before the ")))" that closes tags$head
# Simpler: find the line containing "</style>" or the last CSS line before )))
style_end <- grep("\\)\\)\\),", x)
if (length(style_end) == 0) stop("Could not find style block end")
style_end <- style_end[1]

# ---- CSS to append (before the closing tag) ----
extra_css <- c(
'    /* ==== Layout fixes ==== */',
'    .container-fluid {',
'      max-width: 100% !important;',
'      width: 100% !important;',
'      padding-left: 32px !important;',
'      padding-right: 32px !important;',
'    }',
'    .tabbable > .nav-tabs {',
'      display: flex;',
'      flex-wrap: nowrap !important;',
'      overflow-x: auto;',
'      overflow-y: hidden;',
'      white-space: nowrap;',
'      border-bottom: 3px solid #1a4d38;',
'    }',
'    .tabbable > .nav-tabs > li {',
'      float: none !important;',
'      flex: 0 0 auto;',
'    }',
'    .tabbable > .nav-tabs > li > a {',
'      font-size: 15px !important;',
'      padding: 10px 16px !important;',
'      border: none !important;',
'    }',
'    .tabbable > .nav-tabs > li.active > a {',
'      background: #1a4d38 !important;',
'      color: #ffffff !important;',
'      border-radius: 6px 6px 0 0;',
'    }',
'    .stat-box {',
'      min-height: 110px !important;',
'      padding: 16px 12px !important;',
'      display: flex;',
'      flex-direction: column;',
'      justify-content: center;',
'      align-items: center;',
'    }',
'    .stat-box h2 {',
'      font-size: 1.9rem !important;',
'      line-height: 1.1;',
'      margin-bottom: 4px;',
'    }',
'    .stat-box p {',
'      font-size: 0.95rem !important;',
'      margin: 0;',
'    }',
'    body {',
'      font-size: 16px !important;',
'      font-family: "Source Sans Pro", "Inter", -apple-system, BlinkMacSystemFont, sans-serif !important;',
'    }',
'    .masthead-inner {',
'      max-width: 100% !important;',
'      padding-left: 32px !important;',
'      padding-right: 32px !important;',
'    }',
''
)

new <- c(x[seq_len(style_end - 1)], extra_css, x[style_end:length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Layout CSS inserted before line", style_end, "\n")