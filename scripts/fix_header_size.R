# scripts/fix_header_size.R
# Bump the masthead title, subtitle, badges, and buttons.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# --- Insert a header-sized CSS block just before the closing of tags$style ---
close_idx <- which(grepl('^\\s*"\\)\\)\\),\\s*$', x) | grepl('^\\s*\\)\\)\\),\\s*$', x))
if (length(close_idx) == 0) stop("Could not find close of style block")
close_idx <- close_idx[1]

override <- c(
'',
'    /* ===== BIGGER MASTHEAD ===== */',
'    .masthead {',
'      padding: 2.4rem 2.6rem 1.8rem 2.6rem !important;',
'    }',
'    .masthead h1 {',
'      font-size: 2.6rem !important;',
'      letter-spacing: -0.02em !important;',
'      font-weight: 800 !important;',
'    }',
'    .masthead h1 .logo-leaf {',
'      font-size: 2.2rem !important;',
'    }',
'    .masthead .subtitle {',
'      font-size: 1.15rem !important;',
'      margin-top: 10px !important;',
'    }',
'    .masthead .meta-row {',
'      gap: 12px !important;',
'      margin-top: 20px !important;',
'    }',
'    .meta-badge {',
'      font-size: 0.95rem !important;',
'      padding: 6px 18px !important;',
'      font-weight: 700 !important;',
'    }',
'    .meta-badge .val {',
'      font-size: 1rem !important;',
'      font-weight: 800 !important;',
'    }',
'    .cite-btn, .license-btn {',
'      font-size: 1rem !important;',
'      padding: 10px 24px !important;',
'      font-weight: 800 !important;',
'    }',
'    .license-btn {',
'      margin-left: 10px !important;',
'    }',
''
)

new <- c(x[1:(close_idx - 1)], override, x[close_idx:length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Header enlarged.\n")