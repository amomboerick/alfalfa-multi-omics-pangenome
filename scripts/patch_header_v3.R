# patch_header_v3.R
# Fix app.R header: photo icon, no subtitle, styling

f <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(f)
n_orig <- length(x)
cat("Original lines:", n_orig, "\n")

# ---- 1. Locate the main style block (tags$style(HTML(" ... ")) ----
i_style_start <- grep('tags\\$style\\(HTML\\(', x)[1]
cat("Style block starts at line:", i_style_start, "\n")

# Find the matching closing line — the "))" that closes tags$style(HTML(
# Search forward from i_style_start for a line that is exactly `"))` or `"))` with whitespace
i_style_end <- NA
for (j in (i_style_start + 1):min(n_orig, i_style_start + 500)) {
  if (grepl('^\\s*"\\)\\)\\s*[,)]?\\s*$', x[j])) {
    i_style_end <- j
    break
  }
}
cat("Style block ends at line:", i_style_end, "\n")

# ---- 2. New CSS to insert before the close ----
new_css <- c(
  "",
  "    /* === PHOTO LOGO === */",
  "    .logo-photo {",
  "      display: inline-block;",
  "      width: 60px;",
  "      height: 60px;",
  "      border-radius: 50%;",
  "      overflow: hidden;",
  "      vertical-align: middle;",
  "      margin-right: 14px;",
  "      border: 3px solid #FFFFFF;",
  "      box-shadow: 0 2px 8px rgba(0,0,0,0.18);",
  "    }",
  "    .logo-photo img {",
  "      width: 100%;",
  "      height: 100%;",
  "      object-fit: cover;",
  "      display: block;",
  "    }",
  "    .masthead {",
  "      background: linear-gradient(135deg, #FDF6EC 0%, #EDE3CE 100%) !important;",
  "    }"
)

if (!is.na(i_style_end)) {
  # Insert new_css before i_style_end (but preserve any "," on that closing line)
  x <- append(x, new_css, after = i_style_end - 1)
  cat("Inserted CSS rules before line", i_style_end, "\n")
} else {
  cat("ERROR: Could not find style block end\n")
}

# ---- 3. Replace the HTML icon in masthead h1 ----
i_icon_html <- NA
for (j in 1:length(x)) {
  if (grepl('shiny::icon\\("leaf"\\)', x[j]) && grepl('logo-leaf', x[j])) {
    i_icon_html <- j
    break
  }
}
if (!is.na(i_icon_html)) {
  # Preserve indentation from the original line
  indent <- sub("\\S.*", "", x[i_icon_html])
  x[i_icon_html] <- paste0(indent, 'span(class = "logo-photo", tags$img(src = "alfalfa_logo.jpg", alt = "Alfalfa")),')
  cat("Replaced HTML icon at line", i_icon_html, "\n")
} else {
  cat("WARNING: HTML icon line with logo-leaf + shiny::icon leaf not found\n")
}

# ---- 4. Remove the subtitle div block ----
i_sub <- NA
for (j in 1:length(x)) {
  if (grepl('div\\(class = "subtitle"', x[j])) {
    i_sub <- j
    break
  }
}
if (!is.na(i_sub)) {
  # Blank the div opening + the next 3 lines (typical structure)
  x[i_sub]     <- ""
  x[i_sub + 1] <- ""
  x[i_sub + 2] <- ""
  x[i_sub + 3] <- ""
  cat("Removed subtitle div starting at line", i_sub, "\n")
} else {
  cat("WARNING: subtitle div not found\n")
}

# ---- 5. Write back ----
writeLines(x, f)
cat("\nDONE. New file lines:", length(x), "\n")