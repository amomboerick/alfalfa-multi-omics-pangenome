# patch_header_v2.R
# Fix: replace HTML icon with photo, remove remaining subtitle CSS

f <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(f)

# ---- 1. Replace the HTML icon (find the one inside div, not in CSS) ----
# The actual HTML line uses: span(class = "logo-leaf", shiny::icon("leaf"))
i_html <- grep('span\\(class = "logo-leaf"', x)[1]
if (!is.na(i_html)) {
  x[i_html] <- '      span(class = "logo-photo", tags$img(src = "alfalfa_logo.jpg", alt = "Alfalfa")),'
  cat("Replaced HTML icon at line", i_html, "\n")
} else {
  cat("WARNING: HTML icon line not found\n")
}

# ---- 2. Replace the CSS rule for .masthead h1 .logo-leaf (line ~197) ----
# So the styles flow correctly to .logo-photo
i_css1 <- grep('^\\.masthead h1 \\.logo-leaf', x)[1]
if (!is.na(i_css1)) {
  x[i_css1] <- '    .masthead h1 .logo-photo { display: inline-block; vertical-align: middle; margin-right: 14px; }'
  cat("Updated CSS rule at line", i_css1, "\n")
}

# Also handle the second one at line ~485
i_css2 <- grep('^\\.masthead h1 \\.logo-leaf', x)
if (length(i_css2) > 1) {
  x[i_css2[2]] <- '    .masthead h1 .logo-photo { display: inline-block; vertical-align: middle; margin-right: 14px; }'
  cat("Updated CSS rule at line", i_css2[2], "\n")
}

# ---- 3. Remove the subtitle CSS rules (no longer needed) ----
i_subcss <- grep('^\\.masthead \\.subtitle', x)
for (j in i_subcss) {
  x[j] <- ''
  cat("Removed subtitle CSS at line", j, "\n")
}

# ---- 4. Add the .logo-photo sizing CSS after the main .masthead rule ----
# Find line with just ".masthead {" (the opening of the main block)
i_mast <- grep('^\\.masthead \\{', x)[1]
new_css <- c(
  "    .masthead h1 .logo-photo img {",
  "      width: 60px;",
  "      height: 60px;",
  "      border-radius: 50%;",
  "      object-fit: cover;",
  "      border: 3px solid #FFFFFF;",
  "      box-shadow: 0 2px 8px rgba(0,0,0,0.18);",
  "    }",
  "    .masthead {",
  "      background: linear-gradient(135deg, #FDF6EC 0%, #EDE3CE 100%) !important;",
  "    }"
)
if (!is.na(i_mast)) {
  x <- append(x, new_css, after = i_mast + 2)
  cat("Added logo-photo CSS after line", i_mast, "\n")
}

# ---- Write back ----
writeLines(x, f)
cat("DONE. app.R updated.\n")