# patch_header.R
# Replace leaf icon with photo, remove subtitle, add CSS

f <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(f)

# ---- 1. Replace the leaf icon with the photo ----
i_icon <- grep("logo-leaf", x)[1]
if (!is.na(i_icon)) {
  x[i_icon] <- '      span(class = "logo-photo", tags$img(src = "alfalfa_logo.jpg", alt = "Alfalfa")),'
  cat("Replaced icon at line", i_icon, "\n")
} else {
  cat("WARNING: logo-leaf not found\n")
}

# ---- 2. Remove the subtitle block ----
i_sub <- grep('class = "subtitle"', x, fixed = TRUE)[1]
if (!is.na(i_sub)) {
  # Blank out the subtitle div and its contents (usually 4 lines)
  # Find the closing ")," by counting from i_sub
  # Simpler: blank from the "div(class = subtitle" line until the matching "),"
  x[i_sub] <- ""
  x[i_sub + 1] <- ""
  x[i_sub + 2] <- ""
  x[i_sub + 3] <- ""
  cat("Removed subtitle starting at line", i_sub, "\n")
} else {
  cat("WARNING: subtitle div not found\n")
}

# ---- 3. Add CSS for .logo-photo and .masthead ----
i_css <- grep("^\\.license-btn", x)[1]
if (is.na(i_css)) i_css <- grep("^\\.masthead", x)[1]

new_css <- c(
  "",
  "    /* logo photo */",
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

x <- append(x, new_css, after = i_css + 2)
cat("Added CSS after line", i_css, "\n")

# ---- Write back ----
writeLines(x, f)
cat("DONE. app.R updated.\n")