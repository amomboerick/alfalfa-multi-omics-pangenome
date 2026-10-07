# scripts/fix_ai_escapes.R
# Fix the four gsub() lines inside ask_groq_for_sql so R can parse them.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find the four target lines
find1 <- grep('gsub\\("\\^```sql', x)
find2 <- grep('gsub\\("\\^```\\\\s\\*", "", txt\\)', x)
find3 <- grep('gsub\\("```\\\\s\\*\\$", "", txt\\)', x)

# Replace all three (there may be 1, 2, or 3 depending on the exact prior state)
fix_line <- function(line, pattern, replacement) {
  sub(pattern, replacement, line)
}

new <- x
for (i in seq_along(x)) {
  # Line 1: strip leading ```sql
  if (grepl('gsub\\("\\^```sql', x[i])) {
    new[i] <- '  txt <- gsub("^```sql\\\\s*", "", txt, ignore.case = TRUE)'
  }
  # Line 2: strip leading ```
  else if (grepl('gsub\\("\\^```', x[i]) && grepl('txt', x[i])) {
    new[i] <- '  txt <- gsub("^```\\\\s*", "", txt)'
  }
  # Line 3: strip trailing ```
  else if (grepl('gsub\\("```', x[i]) && grepl('txt', x[i])) {
    new[i] <- '  txt <- gsub("```\\\\s*$", "", txt)'
  }
}

writeLines(new, p, useBytes = TRUE)
cat("Fixed", sum(new != x), "line(s)\n")