# scripts/debug_entry.R
# Add a print right after the SQL is stored, so we see what the entry contains.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find where result is stored
idx <- grep("ai_state\\$history\\[\\[idx\\]\\]\\$result <- out", x)
if (length(idx) == 0) stop("Could not find result assignment")
idx <- idx[1]

# Insert debug print AFTER the if/else block that sets result/error
# Find the closing brace of that if/else, which is a few lines below
close_idx <- idx
for (i in (idx + 1):(idx + 10)) {
  if (grepl("^\\s*\\}\\s*$", x[i])) { close_idx <- i; break }
}

# Insert debug right after the closing brace
x <- c(x[1:close_idx],
       '    # DEBUG: show what we stored',
       '    cat("[DEBUG] entry sql:", if (is.null(ai_state$history[[idx]]$sql)) "NULL" else substr(ai_state$history[[idx]]$sql, 1, 80), "\\n")',
       '    cat("[DEBUG] entry result:", if (is.null(ai_state$history[[idx]]$result)) "NULL" else paste(nrow(ai_state$history[[idx]]$result), "rows"), "\\n")',
       '    cat("[DEBUG] entry error:", if (is.null(ai_state$history[[idx]]$error)) "NULL" else ai_state$history[[idx]]$error, "\\n")',
       x[(close_idx + 1):length(x)])

writeLines(x, p, useBytes = TRUE)
cat("DONE. Debug lines added after line", close_idx, "\n")