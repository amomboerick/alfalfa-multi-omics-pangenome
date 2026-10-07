# scripts/debug_observer.R
# Add verbose logging to the AI observer so we can see exactly where it hangs.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find the line:  res <- ask_groq_for_sql(q)
idx1 <- grep("res <- ask_groq_for_sql\\(q\\)", x)
if (length(idx1) == 0) stop("Could not find res <- ask_groq_for_sql(q)")
idx1 <- idx1[1]

# Insert logging before and after
new_before <- '    cat("[AI] About to call ask_groq_for_sql\\n")'
new_after  <- '    cat("[AI] Returned:", if (isTRUE(res$ok)) "ok=TRUE" else paste("ok=FALSE error=", res$error), "\\n")'

x <- c(x[1:(idx1 - 1)],
       new_before,
       x[idx1],
       new_after,
       x[(idx1 + 1):length(x)])

# Also log the DB step
idx2 <- grep("dbGetQuery\\(con, res\\$sql\\)", x)
if (length(idx2) > 0) {
  idx2 <- idx2[1]
  x <- c(x[1:(idx2 - 1)],
         '      cat("[AI] Running SQL:", res$sql, "\\n")',
         x[idx2:length(x)])
}

# Log when the observer starts
idx3 <- grep("^\\s*observeEvent\\(input\\$ai_ask", x)
if (length(idx3) > 0) {
  idx3 <- idx3[1]
  # The line already contains "cat("\nAIBUTTONCLICKED\n");" — good, leave it
}

writeLines(x, p, useBytes = TRUE)
cat("DONE. Debug logging added to AI observer.\n")