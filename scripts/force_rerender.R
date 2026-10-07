# scripts/force_rerender.R
# Force the reactive to invalidate after nested assignments.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find the debug block we added (3 cat lines), replace with a re-render trigger

idx <- grep('# DEBUG: show what we stored', x)
if (length(idx) == 0) stop("Could not find debug block")
idx <- idx[1]

# The 3 debug cat lines follow. Find where they end.
end_idx <- idx
for (i in (idx + 1):(idx + 10)) {
  if (grepl('entry error:', x[i])) { end_idx <- i; break }
}

# Replace the whole debug block with a rerender trigger + minimal logging
replacement <- c(
'    # Force re-render: assign the whole list back to itself',
'    ai_state$history <- ai_state$history',
'    cat("[DEBUG] history length:", length(ai_state$history), "\\n")'
)

x <- c(x[1:(idx - 1)], replacement, x[(end_idx + 1):length(x)])

writeLines(x, p, useBytes = TRUE)
cat("DONE. Rerender trigger added.\n")