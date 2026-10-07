# scripts/fix_bot_entry.R
# Append a proper bot entry to history after the SQL runs.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find the debug line and everything back to "res <- ask_groq_for_sql(q)"
start <- grep("res <- ask_groq_for_sql\\(q\\)", x)
if (length(start) == 0) stop("Could not find res <- ask_groq_for_sql(q)")
start <- start[1]

# Find the end -- the rerender trigger we just added
end <- grep('ai_state\\$history <- ai_state\\$history', x)
if (length(end) == 0) stop("Could not find rerender trigger")
end <- end[1]

cat("Replacing lines", start, "to", end, "\n")

replacement <- c(
'    res <- ask_groq_for_sql(q)',
'',
'    if (!isTRUE(res$ok)) {',
'      ai_state$history <- c(ai_state$history,',
'        list(list(role = "bot", text = NULL, sql = NULL, result = NULL, error = res$error)))',
'      ai_state$history <- ai_state$history',
'      return()',
'    }',
'',
'    out <- tryCatch({',
'      con <- connect_db(); on.exit(dbDisconnect(con))',
'      dbGetQuery(con, res$sql)',
'    }, error = function(e) e)',
'',
'    if (inherits(out, "error")) {',
'      ai_state$history <- c(ai_state$history,',
'        list(list(role = "bot", text = NULL, sql = res$sql, result = NULL,',
'                  error = paste("SQL execution failed:", conditionMessage(out)))))',
'    } else {',
'      ai_state$history <- c(ai_state$history,',
'        list(list(role = "bot", text = NULL, sql = res$sql, result = out, error = NULL)))',
'    }',
'',
'    # Force the renderUI to re-run',
'    ai_state$history <- ai_state$history',
'    cat("[DEBUG] history length:", length(ai_state$history), "\\n")'
)

new <- c(x[seq_len(start - 1)], replacement, x[(end + 1):length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Bot entry appended after user entry.\n")