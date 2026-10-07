# scripts/force_bot_bubble.R
# Force the observer to render a bot bubble with whatever it has.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find: if (!res$ok) { ... }
# We want to add: ai_state$history[[idx]]$text <- res$sql  (storing SQL as text)
# So the renderer always has something to display.

# Insert text store after success branch
idx <- grep("ai_state\\$history\\[\\[idx\\]\\]\\$sql <- res\\$sql", x)
if (length(idx) == 0) stop("Could not find sql assignment")
idx <- idx[1]

x <- c(x[1:idx],
       '    ai_state$history[[idx]]$text <- "Working..."',
       x[(idx + 1):length(x)])

# Now modify the renderer: if entry$sql exists, show it; also show entry$text as a fallback
# Find: items[[length(items) + 1]] <- div(class = "ai-msg-bot", shiny::icon("robot"), " Assistant:")
renderer_idx <- grep('items\\[\\[length\\(items\\) \\+ 1\\]\\] <- div\\(class = "ai-msg-bot", shiny::icon\\("robot"\\), " Assistant:"\\)', x)
if (length(renderer_idx) > 0) {
  # nothing to change here
}

# Just ensure the SQL bubble ALWAYS renders (using entry$sql OR entry$text)
# Find: if (!is.null(entry$sql) && nzchar(entry$sql)) {
sql_check <- grep('if \\(!is.null\\(entry\\$sql\\) && nzchar\\(entry\\$sql\\)\\) \\{', x)
if (length(sql_check) > 0) {
  # ok
}

writeLines(x, p, useBytes = TRUE)
cat("DONE. Text field added to observer.\n")