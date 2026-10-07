# scripts/restore_observer.R
# Insert the missing observeEvent opening line.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find "ai_state <- reactiveValues(history = list(), last_ask = NULL)"
idx <- grep("^\\s*ai_state <- reactiveValues\\(history = list\\(\\), last_ask = NULL\\)\\s*$", x)
if (length(idx) == 0) stop("Could not find ai_state reactiveValues line")
idx <- idx[1]

# Insert the observeEvent line right after it
x <- c(x[1:idx],
       '',
       '  observeEvent(input$ai_ask, {',
       x[(idx + 1):length(x)])

writeLines(x, p, useBytes = TRUE)
cat("DONE. observeEvent line restored after line", idx, "\n")