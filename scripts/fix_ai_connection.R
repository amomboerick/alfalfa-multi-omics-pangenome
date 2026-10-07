# scripts/fix_ai_connection.R
# Replace the observer's per-click connect_db() with a shared session connection.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# --- 1. Replace the DB call in the AI observer with a null-safe version using a pooled handle ---
old <- c(
'    out <- tryCatch({',
'      con <- connect_db(); on.exit(dbDisconnect(con))',
'      dbGetQuery(con, res$sql)',
'    }, error = function(e) e)'
)

new <- c(
'    out <- tryCatch({',
'      if (is.null(shared_con) || !dbIsValid(shared_con)) {',
'        shared_con <<- connect_db()',
'      }',
'      dbGetQuery(shared_con, res$sql)',
'    }, error = function(e) e)'
)

d_start <- which(x == old[1])
if (length(d_start) == 0) {
  cat("Pattern not found at expected location. Trying looser match...\n")
  # Looser match
  for (i in seq_along(x)) {
    if (grepl("con <- connect_db\\(\\); on.exit\\(dbDisconnect\\(con\\)\\)", x[i]) &&
        grepl("dbGetQuery\\(con, res\\$sql\\)", x[i + 1])) {
      # Replace 3 lines starting here
      x <- c(x[1:(i - 1)],
             '      if (is.null(shared_con) || !dbIsValid(shared_con)) {',
             '        shared_con <<- connect_db()',
             '      }',
             '      dbGetQuery(shared_con, res$sql)',
             x[(i + 3):length(x)])
      writeLines(x, p, useBytes = TRUE)
      cat("DONE. AI observer using shared connection.\n")
      quit(status = 0)
    }
  }
  stop("Could not find the AI DB call block")
}

# Block found exactly
d_end <- d_start + length(old) - 1
if (!all(x[d_start:d_end] == old)) stop("Pattern mismatch on second pass")

new_full <- c(
'    out <- tryCatch({',
'      if (is.null(shared_con) || !dbIsValid(shared_con)) {',
'        shared_con <<- connect_db()',
'      }',
'      dbGetQuery(shared_con, res$sql)',
'    }, error = function(e) e)'
)

x <- c(x[1:(d_start - 1)], new_full, x[(d_end + 1):length(x)])

# --- 2. Add shared_con declaration at the top of the server function ---
srv_start <- grep("^server <- function\\(input, output, session\\) \\{", x)
if (length(srv_start) == 0) stop("Could not find server function")
srv_start <- srv_start[1]

# Check if already added
if (!any(grepl("shared_con <- NULL", x, fixed = TRUE))) {
  x <- c(x[1:srv_start],
         '  shared_con <- NULL',
         '  session$onSessionEnded(function() {',
         '    if (!is.null(shared_con) && dbIsValid(shared_con)) dbDisconnect(shared_con)',
         '  })',
         x[(srv_start + 1):length(x)])
}

writeLines(x, p, useBytes = TRUE)
cat("DONE. AI observer using shared connection.\n")