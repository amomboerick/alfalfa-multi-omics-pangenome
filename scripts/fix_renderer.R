# scripts/fix_renderer.R
# Replace the ai_conversation renderUI with a robust version.

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# Find start of renderUI
start <- grep("^\\s*output\\$ai_conversation <- renderUI\\(\\{", x)
if (length(start) == 0) stop("Could not find ai_conversation renderUI")
start <- start[1]

# Find its close -- count braces
depth <- 0
end <- NA
for (i in start:length(x)) {
  line <- x[i]
  depth <- depth + lengths(regmatches(line, gregexpr("\\{", line)))
  depth <- depth - lengths(regmatches(line, gregexpr("\\}", line)))
  if (depth == 0 && i > start) { end <- i; break }
}
if (is.na(end)) stop("Could not find end of renderUI block")

cat("Replacing lines", start, "to", end, "\n")

replacement <- c(
'  output$ai_conversation <- renderUI({',
'    if (length(ai_state$history) == 0) return(NULL)',
'',
'    # Register DTOutputs for any entry that has a result',
'    for (i in seq_along(ai_state$history)) {',
'      entry <- ai_state$history[[i]]',
'      if (!is.null(entry$result)) {',
'        local({',
'          idx <- i',
'          df <- entry$result',
'          output[[paste0("ai_result_", idx)]] <- renderDT({',
'            datatable(df, rownames = FALSE,',
'                      options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")) %>%',
'              formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")',
'          })',
'        })',
'      }',
'    }',
'',
'    # Build the bubbles',
'    out <- lapply(seq_along(ai_state$history), function(i) {',
'      entry <- ai_state$history[[i]]',
'',
'      if (entry$role == "user") {',
'        return(div(class = "ai-msg-user", shiny::icon("user"), " ", entry$text))',
'      }',
'',
'      items <- list()',
'',
'      # Bot header',
'      items[[length(items) + 1]] <- div(class = "ai-msg-bot",',
'                                        shiny::icon("robot"), " Assistant:")',
'',
'      # Error message (if any)',
'      if (!is.null(entry$error) && nzchar(entry$error)) {',
'        items[[length(items) + 1]] <- div(class = "ai-err",',
'                                          shiny::icon("triangle-exclamation"),',
'                                          " ", entry$error)',
'      }',
'',
'      # SQL display',
'      if (!is.null(entry$sql) && nzchar(entry$sql)) {',
'        items[[length(items) + 1]] <- div(class = "ai-sql", entry$sql)',
'      }',
'',
'      # Result table',
'      if (!is.null(entry$result)) {',
'        items[[length(items) + 1]] <- DTOutput(paste0("ai_result_", i))',
'      }',
'',
'      # Fallback: if nothing was stored, show a friendly message',
'      if (is.null(entry$error) && is.null(entry$sql) && is.null(entry$result)) {',
'        items[[length(items) + 1]] <- div(class = "ai-msg-bot",',
'                                          " (no reply)")',
'      }',
'',
'      do.call(tagList, items)',
'    })',
'',
'    do.call(tagList, out)',
'  })'
)

new <- c(x[seq_len(start - 1)], replacement, x[(end + 1):length(x)])
writeLines(new, p, useBytes = TRUE)
cat("DONE. Renderer replaced.\n")