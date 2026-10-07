# scripts/reinstall_ai.py
# Remove and reinstall the AI Assistant code in app.R.
# Touches ONLY the four AI-related blocks. Nothing else.

import io, re, os, shutil, sys

APP = r"C:\Users\Erick.Amombo\alfalfa_pangenome\app.R"

if not os.path.exists(APP):
    print("ERROR: app.R not found"); sys.exit(1)

with io.open(APP, 'r', encoding='utf-8') as f:
    s = f.read()

# ------------------------------------------------------------
# 1. The ask_groq_for_sql function (top of file, before ui <-)
# ------------------------------------------------------------
NEW_FN = '''ask_groq_for_sql <- function(question) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (nchar(api_key) < 10) {
    return(list(ok = FALSE,
                error = "GROQ_API_KEY is not set in this R session. Run:  set GROQ_API_KEY=gsk_...  then restart."))
  }

  body <- list(
    model = "qwen/qwen3.8-27b",
    messages = list(
      list(role = "system", content = DB_SCHEMA_PROMPT),
      list(role = "user",   content = question)
    ),
    temperature = 0.1,
    max_tokens  = 2000
  )

  res <- tryCatch(
    POST("https://api.groq.com/openai/v1/chat/completions",
         add_headers(Authorization = paste("Bearer", api_key)),
         content_type_json(),
         body = toJSON(body, auto_unbox = TRUE),
         encode = "json",
         timeout(60)),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    return(list(ok = FALSE,
                error = paste("Network error:", conditionMessage(res))))
  }

  if (status_code(res) != 200) {
    msg <- tryCatch({
      p <- content(res, "parsed")
      if (!is.null(p$error$message)) p$error$message else "Unknown API error"
    }, error = function(e) "Unknown error")
    return(list(ok = FALSE,
                error = paste0("Groq API error (HTTP ", status_code(res), "): ", msg)))
  }

  parsed <- content(res, "parsed")
  txt <- parsed$choices[[1]]$message$content
  if (is.null(txt) || !nzchar(txt)) {
    return(list(ok = FALSE, error = "Groq returned an empty response."))
  }
  txt <- gsub("^```sql\\\\s*", "", txt, ignore.case = TRUE)
  txt <- gsub("^```\\\\s*", "", txt)
  txt <- gsub("```\\\\s*$", "", txt)
  txt <- trimws(txt)
  list(ok = TRUE, sql = txt)
}
'''

# Replace existing function (from "ask_groq_for_sql <- function" up to the next "\n}\n\n#" boundary)
s, n_fn = re.subn(
    r'ask_groq_for_sql\s*<-\s*function\(question\)\s*\{.*?\n\}\n',
    NEW_FN,
    s, count=1, flags=re.S
)

# ------------------------------------------------------------
# 2. The server-side observeEvent(input$ai_ask, ...) block
# ------------------------------------------------------------
NEW_OBS = '''  ai_state <- reactiveValues(history = list(), last_ask = NULL)

  observeEvent(input$ai_ask, {
    req(input$ai_question)
    q <- trimws(input$ai_question)
    if (nchar(q) == 0) return()

    now <- Sys.time()
    if (!is.null(ai_state$last_ask) &&
        as.numeric(difftime(now, ai_state$last_ask, units = "secs")) < 0.8) {
      return()
    }
    ai_state$last_ask <- now

    ai_state$history <- c(
      ai_state$history,
      list(list(role = "user", text = q, sql = NULL, result = NULL, error = NULL))
    )
    idx <- length(ai_state$history)

    res <- ask_groq_for_sql(q)

    if (!res$ok) {
      ai_state$history[[idx]]$error <- res$error
      return()
    }

    ai_state$history[[idx]]$sql <- res$sql

    out <- tryCatch({
      con <- connect_db(); on.exit(dbDisconnect(con))
      dbGetQuery(con, res$sql)
    }, error = function(e) e)

    if (inherits(out, "error")) {
      ai_state$history[[idx]]$error <- paste("SQL execution failed:", conditionMessage(out))
    } else {
      ai_state$history[[idx]]$result <- out
    }
  })

  observeEvent(input$ai_clear, {
    ai_state$history <- list()
    ai_state$last_ask <- NULL
  })

  output$ai_conversation <- renderUI({
    if (length(ai_state$history) == 0) return(NULL)

    out <- lapply(seq_along(ai_state$history), function(i) {
      entry <- ai_state$history[[i]]

      if (entry$role == "user") {
        return(div(class = "ai-msg-user",
                   shiny::icon("user"), " ", entry$text))
      }

      items <- list()
      items[[length(items) + 1]] <- div(class = "ai-msg-bot",
                                        shiny::icon("robot"), " Assistant:")

      if (!is.null(entry$error) && nzchar(entry$error)) {
        items[[length(items) + 1]] <- div(class = "ai-err",
                                          shiny::icon("triangle-exclamation"),
                                          " ", entry$error)
      }

      if (!is.null(entry$sql) && nzchar(entry$sql)) {
        items[[length(items) + 1]] <- div(class = "ai-msg-bot",
                                          shiny::icon("database"), " Generated SQL:")
        items[[length(items) + 1]] <- div(class = "ai-sql", entry$sql)
      }

      if (!is.null(entry$result)) {
        items[[length(items) + 1]] <- DTOutput(paste0("ai_result_", i))
      }

      do.call(tagList, items)
    })

    for (i in seq_along(ai_state$history)) {
      entry <- ai_state$history[[i]]
      if (!is.null(entry$result)) {
        local({
          idx <- i
          df <- entry$result
          output[[paste0("ai_result_", idx)]] <- renderDT({
            datatable(df, rownames = FALSE,
                      options = list(pageLength = 10, scrollX = TRUE, dom = 'ftip')) %>%
              formatStyle(columns = 1:ncol(df),
                          color = FONT_COLOR, fontWeight = "600")
          })
        })
      }
    }

    do.call(tagList, out)
  })
'''

# Match: from "ai_state <- reactiveValues(" through the end of the ai_conversation renderUI close.
s, n_obs = re.subn(
    r'ai_state\s*<-\s*reactiveValues\(.*?\n\}\n\)\n',
    NEW_OBS,
    s, count=1, flags=re.S
)

# ------------------------------------------------------------
# Write back and report
# ------------------------------------------------------------
if n_fn == 0 and n_obs == 0:
    print("ERROR: neither function nor observer matched — nothing changed.")
    sys.exit(2)

with io.open(APP, 'w', encoding='utf-8') as f:
    f.write(s)

print("SUCCESS")
print("  ask_groq_for_sql replaced:", n_fn)
print("  ai_state observer replaced:", n_obs)
print("  file size:", os.path.getsize(APP), "bytes")