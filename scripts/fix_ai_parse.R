# scripts/fix_ai_parse.R

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

start <- grep('^ask_groq_for_sql <- function\\(question\\)', x)
if (length(start) == 0) stop("Could not find ask_groq_for_sql start")
start <- start[1]

depth <- 0
end <- NA
for (i in start:length(x)) {
  line <- x[i]
  depth <- depth + lengths(regmatches(line, gregexpr("\\{", line)))
  depth <- depth - lengths(regmatches(line, gregexpr("\\}", line)))
  if (depth == 0 && i > start) { end <- i; break }
}
if (is.na(end)) stop("Could not find closing brace")

replacement <- c(
'ask_groq_for_sql <- function(question) {',
'  api_key <- Sys.getenv("GROQ_API_KEY")',
'  if (nchar(api_key) < 10) {',
'    return(list(ok = FALSE,',
'                error = "GROQ_API_KEY is not set. Run: set GROQ_API_KEY=gsk_... then restart."))',
'  }',
'',
'  body <- list(',
'    model = "qwen/qwen3.8-27b",',
'    messages = list(',
'      list(role = "system", content = DB_SCHEMA_PROMPT),',
'      list(role = "user",   content = question)',
'    ),',
'    temperature = 0.1,',
'    max_tokens  = 2000',
'  )',
'',
'  res <- tryCatch(',
'    POST("https://api.groq.com/openai/v1/chat/completions",',
'         add_headers(Authorization = paste("Bearer", api_key)),',
'         content_type_json(),',
'         body = toJSON(body, auto_unbox = TRUE),',
'         encode = "json",',
'         timeout(60)),',
'    error = function(e) e',
'  )',
'',
'  if (inherits(res, "error")) {',
'    return(list(ok = FALSE, error = paste("Network error:", conditionMessage(res))))',
'  }',
'',
'  if (status_code(res) != 200) {',
'    msg <- tryCatch({',
'      pp <- content(res, "parsed")',
'      if (!is.null(pp$error$message)) pp$error$message else "Unknown"',
'    }, error = function(e) "Unknown")',
'    return(list(ok = FALSE, error = paste0("Groq HTTP ", status_code(res), ": ", msg)))',
'  }',
'',
'  parsed <- content(res, "parsed")',
'  txt <- parsed$choices[[1]]$message$content',
'  if (is.null(txt) || !nzchar(txt)) {',
'    return(list(ok = FALSE, error = "Empty response from Groq"))',
'  }',
'',
'  txt <- gsub("```[a-zA-Z]*", "", txt)',
'  txt <- gsub("```", "", txt)',
'  txt <- trimws(txt)',
'',
'  first_word <- toupper(sub("[^A-Za-z].*$", "", txt))',
'  if (!first_word %in% c("SELECT", "WITH")) {',
'    return(list(ok = FALSE, error = paste0("Not SQL: ", substr(txt, 1, 150))))',
'  }',
'',
'  list(ok = TRUE, sql = txt)',
'}'
)

new <- c(x[seq_len(start - 1)], replacement, x[(end + 1):length(x)])
writeLines(new, p, useBytes = TRUE)

cat("DONE. Replaced lines", start, "to", end, "\n")