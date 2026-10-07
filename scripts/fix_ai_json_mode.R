# scripts/fix_ai_json_mode.R
# Rewrite ask_groq_for_sql to use Groq's JSON mode (structured output).

p <- "C:/Users/Erick.Amombo/alfalfa_pangenome/app.R"
x <- readLines(p, warn = FALSE)

# ---- 1. Rewrite DB_SCHEMA_PROMPT to require JSON output ----
new_prompt_start <- grep("^DB_SCHEMA_PROMPT <- \"", x)
if (length(new_prompt_start) == 0) stop("Could not find DB_SCHEMA_PROMPT")
s1 <- new_prompt_start[1]

# Find end of prompt string (line ending with closing ")
s2 <- NA
for (i in (s1 + 1):length(x)) {
  if (grepl('^\\s*"\\s*$', x[i])) { s2 <- i; break }
}
if (is.na(s2)) stop("Could not find end of DB_SCHEMA_PROMPT")

new_prompt <- c(
'DB_SCHEMA_PROMPT <- "',
'You are an expert PostgreSQL assistant for the alfalfa pan-genome database.',
'',
'Given a user question, respond with ONLY a JSON object of the form:',
'  {\\"sql\\": \\"<your SQL query here>\\"}',
'',
'Do not include any text outside the JSON. Do not include markdown fences.',
'If the question cannot be answered with SQL, respond with:',
'  {\\"sql\\": null, \\"error\\": \\"<short reason>\\"}',
'',
'Database: alfalfa_pangenome (PostgreSQL 15, port 5433)',
'',
'Schema:',
'  accessions(accession_id, accession_name, species, subspecies, cultivar,',
'             genome_size, gc_content, n50, contig_count, gene_count)',
'  contigs(contig_id, accession_id, contig_name, contig_length, gc_content)',
'  genes(gene_id, accession_id, contig_id, gene_name, gene_symbol,',
'        start_pos, end_pos, strand, gene_type, biotype, attributes)',
'  pan_gene_clusters(cluster_id, cluster_name, cluster_type, gene_count,',
'                    presence_across_accessions)',
'  cluster_membership(membership_id, cluster_id, gene_id, accession_id)',
'  gene_presence_absence(pa_id, cluster_id, accession_id, is_present, copy_number)',
'  go_annotations(go_ann_id, gene_id, accession_id, go_term)',
'  kegg_ko(ko_id, gene_id, accession_id, ko_number)',
'  kegg_pathways(pathway_id, gene_id, accession_id, pathway_code)',
'  kog_annotations(kog_id, gene_id, accession_id, cog_letter)',
'  crispr_guides(guide_id, gene_id, accession_id, genome, gene_name,',
'                rank_in_gene, chrom, strand, gene_strand, spacer_seq, pam_seq,',
'                spacer_start, spacer_end, pam_start, pam_end, cut_site,',
'                gc_pct, n_genome_hits, uniqueness, specificity_class)',
'',
'Important notes:',
'- The table for pan-gene clusters is pan_gene_clusters (not clusters).',
'- Core clusters are: cluster_type = \'core\'.',
'- cluster_type values: core, soft_core, dispensable, private, singleton.',
'- 12 accessions, 823,838 genes, ~217,000 clusters, 3.56M CRISPR guides.',
'- Always LIMIT 100 unless aggregate counts are requested.',
'"'
)

x <- c(x[1:(s1 - 1)], new_prompt, x[(s2 + 1):length(x)])

# ---- 2. Rewrite ask_groq_for_sql function with JSON mode ----
start <- grep("^ask_groq_for_sql <- function\\(question\\)", x)
if (length(start) == 0) stop("Could not find ask_groq_for_sql")
start <- start[1]

depth <- 0
end <- NA
for (i in start:length(x)) {
  line <- x[i]
  depth <- depth + lengths(regmatches(line, gregexpr("\\{", line)))
  depth <- depth - lengths(regmatches(line, gregexpr("\\}", line)))
  if (depth == 0 && i > start) { end <- i; break }
}
if (is.na(end)) stop("Could not find closing brace of ask_groq_for_sql")

replacement <- c(
'ask_groq_for_sql <- function(question) {',
'  api_key <- Sys.getenv("GROQ_API_KEY")',
'  if (nchar(api_key) < 10) {',
'    return(list(ok = FALSE, error = "GROQ_API_KEY is not set"))',
'  }',
'',
'  body <- list(',
'    model = "qwen/qwen3.8-27b",',
'    messages = list(',
'      list(role = "system", content = DB_SCHEMA_PROMPT),',
'      list(role = "user",   content = question)',
'    ),',
'    temperature = 0.1,',
'    max_tokens  = 2000,',
'    response_format = list(type = "json_object")',
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
'      p <- content(res, "parsed")',
'      if (!is.null(p$error$message)) p$error$message else "Unknown API error"',
'    }, error = function(e) "Unknown error")',
'    return(list(ok = FALSE, error = paste0("Groq HTTP ", status_code(res), ": ", msg)))',
'  }',
'',
'  parsed <- tryCatch(content(res, "parsed"), error = function(e) NULL)',
'  if (is.null(parsed) || is.null(parsed$choices) || length(parsed$choices) == 0) {',
'    return(list(ok = FALSE, error = "Groq returned no choices"))',
'  }',
'',
'  # The model returns a JSON string in message$content',
'  json_text <- parsed$choices[[1]]$message$content',
'  if (is.null(json_text) || !nzchar(json_text)) {',
'    return(list(ok = FALSE, error = "Empty content from Groq"))',
'  }',
'',
'  # Parse the inner JSON',
'  inner <- tryCatch(fromJSON(json_text, simplifyVector = TRUE), error = function(e) NULL)',
'  if (is.null(inner)) {',
'    return(list(ok = FALSE, error = paste("Could not parse JSON:", substr(json_text, 1, 150))))',
'  }',
'',
'  if (!is.null(inner$error)) {',
'    return(list(ok = FALSE, error = inner$error))',
'  }',
'',
'  sql <- inner$sql',
'  if (is.null(sql) || !nzchar(sql)) {',
'    return(list(ok = FALSE, error = "Model returned no SQL field"))',
'  }',
'',
'  list(ok = TRUE, sql = trimws(sql))',
'}'
)

x <- c(x[1:(start - 1)], replacement, x[(end + 1):length(x)])

writeLines(x, p, useBytes = TRUE)
cat("DONE. JSON mode applied.\n")