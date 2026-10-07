# ============================================================
# Alfalfa Multi-Omics Pan-Genome Database - R Shiny App
# v2.6 - Groq model fixed to openai/gpt-oss-120b
# ============================================================

library(shiny)
library(shinythemes)
library(DT)
library(DBI)
library(RPostgres)
library(dplyr)
library(httr)
library(jsonlite)

DB_CONFIG <- list(
  host = "localhost", port = 5433,
  dbname = "alfalfa_pangenome",
  user = "postgres", password = "postgres"
)

connect_db <- function() {
  dbConnect(RPostgres::Postgres(),
            host = DB_CONFIG$host, port = DB_CONFIG$port,
            dbname = DB_CONFIG$dbname,
            user = DB_CONFIG$user, password = DB_CONFIG$password)
}

FONT_COLOR <- "#0B0B0B"
FONT_BOLD  <- 2
AXIS_CEX   <- 1.35
TITLE_CEX  <- 1.55
LABEL_CEX  <- 1.35

# ============================================================
# AI: Groq NL -> SQL
# ============================================================
DB_SCHEMA_PROMPT <- "
You are an expert PostgreSQL assistant for the alfalfa pan-genome database.

Given a user question, respond with ONLY a JSON object of the form:
  {\"sql\": \"<your SQL query here>\"}

Do not include any text outside the JSON. Do not include markdown fences.
If the question cannot be answered with SQL, respond with:
  {\"sql\": null, \"error\": \"<short reason>\"}

Database: alfalfa_pangenome (PostgreSQL 15, port 5433)

Schema:
  accessions(accession_id, accession_name, species, subspecies, cultivar,
             genome_size, gc_content, n50, contig_count, gene_count)
  contigs(contig_id, accession_id, contig_name, contig_length, gc_content)
  genes(gene_id, accession_id, contig_id, gene_name, gene_symbol,
        start_pos, end_pos, strand, gene_type, biotype, attributes)
  pan_gene_clusters(cluster_id, cluster_name, cluster_type, gene_count,
                    presence_across_accessions)
  cluster_membership(membership_id, cluster_id, gene_id, accession_id)
  gene_presence_absence(pa_id, cluster_id, accession_id, is_present, copy_number)
  go_annotations(go_ann_id, gene_id, accession_id, go_term)
  kegg_ko(ko_id, gene_id, accession_id, ko_number)
  kegg_pathways(pathway_id, gene_id, accession_id, pathway_code)
  kog_annotations(kog_id, gene_id, accession_id, cog_letter)
  crispr_guides(guide_id, gene_id, accession_id, genome, gene_name,
                rank_in_gene, chrom, strand, gene_strand, spacer_seq, pam_seq,
                spacer_start, spacer_end, pam_start, pam_end, cut_site,
                gc_pct, n_genome_hits, uniqueness, specificity_class)

Important notes:
- The table for pan-gene clusters is pan_gene_clusters (not clusters).
- Core clusters are: cluster_type = 'core'.
- cluster_type values: core, soft_core, dispensable, private, singleton.
- 12 accessions, 823,838 genes, ~217,000 clusters, 3.56M CRISPR guides.
- Always LIMIT 100 unless aggregate counts are requested.
"

ask_groq_for_sql <- function(question) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (nchar(api_key) < 10) {
    return(list(ok = FALSE, error = "GROQ_API_KEY is not set"))
  }

  body <- list(
    model = "qwen/qwen3.8-27b",
    messages = list(
      list(role = "system", content = DB_SCHEMA_PROMPT),
      list(role = "user",   content = question)
    ),
    temperature = 0.1,
    max_tokens  = 2000,
    response_format = list(type = "json_object")
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
    return(list(ok = FALSE, error = paste("Network error:", conditionMessage(res))))
  }

  if (status_code(res) != 200) {
    msg <- tryCatch({
      p <- content(res, "parsed")
      if (!is.null(p$error$message)) p$error$message else "Unknown API error"
    }, error = function(e) "Unknown error")
    return(list(ok = FALSE, error = paste0("Groq HTTP ", status_code(res), ": ", msg)))
  }

  parsed <- tryCatch(content(res, "parsed"), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$choices) || length(parsed$choices) == 0) {
    return(list(ok = FALSE, error = "Groq returned no choices"))
  }

  # The model returns a JSON string in message$content
  json_text <- parsed$choices[[1]]$message$content
  if (is.null(json_text) || !nzchar(json_text)) {
    return(list(ok = FALSE, error = "Empty content from Groq"))
  }

  # Parse the inner JSON
  inner <- tryCatch(fromJSON(json_text, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(inner)) {
    return(list(ok = FALSE, error = paste("Could not parse JSON:", substr(json_text, 1, 150))))
  }

  if (!is.null(inner$error)) {
    return(list(ok = FALSE, error = inner$error))
  }

  sql <- inner$sql
  if (is.null(sql) || !nzchar(sql)) {
    return(list(ok = FALSE, error = "Model returned no SQL field"))
  }

  list(ok = TRUE, sql = trimws(sql))
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$script(HTML("
      document.addEventListener('keydown', function(e) {
        if (e.key === '/' && e.target.tagName !== 'INPUT' && e.target.tagName !== 'TEXTAREA') {
          e.preventDefault();
          var searchBox = document.querySelector('input[type=text]');
          if (searchBox) searchBox.focus();
        }
        if (e.key === 'Escape') {
          var modal = document.querySelector('.modal');
          if (modal) {
            var closeBtn = modal.querySelector('.modal-footer button, .modal-header button');
            if (closeBtn) closeBtn.click();
          }
        }
      });
    ")),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$style(HTML("
    html, body { min-height: 100%; }
    body {
      background: #e8efe9;
      display: flex; flex-direction: column; min-height: 100vh;
      font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
      color: #0c1f17; font-weight: 500;
    }
    .main-wrapper { flex: 1 0 auto; }
    *:focus { outline: 3px solid #F5B342 !important; outline-offset: 2px; }
    .skip-link { position: absolute; top: -100px; left: 0; background: #0B3B2C; color: white; padding: 8px 16px; z-index: 9999; transition: top 0.2s; }
    .skip-link:focus { top: 0; }
    .masthead {
      background: linear-gradient(135deg, #0B3B2C 0%, #1A4D38 60%, #2B7A5E 100%);
      color: white; padding: 2rem 2.5rem 1.5rem 2.5rem;
      border-bottom: 6px solid #F5B342;
      position: relative; overflow: hidden; margin-bottom: 20px;
    }
    .masthead::before {
      content: ''; position: absolute; top: -50%; right: -10%;
      width: 400px; height: 400px;
      background: radial-gradient(circle, rgba(245,179,66,0.12) 0%, transparent 70%);
      pointer-events: none;
    }
    .masthead-inner { max-width: 1400px; margin: 0 auto; position: relative; z-index: 2; }
    .masthead h1 {
      font-weight: 800; font-size: clamp(1.6rem, 3vw, 2.3rem);
      letter-spacing: -0.02em; margin: 0; color: white;
      display: flex; align-items: center; gap: 16px; flex-wrap: wrap;
    }
    .masthead h1 .logo-leaf { color: #F5B342; font-size: 1.9rem; }
    .masthead .subtitle { font-size: clamp(0.9rem, 1.4vw, 1.05rem); color: #eaf2ec; margin-top: 8px; font-weight: 500; }
    .masthead .meta-row { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 16px; align-items: center; }
    .meta-badge {
      display: inline-flex; align-items: center; gap: 6px;
      background: rgba(255,255,255,0.14);
      border: 1px solid rgba(245,179,66,0.55);
      color: #FFD166; padding: 4px 14px;
      border-radius: 40px; font-size: 0.78rem; font-weight: 700;
    }
    .meta-badge .val { color: white; margin-left: 4px; font-weight: 800; }
    .cite-btn {
      background: #F5B342; color: #0B3B2C !important;
      border: none; padding: 8px 22px; border-radius: 40px;
      font-weight: 800; font-size: 0.85rem; cursor: pointer;
      transition: all 0.2s ease;
      display: inline-flex; align-items: center; gap: 8px;
      box-shadow: 0 4px 12px rgba(245,179,66,0.3);
    }
    .cite-btn:hover { background: #fccf6b; transform: translateY(-2px); }
    .license-btn {
      background: rgba(255,255,255,0.12); color: #FFD166 !important;
      border: 1px solid rgba(245,179,66,0.55);
      padding: 8px 20px; border-radius: 40px;
      font-weight: 700; font-size: 0.85rem; cursor: pointer;
      transition: all 0.2s ease;
      display: inline-flex; align-items: center; gap: 8px; margin-left: 8px;
    }
    .license-btn:hover { background: rgba(255,255,255,0.2); transform: translateY(-2px); }
    .stat-box {
      background: white; border-left: 8px solid #1a4d38;
      border-radius: 12px; padding: 15px; text-align: center;
      box-shadow: 0 4px 12px rgba(0,0,0,0.05); margin-bottom: 15px;
      transition: 0.15s;
    }
    .stat-box:hover { transform: translateY(-2px); box-shadow: 0 8px 20px rgba(0,0,0,0.08); }
    .stat-box h2 { color: #0B3B2C; font-size: clamp(1.5rem, 2.5vw, 2.2rem); margin: 0; font-weight: 800; }
    .stat-box p { color: #0c1f17; margin: 0; font-weight: 700; font-size: 0.9rem; }
    .quick-card {
      background: white; border-left: 6px solid #f5b342;
      border-radius: 10px; padding: 16px; margin-bottom: 16px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.04);
      transition: 0.15s; cursor: pointer;
    }
    .quick-card:hover { transform: translateY(-3px); box-shadow: 0 8px 20px rgba(0,0,0,0.08); }
    .quick-card h4 { color: #0B3B2C; margin: 0 0 6px 0; font-weight: 800; }
    .quick-card p { color: #1a3a2a; margin: 0; font-size: 0.9rem; font-weight: 600; }
    .quick-card i { color: #b36b0e; font-size: 1.6rem; margin-right: 10px; }
    .badge { display:inline-block; padding:2px 10px; border-radius:40px; font-size:0.75rem; font-weight:800; margin-left:6px; }
    .badge-core { background:#1a4d38; color:white; }
    .badge-soft_core { background:#f5b342; color:#08261a; }
    .badge-dispensable { background:#d48c1a; color:white; }
    .badge-private { background:#8b5e9b; color:white; }
    .badge-singleton { background:#5a7a8a; color:white; }
    .ai-msg-user { background: #e8efe9; border-left: 4px solid #1a4d38; padding: 10px 14px; border-radius: 8px; margin-bottom: 10px; color: #0c1f17; font-weight: 500; }
    .ai-msg-bot { background: #fff7e6; border-left: 4px solid #f5b342; padding: 10px 14px; border-radius: 8px; margin-bottom: 10px; color: #0c1f17; font-weight: 500; }
    .ai-sql { background: #08261a; color: #b8e3b0; padding: 12px; border-radius: 6px; font-family: monospace; font-size: 0.85rem; white-space: pre-wrap; margin: 8px 0; overflow-x: auto; }
    .ai-err { background: #fdecea; border-left: 4px solid #c0392b; padding: 10px 14px; border-radius: 8px; margin-bottom: 10px; color: #7b241c; font-weight: 600; }
    .license-badge { display: inline-block; padding: 6px 16px; border-radius: 40px; font-weight: 700; font-size: 0.85rem; margin-right: 8px; margin-bottom: 8px; }
    .license-mit { background: #d4e3db; color: #0B3B2C; border: 2px solid #1a4d38; }
    .license-cc { background: #f5e7ce; color: #4d2800; border: 2px solid #d48c1a; }
    .license-box { background: #f7faf5; padding: 18px 22px; border-left: 6px solid #F5B342; border-radius: 8px; margin: 16px 0; }
    .license-box h5 { color: #0B3B2C; font-weight: 800; margin-top: 0; margin-bottom: 10px; font-size: 1rem; }
    .license-box p, .license-box li { color: #0c1f17; font-weight: 500; font-size: 0.9rem; line-height: 1.65; }
    .license-box ul { padding-left: 20px; margin: 8px 0; }
    .license-box li { margin-bottom: 4px; }
    .footer { flex-shrink: 0; background: #08261a; color: #e5f0e9; padding: 2rem 2rem 1rem 2rem; margin-top: 40px; border-top: 6px solid #f5b342; }
    .footer h4 { color: #f5b342; font-size: 1rem; font-weight: 800; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px; }
    .footer p, .footer li { font-size: 0.85rem; color: #c7d9cd; font-weight: 500; }
    .footer a { color: #e5f0e9; text-decoration: none; }
    .footer a:hover { color: #f5b342; }
    .footer ul { list-style: none; padding: 0; }
    .footer ul li { padding: 3px 0; }
    .footer-bottom { border-top: 1px solid #1f4a38; margin-top: 20px; padding-top: 12px; text-align: center; font-size: 0.8rem; color: #a6c1b0; }
    .footer-bottom strong { color: #f5b342; }
    .footer .license-tag { display: inline-block; padding: 2px 12px; border-radius: 20px; font-size: 0.72rem; font-weight: 700; margin-right: 8px; background: rgba(245,179,66,0.15); color: #f5b342; border: 1px solid rgba(245,179,66,0.4); }
    table.dataTable tbody tr { cursor: pointer; }
    table.dataTable tbody tr:hover { background: #fef6e0 !important; }
    @media (max-width: 768px) {
      .masthead { padding: 1.2rem 1rem; }
      .meta-badge { font-size: 0.7rem; padding: 3px 10px; }
      .license-btn, .cite-btn { font-size: 0.75rem; padding: 6px 14px; margin-left: 0; margin-top: 6px; }
      .stat-box h2 { font-size: 1.5rem; }
      .nav-tabs { flex-wrap: nowrap; overflow-x: auto; }
      .nav-tabs > li { white-space: nowrap; }
      .container-fluid { padding-left: 24px; padding-right: 24px; max-width: 100% !important; }
      .footer { padding: 1.5rem 1rem; }
      .quick-card { padding: 12px; }
    }
    @media (max-width: 480px) {
      .masthead h1 { font-size: 1.3rem; }
      .meta-badge { font-size: 0.65rem; padding: 2px 8px; }
      .stat-box h2 { font-size: 1.2rem; }
      .stat-box p { font-size: 0.75rem; }
    }
    /* ==== Layout fixes ==== */
    .container-fluid {
      max-width: 100% !important;
      width: 100% !important;
      padding-left: 32px !important;
      padding-right: 32px !important;
    }
    .tabbable > .nav-tabs {
      display: flex;
      flex-wrap: nowrap !important;
      overflow-x: auto;
      overflow-y: hidden;
      white-space: nowrap;
      border-bottom: 3px solid #1a4d38;
    }
    .tabbable > .nav-tabs > li {
      float: none !important;
      flex: 0 0 auto;
    }
    .tabbable > .nav-tabs > li > a {
      font-size: 15px !important;
      padding: 10px 16px !important;
      border: none !important;
    }
    .tabbable > .nav-tabs > li.active > a {
      background: #1a4d38 !important;
      color: #ffffff !important;
      border-radius: 6px 6px 0 0;
    }
    .stat-box {
      min-height: 110px !important;
      padding: 16px 12px !important;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
    }
    .stat-box h2 {
      font-size: 1.9rem !important;
      line-height: 1.1;
      margin-bottom: 4px;
    }
    .stat-box p {
      font-size: 0.95rem !important;
      margin: 0;
    }
    body {
      font-size: 17px !important;
    }
    .masthead-inner {
      max-width: 100% !important;
      padding-left: 32px !important;
      padding-right: 32px !important;
    }

    body, .content-wrapper {
      font-size: 15px !important;
      line-height: 1.55 !important;
    }
    h1, h2, h3, h4, h5 {
    }
    h3 { font-size: 1.55rem !important; }
    h4 { font-size: 1.20rem !important; }
    /* ==== White plot backgrounds ==== */
    .shiny-plot-output, img.shiny-plot-output {
      background: #ffffff !important;
    }
    /* ==== Tables fill tab width ==== */
    .dataTables_wrapper,
    .dataTables_wrapper .dataTables_scroll,
    .dataTables_wrapper .dataTables_scrollHead,
    .dataTables_wrapper .dataTables_scrollBody {
      width: 100% !important;
    }
    .tab-content .container-fluid {
      max-width: 100% !important;
      width: 100% !important;
    }
    /* ==== Container fills window ==== */
    .container-fluid {
      max-width: 100% !important;
      width: 100% !important;
      padding-left: 24px !important;
      padding-right: 24px !important;
    }


    /* ================================================
       FINAL LAYOUT OVERRIDE -- full width, readable fonts
       ================================================ */

    /* Header uses the full window */
    .masthead-inner {
      max-width: 100% !important;
      margin: 0 !important;
      padding-left: 32px !important;
      padding-right: 32px !important;
    }

    /* Tab content uses the full window */
    .container-fluid {
      max-width: 100% !important;
      width: 100% !important;
      padding-left: 32px !important;
      padding-right: 32px !important;
    }

    /* Tables fill the whole tab, not a narrow column */
    .dataTables_wrapper {
      width: 100% !important;
    }
    .dataTables_wrapper .dataTables_scroll,
    .dataTables_wrapper .dataTables_scrollHead,
    .dataTables_wrapper .dataTables_scrollBody,
    table.dataTable {
      width: 100% !important;
    }

    /* Readable fonts everywhere -- single-quoted names are safe in R */
    body, .content-wrapper, h1, h2, h3, h4, h5, p, li, td, th, a, label, .dataTable, .btn, .form-control, .selectize-input {
      font-family: Segoe UI, Calibri, Arial, Helvetica, sans-serif !important; font-size: 17px !important;
    }
    body {
      font-size: 17px !important;
      line-height: 1.6 !important;
    }
    h3 { font-size: 1.65rem !important; }
    h4 { font-size: 1.25rem !important; }
    p, li, td, th, label { font-size: 1rem !important; }

    /* Bigger stat boxes */
    .stat-box h2 {
      font-size: 2.4rem !important;
      margin: 0 0 6px 0 !important;
    }
    .stat-box p { font-size: 1.05rem !important; }

    /* Charts: white background so axis labels are always visible */
    .shiny-plot-output, img.shiny-plot-output {
      background: #ffffff !important;
    }


    /* ==== DEFINITIVE READABLE FONT SIZES ==== */
    body, .content-wrapper {
      font-size: 17px !important;
      line-height: 1.6 !important;
    }
    table.dataTable tbody td,
    table.dataTable tbody th {
      font-size: 16px !important;
      padding: 10px 12px !important;
      font-weight: 500 !important;
      color: #0c1f17 !important;
    }
    table.dataTable thead th {
      font-size: 16px !important;
      padding: 12px !important;
      font-weight: 700 !important;
      color: #ffffff !important;
      background-color: #1a4d38 !important;
    }
    .stat-box h2 { font-size: 2.4rem !important; }
    .stat-box p { font-size: 1.1rem !important; font-weight: 600 !important; }
    h3 { font-size: 1.7rem !important; }
    h4 { font-size: 1.3rem !important; }
    p, li, label { font-size: 1.05rem !important; }
    .quick-card h4 { font-size: 1.35rem !important; }
    .quick-card p { font-size: 1rem !important; }
    .nav-tabs > li > a { font-size: 16px !important; padding: 12px 18px !important; }
    .btn { font-size: 16px !important; padding: 8px 20px !important; }
    input, select, textarea, .form-control { font-size: 16px !important; padding: 8px 12px !important; }
    .selectize-input { font-size: 16px !important; padding: 8px 12px !important; }
    /* Heatmap & image plots: white background */
    .shiny-plot-output, img.shiny-plot-output {
      background: #ffffff !important;
    }


    /* ===== BIGGER MASTHEAD ===== */
    .masthead {
      padding: 2.4rem 2.6rem 1.8rem 2.6rem !important;
    }
    .masthead h1 {
      font-size: 2.6rem !important;
      letter-spacing: -0.02em !important;
      font-weight: 800 !important;
    }
    .masthead h1 .logo-leaf {
      font-size: 2.2rem !important;
    }
    .masthead .subtitle {
      font-size: 1.15rem !important;
      margin-top: 10px !important;
    }
    .masthead .meta-row {
      gap: 12px !important;
      margin-top: 20px !important;
    }
    .meta-badge {
      font-size: 0.95rem !important;
      padding: 6px 18px !important;
      font-weight: 700 !important;
    }
    .meta-badge .val {
      font-size: 1rem !important;
      font-weight: 800 !important;
    }
    .cite-btn, .license-btn {
      font-size: 1rem !important;
      padding: 10px 24px !important;
      font-weight: 800 !important;
    }
    .license-btn {
      margin-left: 10px !important;
    }

  "))),

  tags$a(href = "#main-content", class = "skip-link", "Skip to main content"),

  div(class = "main-wrapper",
    div(class = "masthead",
      div(class = "masthead-inner",
        h1(
          span(class = "logo-photo", tags$img(src = "alfalfa_logo.jpg", alt = "Alfalfa")),
          "Alfalfa Multi-Omics Pan-Genome Database"
        ),




        div(class = "meta-row",
          span(class = "meta-badge", shiny::icon("code-branch"), "Version", span(class = "val", "2.6")),
          span(class = "meta-badge", shiny::icon("calendar"), "Updated", span(class = "val", as.character(Sys.Date()))),
          span(class = "meta-badge", shiny::icon("globe"), "Species", span(class = "val", "6")),
          span(class = "meta-badge", shiny::icon("dna"), "Genes", span(class = "val", "823,838")),
          span(class = "meta-badge", shiny::icon("microscope"), "Annotations", span(class = "val", "13M+")),
          span(class = "meta-badge", shiny::icon("cut"), "CRISPR guides", span(class = "val", "3.5M")),
          actionButton("cite_btn",
                       label = tagList(shiny::icon("quote-left"), "Cite"),
                       class = "cite-btn"),
          actionButton("license_btn",
                       label = tagList(shiny::icon("scale-balanced"), "License & Usage"),
                       class = "license-btn")
        )
      )
    ),

    div(class = "container-fluid", id = "main-content",
      tabsetPanel(id = "tabs",

        tabPanel("Home",
          br(),
          fluidRow(
            column(3, div(class = "stat-box", h2(uiOutput("n_acc_animated")),  p("Accessions"))),
            column(3, div(class = "stat-box", h2(uiOutput("n_spec_animated")), p("Species"))),
            column(3, div(class = "stat-box", h2(uiOutput("n_genes_animated")),p("Genes"))),
            column(3, div(class = "stat-box", h2(uiOutput("n_clu_animated")),  p("Pan-Gene Clusters")))
          ),
          fluidRow(
            column(3, div(class = "stat-box", h2(uiOutput("n_go_animated")),   p("GO annotations"))),
            column(3, div(class = "stat-box", h2(uiOutput("n_ko_animated")),   p("KEGG KO"))),
            column(3, div(class = "stat-box", h2(uiOutput("n_path_animated")), p("KEGG pathways"))),
            column(3, div(class = "stat-box", h2(uiOutput("n_kog_animated")),  p("KOG categories")))
          ),
          fluidRow(
            column(3, div(class = "stat-box", h2(uiOutput("n_crispr_animated")), p("CRISPR guides"))),
            column(3, div(class = "stat-box", h2(uiOutput("n_crispr_genes_animated")), p("Genes with guides"))),
            column(6, div(class = "stat-box", h2("16"), p("Interactive tabs")))
          ),
          br(),
          h3("\U0001F4CA Figure 1: Pan-Genome Overview"),
          helpText("Summary of the 12-accession Medicago multi-omics pan-genome."),
          div(style = "background:white; padding:20px; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.05); overflow-x:auto;",
            plotOutput("hero_figure", height = "820px")
          ),
          br(),
          downloadButton("dl_hero_figure", "Download Figure 1 as PNG",
                         style = "background:#1a4d38; color:white; font-weight:800; padding:8px 20px; border-radius:8px; border:none;"),
          br(), br(),
          h3("\U0001F680 Quick Actions"),
          br(),
          fluidRow(
            column(3, div(class = "quick-card",
              onclick = "Shiny.setInputValue('nav_to','core_clusters',{priority:'event'})",
              h4(shiny::icon("dna"), "Core Genes"),
              p("Explore the 561 core pan-gene families."))),
            column(3, div(class = "quick-card",
              onclick = "Shiny.setInputValue('nav_to','go',{priority:'event'})",
              h4(shiny::icon("sitemap"), "GO Browser"),
              p("Search genes by Gene Ontology terms."))),
            column(3, div(class = "quick-card",
              onclick = "Shiny.setInputValue('nav_to','crispr',{priority:'event'})",
              h4(shiny::icon("cut"), "CRISPR Guides"),
              p("3.5M guide RNAs across 518,858 genes."))),
            column(3, div(class = "quick-card",
              onclick = "Shiny.setInputValue('nav_to','ai',{priority:'event'})",
              h4(shiny::icon("robot"), "AI Assistant"),
              p("Ask questions in plain English.")))
          )
        ),

        tabPanel("Accessions", br(), h3("\U0001F331 The 12 Medicago Accessions"), DTOutput("accessions_table")),

        tabPanel("Pan-Gene Clusters",
          br(), h3("\U0001F9EC Pan-Gene Clusters"),
          selectInput("cluster_type_filter", "Filter by type:",
                      choices = c("All", "core", "soft_core", "dispensable", "private", "singleton")),
          helpText("\U0001F4A1 Click any row to see presence/absence matrix and member genes."),
          DTOutput("clusters_table")),

        tabPanel("Top Gene Families",
          br(), h3("\U0001F9EC Top 20 Largest Pan-Gene Families"),
          helpText("\U0001F4A1 Click any row to see its members and presence/absence matrix."),
          DTOutput("top_families_table")),

        tabPanel("Presence/Absence",
          br(), h3("\U0001F4CB Gene Presence / Absence"),
          selectInput("pa_accession", "Select accession:", choices = NULL),
          DTOutput("pa_table")),

        tabPanel("Search",
          br(), h3("\U0001F50D Search Genes"),
          textInput("gene_query", "Search gene name (partial match):", placeholder = "e.g. Mara000007"),
          helpText("\U0001F4A1 Click any row to see gene details and pan-gene cluster members."),
          DTOutput("search_table")),

        tabPanel("Download",
          br(), h3("\U0001F4E5 Download Database Tables"),
          br(),
          downloadButton("dl_genes",      "Genes (CSV)"),
          downloadButton("dl_clusters",   "Pan-Gene Clusters (CSV)"),
          downloadButton("dl_accessions", "Accessions (CSV)"),
          downloadButton("dl_presence",   "Presence/Absence (CSV)"),
          br(), br(),
          downloadButton("dl_go",   "GO Annotations (CSV)"),
          downloadButton("dl_kegg", "KEGG KO (CSV)"),
          downloadButton("dl_path", "KEGG Pathways (CSV)"),
          downloadButton("dl_kog",  "KOG (CSV)"),
          br(), br(),
          downloadButton("dl_crispr", "CRISPR Guides (CSV)")),

        tabPanel("GO Browser",
          br(), h3("\U0001F9EC Gene Ontology Browser"),
          br(),
          fluidRow(
            column(6, textInput("go_query", "Search by GO term or gene name:",
                                placeholder = "e.g. GO:0006952 or Mara000007")),
            column(6, selectInput("go_accession", "Filter by accession:",
                                  choices = c("All"), selected = "All"))
          ),
          br(), h4("GO Term Distribution"), plotOutput("go_distribution", height = "320px"),
          br(), h4("Matching Annotations"), DTOutput("go_table")),

        tabPanel("KEGG Pathways",
          br(), h3("\U0001F9EC KEGG Pathway Browser"),
          br(),
          fluidRow(
            column(6, textInput("kegg_query", "Search by KO number or gene name:",
                                placeholder = "e.g. K15285 or Mara000006")),
            column(6, selectInput("kegg_accession", "Filter by accession:",
                                  choices = c("All"), selected = "All"))
          ),
          br(),
          fluidRow(
            column(6, h4("KO Number Distribution"), plotOutput("ko_distribution", height = "320px")),
            column(6, h4("Pathway Distribution (top 20)"), plotOutput("path_distribution", height = "320px"))
          ),
          br(), h4("Matching Annotations"), DTOutput("kegg_table")),

        tabPanel("KOG Classes",
          br(), h3("\U0001F9EC KOG / COG Functional Categories"),
          br(),
          fluidRow(
            column(6, selectInput("kog_accession", "Filter by accession:",
                                  choices = c("All"), selected = "All")),
            column(6, br(), br(), h4("Total genes with KOG:", textOutput("kog_total", inline = TRUE)))
          ),
          br(), h4("KOG Category Distribution"), plotOutput("kog_chart", height = "500px"),
          br(), h4("KOG Annotations Table"), DTOutput("kog_table")),

        tabPanel("Annotation Search",
          br(), h3("\U0001F50D Unified Annotation Search"),
          br(),
          textInput("unified_query", "Enter a gene name or ID:",
                    placeholder = "e.g. Mara000007", width = "400px"),
          br(), h4("Gene Info"), DTOutput("unified_gene"),
          br(), h4("GO Annotations"), DTOutput("unified_go"),
          br(), h4("KEGG KO + Pathways"), DTOutput("unified_kegg"),
          br(), h4("KOG Category"), DTOutput("unified_kog")),

        tabPanel("CRISPR Guides",
          br(), h3("\U0001F9EC CRISPR Guide RNA Browser"),
          helpText("3.5 million guide RNAs targeting 518,858 genes across 12 accessions."),
          br(),
          fluidRow(
            column(4, textInput("crispr_query", "Search by gene name or guide sequence:",
                                placeholder = "e.g. Mara000001 or GTTCAACCTGT")),
            column(4, selectInput("crispr_accession", "Filter by accession:",
                                  choices = c("All"), selected = "All")),
            column(4, selectInput("crispr_uniqueness", "Filter by uniqueness:",
                                  choices = c("All", "UNIQUE", "MULTI"), selected = "All"))
          ),
          br(), h4("Matching Guides"), DTOutput("crispr_table")),

        tabPanel("AI Assistant",
          br(), h3("\U0001F916 Ask the Database"),
          helpText("Ask a question in plain English. The AI generates SQL, runs it, and shows the results."),
          br(),
          div(style = "max-width: 900px;",
            div(class = "quick-card", style = "cursor:default;",
              h4(shiny::icon("lightbulb"), "Example questions:"),
              tags$ul(
                tags$li("\"How many core clusters are there?\""),
                tags$li("\"Show me the top 10 largest pan-gene families\""),
                tags$li("\"Which GO terms are most common in M_arabica?\""),
                tags$li("\"Show me the top 10 CRISPR guides for Mara000001\""),
                tags$li("\"How many UNIQUE CRISPR guides are there?\"")
              )
            )
          ),
          br(),
          div(style = "max-width: 900px;",
            textAreaInput("ai_question", NULL,
                          placeholder = "Type your question here...",
                          rows = 2, width = "100%"),
            actionButton("ai_ask", label = tagList(shiny::icon("paper-plane"), "Ask AI"),
                         style = "background:#f5b342; color:#08261a; font-weight:800; border:2px solid #d4942e; border-radius:8px; padding:8px 20px;"),
            actionButton("ai_clear", label = tagList(shiny::icon("trash"), "Clear"),
                         style = "background:#e0e8e4; color:#08261a; font-weight:700; border:2px solid #c8d5cd; border-radius:8px; padding:8px 20px; margin-left:8px;")
          ),
          br(), uiOutput("ai_conversation")),

        tabPanel("Statistics",
          br(), h3("\U0001F4CA Pan-Genome Statistics"), br(),
          fluidRow(
            column(6, div(class = "stat-box", h4("Genes per Accession"),
                          plotOutput("stat_genes_per_acc", height = "420px"))),
            column(6, div(class = "stat-box", h4("GC Content per Accession"),
                          plotOutput("stat_gc", height = "420px")))
          ),
          fluidRow(
            column(6, div(class = "stat-box", h4("Genes per Chromosome (stacked)"),
                          plotOutput("stat_genes_per_chrom", height = "440px"))),
            column(6, div(class = "stat-box", h4("Cluster Type Distribution"),
                          plotOutput("stat_cluster_types", height = "420px")))
          ),
          fluidRow(
            column(12, div(class = "stat-box", h4("Contig Length Distribution"),
                           plotOutput("stat_contig_len", height = "340px")))
          )),

        tabPanel("Assembly Quality",
          br(), h3("\U0001F4CA Assembly Quality Summary"), br(),
          fluidRow(
            column(4, div(class = "stat-box", h4("Genome Size (Mb)"),
                          plotOutput("qc_genome_size", height = "420px"))),
            column(4, div(class = "stat-box", h4("N50 (Mb)"),
                          plotOutput("qc_n50", height = "420px"))),
            column(4, div(class = "stat-box", h4("GC Content (%)"),
                          plotOutput("qc_gc", height = "420px")))
          ),
          br(), h4("Detailed Assembly Metrics"), DTOutput("qc_table")),

        tabPanel("Species Comparison",
          br(), h3("\U0001F30D Cross-Accession Cluster Sharing"),
          br(), div(style = "overflow-x:auto;", plotOutput("species_matrix_plot", height = "700px")),
          br(), h4("Shared Cluster Counts (Table View)"), DTOutput("species_matrix_table")),

        tabPanel("Heatmap",
          br(), h3("\U0001F7E9 Presence/Absence Heatmap (Top 100 Clusters)"),
          br(),
          selectInput("heatmap_type", "Filter clusters by type:",
                      choices = c("core", "soft_core", "dispensable", "All"),
                      selected = "core"),
          div(style = "overflow-x:auto;", plotOutput("presence_heatmap", height = "900px")))
      )
    )
  ),

  div(class = "footer",
    div(class = "container-fluid",
      fluidRow(
        column(4,
          h4(shiny::icon("leaf"), " Alfalfa Multi-Omics Pan-Genome Database"),
          p("A comprehensive multi-omics resource for the Medicago genus."),
          p(style = "margin-top:12px;",
            span(class = "license-tag", shiny::icon("code"), " MIT (code)"),
            span(class = "license-tag", shiny::icon("creative-commons"), " CC-BY 4.0 (data)")),
          p(style = "color:#a6c1b0; margin-top:10px; font-size:0.8rem; font-weight:500;",
            shiny::icon("database"), " Powered by PostgreSQL + R Shiny + Groq AI")
        ),
        column(2, h4("Data"), tags$ul(
          tags$li("12 Accessions"), tags$li("823,838 Genes"),
          tags$li("217,122 Clusters"), tags$li("13M+ Annotations"))),
        column(2, h4("Analysis"), tags$ul(
          tags$li("Pan-Genome"), tags$li("GO / KEGG / KOG"),
          tags$li("CRISPR Guides"), tags$li("AI Assistant"))),
        column(2, h4("Resources"), tags$ul(
          tags$li(tags$a(href = "https://github.com/amomboerick/alfalfa-multi-omics-pangenome",
                         target = "_blank", shiny::icon("book"), " Documentation")),
          tags$li(tags$a(href = "https://github.com/amomboerick/alfalfa-multi-omics-pangenome",
                         target = "_blank", shiny::icon("file-code"), " Source Code")),
          tags$li(tags$a(href = "https://github.com/amomboerick/alfalfa-multi-omics-pangenome",
                         target = "_blank", shiny::icon("github"), " GitHub")),
          tags$li(tags$a(href = "https://github.com/amomboerick/alfalfa-multi-omics-pangenome/archive/refs/heads/main.zip",
                         target = "_blank", shiny::icon("download"), " Downloads")))),
        column(2, h4("About"), tags$ul(
          tags$li(tags$a(href = "#", "Consortium")),
          tags$li(tags$a(href = "#", "Contact")),
          tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('cite_btn', Math.random(), {priority:'event'})", "Citation")),
          tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('license_btn', Math.random(), {priority:'event'})", "License"))))
      ),
      div(class = "footer-bottom",
        HTML(paste0(
          "\u00A9 2026 <strong>Alfalfa Multi-Omics Pan-Genome Database Consortium</strong> \u00B7 ",
          "Version 2.6 \u00B7 Last updated: ", Sys.Date(),
          " \u00B7 Licensed under <strong>MIT (code)</strong> and <strong>CC-BY 4.0 (data)</strong>"
        ))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  shared_con <- NULL
  session$onSessionEnded(function() {
    if (!is.null(shared_con) && dbIsValid(shared_con)) dbDisconnect(shared_con)
  })

  observeEvent(input$cite_btn, {
    showModal(modalDialog(
      title = tagList(shiny::icon("quote-left"), " Cite the Alfalfa Multi-Omics Pan-Genome Database"),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      HTML(paste0(
        "<h4 style='color:#0B3B2C; margin-bottom:14px; font-weight:800;'>Plain Text Citation</h4>",
        "<div style='background:#f7faf5; padding:16px; border-left:6px solid #F5B342;",
        "     border-radius:8px; font-family:monospace; font-size:0.9rem; line-height:1.6; color:#0c1f17;'>",
        "Alfalfa Multi-Omics Pan-Genome Database Consortium (2026). ",
        "<i>A comprehensive multi-omics pan-genome resource for the Medicago genus.</i> ",
        "Version 2.6. Available at: https://github.com/amomboerick/alfalfa-multi-omics-pangenome",
        "</div>",
        "<h4 style='color:#0B3B2C; margin-top:24px; margin-bottom:14px; font-weight:800;'>BibTeX</h4>",
        "<div style='background:#0B3B2C; color:#b8e3b0; padding:16px; border-radius:8px;",
        "     font-family:monospace; font-size:0.85rem; white-space:pre; overflow-x:auto;'>",
        "@misc{alfalfa_pangenome_2026,\n",
        "  title        = {Alfalfa Multi-Omics Pan-Genome Database},\n",
        "  author       = {{Alfalfa Multi-Omics Pan-Genome Database Consortium}},\n",
        "  year         = {2026},\n",
        "  version      = {2.6},\n",
        "  howpublished = {\\url{https://github.com/amomboerick/alfalfa-multi-omics-pangenome}},\n",
        "  note         = {Licensed under MIT and CC-BY 4.0}\n",
        "}",
        "</div>",
        "<h4 style='color:#0B3B2C; margin-top:24px; margin-bottom:14px; font-weight:800;'>Contact</h4>",
        "<p style='font-size:0.9rem; color:#0c1f17; font-weight:500;'>For questions: <b>Erick Amombo</b> \u2014 amomboeric@gmail.com</p>"
      ))
    ))
  })

  observeEvent(input$license_btn, {
    showModal(modalDialog(
      title = tagList(shiny::icon("scale-balanced"), " License & Usage Terms"),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      HTML(paste0(
        "<div style='display:flex;flex-wrap:wrap;gap:12px;margin-bottom:20px;'>",
        "<span class='license-badge license-mit'><i class='fas fa-code'></i> MIT License \u2014 Source Code</span>",
        "<span class='license-badge license-cc'><i class='fas fa-database'></i> CC-BY 4.0 \u2014 Data</span>",
        "</div>",
        "<p style='color:#0c1f17;font-weight:500;line-height:1.65;font-size:0.95rem;'>",
        "This resource is released under a dual license. The <strong>source code</strong> is licensed under the ",
        "<strong>MIT License</strong>. The <strong>data</strong> is licensed under <strong>CC-BY 4.0</strong>.",
        "</p>",
        "<div class='license-box'><h5><i class='fas fa-code' style='color:#1a4d38;'></i> MIT License (Source Code)</h5>",
        "<p><strong>You are free to:</strong></p><ul>",
        "<li>Use, copy, modify, and distribute the code</li>",
        "<li>Use it commercially</li>",
        "<li>Sublicense and merge with other software</li></ul>",
        "<p><strong>Under the conditions that:</strong></p><ul>",
        "<li>The original copyright notice is included</li></ul></div>",
        "<div class='license-box'><h5><i class='fas fa-database' style='color:#d48c1a;'></i> CC-BY 4.0 (Data)</h5>",
        "<p><strong>You are free to:</strong></p><ul>",
        "<li>Share \u2014 copy and redistribute in any medium</li>",
        "<li>Adapt \u2014 remix, transform, and build upon for any purpose, even commercially</li></ul>",
        "<p><strong>Under the condition that:</strong></p><ul>",
        "<li><strong>Attribution</strong> \u2014 You must give appropriate credit and indicate if changes were made.</li></ul></div>",
        "<div class='license-box'><h5><i class='fas fa-server' style='color:#8b5e9b;'></i> Data Availability</h5><ul>",
        "<li><strong>Source code:</strong> <a href='https://github.com/amomboerick/alfalfa-multi-omics-pangenome' target='_blank' style='color:#1a4d38;font-weight:700;'>GitHub repository</a></li>",
        "<li><strong>Database dump:</strong> Available upon reasonable request</li>",
        "<li><strong>Annotation files:</strong> Via Download tab or on request</li></ul></div>",
        "<p style='color:#5a6b62;font-size:0.85rem;margin-top:20px;font-style:italic;'>",
        "<i class='fas fa-circle-info'></i> Provided \"as is\", without warranty of any kind.</p>"
      ))
    ))
  })

  make_counter_ui <- function(numeric_value) {
    formatted <- format(numeric_value, big.mark = ",", scientific = FALSE)
    tags$span(class = "counter-value",
              `data-animate-counter` = "true",
              `data-target` = numeric_value,
              formatted)
  }

  output$n_acc_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM accessions")$n)
    make_counter_ui(n)
  })
  output$n_spec_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(DISTINCT species)::text AS n FROM accessions")$n)
    make_counter_ui(n)
  })
  output$n_genes_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM genes")$n)
    make_counter_ui(n)
  })
  output$n_clu_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM pan_gene_clusters")$n)
    make_counter_ui(n)
  })
  output$n_go_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM go_annotations")$n)
    make_counter_ui(n)
  })
  output$n_ko_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM kegg_ko")$n)
    make_counter_ui(n)
  })
  output$n_path_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM kegg_pathways")$n)
    make_counter_ui(n)
  })
  output$n_kog_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM kog_annotations")$n)
    make_counter_ui(n)
  })
  output$n_crispr_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(*)::text AS n FROM crispr_guides")$n)
    make_counter_ui(n)
  })
  output$n_crispr_genes_animated <- renderUI({
    con <- connect_db(); on.exit(dbDisconnect(con))
    n <- as.numeric(dbGetQuery(con, "SELECT COUNT(DISTINCT gene_id)::text AS n FROM crispr_guides")$n)
    make_counter_ui(n)
  })

  observeEvent(input$nav_to, {
    t <- input$nav_to
    if (t == "core_clusters") {
      updateTabsetPanel(session, "tabs", selected = "Pan-Gene Clusters")
      updateSelectInput(session, "cluster_type_filter", selected = "core")
    } else if (t == "top_families") {
      updateTabsetPanel(session, "tabs", selected = "Top Gene Families")
    } else if (t == "heatmap") {
      updateTabsetPanel(session, "tabs", selected = "Heatmap")
    } else if (t == "ai") {
      updateTabsetPanel(session, "tabs", selected = "AI Assistant")
    } else if (t == "go") {
      updateTabsetPanel(session, "tabs", selected = "GO Browser")
    } else if (t == "kegg") {
      updateTabsetPanel(session, "tabs", selected = "KEGG Pathways")
    } else if (t == "crispr") {
      updateTabsetPanel(session, "tabs", selected = "CRISPR Guides")
    }
  })

  show_cluster_detail <- function(cluster_id, cluster_name, cluster_type,
                                  gene_count, presence) {
    con <- connect_db(); on.exit(dbDisconnect(con))

    memb <- tryCatch(dbGetQuery(con, sprintf("
      SELECT a.accession_name, a.species, gm.gene_name,
             gm.start_pos, gm.end_pos, gm.strand
      FROM cluster_membership cm
      JOIN genes gm ON cm.gene_id = gm.gene_id
      JOIN accessions a ON cm.accession_id = a.accession_id
      WHERE cm.cluster_id = %d
      ORDER BY a.accession_name, gm.start_pos
    ", cluster_id)), error = function(e) data.frame())

    all_acc <- dbGetQuery(con, "SELECT accession_name, species FROM accessions ORDER BY species, accession_name")
    badge_cls <- paste0("badge badge-", cluster_type)

    matrix_html <- paste0(
      "<h4 style='color:#1a4d38; margin-top:20px; font-weight:800;'>Presence / Absence Matrix</h4>",
      "<table style='width:100%; border-collapse:collapse; font-size:0.9rem;'>",
      "<tr style='background:#1a4d38; color:white;'>",
      "<th style='padding:8px; text-align:left;'>Accession</th>",
      "<th style='padding:8px; text-align:left;'>Species</th>",
      "<th style='padding:8px; text-align:center;'>Status</th>",
      "<th style='padding:8px; text-align:right;'>Copies</th></tr>")
    for (i in seq_len(nrow(all_acc))) {
      acc_name <- all_acc$accession_name[i]
      is_present <- acc_name %in% memb$accession_name
      copies <- if (is_present) sum(memb$accession_name == acc_name) else 0
      status_cell <- if (is_present) {
        "<span style='background:#1a4d38; color:white; padding:3px 12px; border-radius:20px; font-weight:700; font-size:0.8rem;'>\u2713 Present</span>"
      } else {
        "<span style='background:#d0d8d4; color:#333; padding:3px 12px; border-radius:20px; font-weight:700; font-size:0.8rem;'>\u2717 Absent</span>"
      }
      matrix_html <- paste0(matrix_html,
        "<tr style='border-bottom:1px solid #eef2f0;'>",
        "<td style='padding:6px 8px; font-weight:600;'>", acc_name, "</td>",
        "<td style='padding:6px 8px;'><i>", all_acc$species[i], "</i></td>",
        "<td style='padding:6px 8px; text-align:center;'>", status_cell, "</td>",
        "<td style='padding:6px 8px; text-align:right; font-weight:700;'>", copies, "</td></tr>")
    }
    matrix_html <- paste0(matrix_html, "</table>")

    members_html <- ""
    if (nrow(memb) > 0) {
      members_html <- paste0(
        "<h4 style='color:#1a4d38; margin-top:20px; font-weight:800;'>Member Genes (",
        nrow(memb), ")</h4>",
        "<table style='width:100%; border-collapse:collapse; font-size:0.9rem;'>",
        "<tr style='background:#1a4d38; color:white;'>",
        "<th style='padding:8px; text-align:left;'>Accession</th>",
        "<th style='padding:8px; text-align:left;'>Gene</th>",
        "<th style='padding:8px; text-align:left;'>Position</th>",
        "<th style='padding:8px; text-align:left;'>Strand</th></tr>")
      for (i in seq_len(nrow(memb))) {
        members_html <- paste0(members_html,
          "<tr style='border-bottom:1px solid #eef2f0;'>",
          "<td style='padding:6px 8px; font-weight:600;'>", memb$accession_name[i], "</td>",
          "<td style='padding:6px 8px; font-weight:600; color:#1a4d38;'>", memb$gene_name[i], "</td>",
          "<td style='padding:6px 8px;'>", memb$start_pos[i], " \u2013 ", memb$end_pos[i], "</td>",
          "<td style='padding:6px 8px;'>", memb$strand[i], "</td></tr>")
      }
      members_html <- paste0(members_html, "</table>")
    } else {
      members_html <- "<p style='color:#666;'>No member genes found.</p>"
    }

    showModal(modalDialog(
      title = paste0("\U0001F9EC Cluster: ", cluster_name),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      HTML(paste0(
        "<div style='background:#f7faf5; padding:16px; border-left:6px solid #f5b342; border-radius:8px;'>",
        "<table style='width:100%; font-size:0.95rem;'>",
        "<tr><td style='width:180px; font-weight:700;'>Cluster Name:</td><td><b>", cluster_name, "</b></td></tr>",
        "<tr><td style='font-weight:700;'>Cluster Type:</td><td><span class='", badge_cls, "'>", cluster_type, "</span></td></tr>",
        "<tr><td style='font-weight:700;'>Family Size:</td><td><b>", gene_count, "</b> genes</td></tr>",
        "<tr><td style='font-weight:700;'>Present in:</td><td><b>", presence, "</b> / 12 accessions</td></tr>",
        "</table></div>",
        matrix_html, members_html
      ))
    ))
  }

  show_gene_detail <- function(g) {
    con <- connect_db(); on.exit(dbDisconnect(con))

    clu <- tryCatch(dbGetQuery(con, sprintf("
      SELECT pc.cluster_id, pc.cluster_name, pc.cluster_type, pc.gene_count,
             pc.presence_across_accessions
      FROM cluster_membership cm
      JOIN pan_gene_clusters pc ON cm.cluster_id = pc.cluster_id
      WHERE cm.gene_id = %d LIMIT 1
    ", g$gene_id)), error = function(e) data.frame())

    members_html <- "<p style='color:#666;'>This gene is not assigned to any pan-gene cluster.</p>"

    if (nrow(clu) > 0) {
      memb <- tryCatch(dbGetQuery(con, sprintf("
        SELECT a.accession_name, a.species, gm.gene_name,
               gm.start_pos, gm.end_pos, gm.strand
        FROM cluster_membership cm
        JOIN genes gm ON cm.gene_id = gm.gene_id
        JOIN accessions a ON cm.accession_id = a.accession_id
        WHERE cm.cluster_id = %d ORDER BY a.accession_name
      ", clu$cluster_id[1])), error = function(e) data.frame())

      badge_cls <- paste0("badge badge-", clu$cluster_type[1])
      members_html <- paste0(
        "<h4 style='color:#1a4d38; margin-top:20px; font-weight:800;'>Cluster: ",
        clu$cluster_name[1], " <span class='", badge_cls, "'>",
        clu$cluster_type[1], "</span></h4>",
        "<p style='font-weight:600;'><b>Family size:</b> ", clu$gene_count[1], " genes \u00b7 ",
        "<b>Present in:</b> ", clu$presence_across_accessions[1], " / 12</p>",
        "<table style='width:100%; border-collapse:collapse; margin-top:12px; font-size:0.9rem;'>",
        "<tr style='background:#1a4d38; color:white;'>",
        "<th style='padding:8px; text-align:left;'>Accession</th>",
        "<th style='padding:8px; text-align:left;'>Species</th>",
        "<th style='padding:8px; text-align:left;'>Gene</th>",
        "<th style='padding:8px; text-align:left;'>Position</th>",
        "<th style='padding:8px; text-align:left;'>Strand</th></tr>")
      if (nrow(memb) > 0) {
        for (i in seq_len(nrow(memb))) {
          members_html <- paste0(members_html,
            "<tr style='border-bottom:1px solid #eef2f0;'>",
            "<td style='padding:6px 8px; font-weight:600;'>", memb$accession_name[i], "</td>",
            "<td style='padding:6px 8px;'><i>", memb$species[i], "</i></td>",
            "<td style='padding:6px 8px; font-weight:600; color:#1a4d38;'>", memb$gene_name[i], "</td>",
            "<td style='padding:6px 8px;'>", memb$start_pos[i], " \u2013 ", memb$end_pos[i], "</td>",
            "<td style='padding:6px 8px;'>", memb$strand[i], "</td></tr>")
        }
      }
      members_html <- paste0(members_html, "</table>")
    }

    showModal(modalDialog(
      title = paste0("\U0001F9EC Gene: ", g$gene_name),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      HTML(paste0(
        "<div style='background:#f7faf5; padding:16px; border-left:6px solid #f5b342; border-radius:8px;'>",
        "<table style='width:100%; font-size:0.95rem;'>",
        "<tr><td style='width:130px; font-weight:700;'>Gene:</td><td><b>", g$gene_name, "</b></td></tr>",
        "<tr><td style='font-weight:700;'>Accession:</td><td><b>", g$accession_name, "</b></td></tr>",
        "<tr><td style='font-weight:700;'>Species:</td><td><i>", g$species, "</i></td></tr>",
        "<tr><td style='font-weight:700;'>Contig:</td><td>", g$contig_name, "</td></tr>",
        "<tr><td style='font-weight:700;'>Position:</td><td>", g$start_pos, " \u2013 ", g$end_pos, "</td></tr>",
        "<tr><td style='font-weight:700;'>Strand:</td><td>", g$strand, "</td></tr>",
        "</table></div>",
        members_html
      ))
    ))
  }

  build_hero <- function() {
    con <- connect_db(); on.exit(dbDisconnect(con), add = TRUE)
    dfA <- dbGetQuery(con, "SELECT cluster_type, COUNT(*)::text AS n FROM pan_gene_clusters GROUP BY cluster_type ORDER BY COUNT(*) DESC"); dfA$n <- as.numeric(dfA$n)
    dfB <- dbGetQuery(con, "SELECT accession_name, species, gene_count::text AS n FROM accessions ORDER BY gene_count DESC"); dfB$n <- as.numeric(dfB$n)
    dfC <- dbGetQuery(con, "SELECT c.contig_name, a.accession_name, COUNT(*)::text AS n FROM genes g JOIN contigs c ON g.contig_id=c.contig_id JOIN accessions a ON g.accession_id=a.accession_id GROUP BY c.contig_name, a.accession_name"); dfC$n <- as.numeric(dfC$n)
    clu <- dbGetQuery(con, "SELECT cluster_id, cluster_name FROM pan_gene_clusters WHERE cluster_type='core' ORDER BY gene_count DESC LIMIT 20")
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    pa  <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present=TRUE")
    acc_order <- acc$accession_id
    mat <- matrix(0, nrow=nrow(clu), ncol=length(acc_order))
    for (i in seq_len(nrow(pa))) {
      r <- which(clu$cluster_id == pa$cluster_id[i]); cc <- which(acc_order == pa$accession_id[i])
      if (length(r)==1 && length(cc)==1) mat[r,cc] <- 1
    }
    par(mfrow=c(2,2), mar=c(5,13,4,2), oma=c(0,0,4,0),
        col.axis=FONT_COLOR, col.lab=FONT_COLOR, col.main=FONT_COLOR,
        fg=FONT_COLOR, cex.axis=AXIS_CEX, font.axis=FONT_BOLD,
        font.lab=FONT_BOLD, font.main=FONT_BOLD, cex.main=TITLE_CEX)
    palA <- c("#1a4d38","#f5b342","#d48c1a","#8b5e9b","#2b7a5e")
    labelsA <- paste0(dfA$cluster_type, " (", format(dfA$n, big.mark=","), ")")
    pie(dfA$n, labels=labelsA, col=palA[seq_len(nrow(dfA))],
        main="A. Cluster type distribution", cex=0.95, border="white",
        col.main=FONT_COLOR, font.main=FONT_BOLD, cex.main=TITLE_CEX)
    species_colors <- c("Medicago sativa"="#1a4d38","Medicago truncatula"="#f5b342",
                        "Medicago ruthenica"="#d48c1a","Medicago arabica"="#8b5e9b",
                        "Medicago polymorpha"="#2b7a5e","Medicago lupulina"="#5a9b7e")
    bc <- species_colors[dfB$species]; bc[is.na(bc)] <- "#8fa7b3"
    par(mar=c(5,13,4,2))
    barplot(dfB$n, names.arg=dfB$accession_name, las=1, horiz=TRUE, col=bc,
            border="white", main="B. Genes per accession", xlab="Gene count",
            cex.names=0.85, font=2, col.axis=FONT_COLOR, col.lab=FONT_COLOR,
            col.main=FONT_COLOR, font.axis=FONT_BOLD, font.lab=FONT_BOLD,
            font.main=FONT_BOLD, cex.axis=AXIS_CEX, cex.lab=LABEL_CEX, cex.main=TITLE_CEX)
    if (nrow(dfC) > 0) {
      tab <- xtabs(n ~ contig_name + accession_name, data=dfC); par(mar=c(5,6,4,2))
      barplot(t(tab), col=rainbow(ncol(tab)), border=NA,
              main="C. Genes per chromosome", xlab="Chromosome", ylab="Gene count",
              las=2, cex.names=0.65, font=2, col.axis=FONT_COLOR, col.lab=FONT_COLOR,
              col.main=FONT_COLOR, font.axis=FONT_BOLD, font.lab=FONT_BOLD,
              font.main=FONT_BOLD, cex.axis=AXIS_CEX, cex.lab=LABEL_CEX, cex.main=TITLE_CEX)
    }
    par(mar=c(6,13,4,2))
    plot(NA, xlim=c(0,ncol(mat)), ylim=c(0,nrow(mat)), xaxt="n", yaxt="n",
         xlab="", ylab="", main="D. Presence/absence of top 20 core clusters",
         bty="n", col.main=FONT_COLOR, font.main=FONT_BOLD, cex.main=TITLE_CEX)
    for (i in 1:nrow(mat)) for (j in 1:ncol(mat)) {
      rect(j-1, i-1, j, i, col=if(mat[i,j]==1) "#1a4d38" else "#e6eae8",
           border="white", lwd=0.6)
    }
    axis(2, at=(1:nrow(mat))-0.5, labels=clu$cluster_name, las=2,
         cex.axis=0.7, tick=FALSE, col.axis=FONT_COLOR, font.axis=FONT_BOLD)
    axis(1, at=(1:ncol(mat))-0.5, labels=acc$accession_name, las=2,
         cex.axis=0.75, tick=FALSE, col.axis=FONT_COLOR, font.axis=FONT_BOLD)
    mtext("Figure 1 - Alfalfa Multi-Omics Pan-Genome Overview",
          outer=TRUE, cex=1.35, font=2, col=FONT_COLOR, line=1)
  }

  output$hero_figure <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    dfA <- dbGetQuery(con, "SELECT cluster_type, COUNT(*)::text AS n FROM pan_gene_clusters GROUP BY cluster_type ORDER BY COUNT(*) DESC")
    dfA$n <- as.numeric(dfA$n)
    dfB <- dbGetQuery(con, "SELECT accession_name, species, gene_count::text AS n FROM accessions ORDER BY gene_count DESC")
    dfB$n <- as.numeric(dfB$n)
    dfC <- dbGetQuery(con, "SELECT c.contig_name, a.accession_name, COUNT(*)::text AS n FROM genes g JOIN contigs c ON g.contig_id=c.contig_id JOIN accessions a ON g.accession_id=a.accession_id GROUP BY c.contig_name, a.accession_name")
    dfC$n <- as.numeric(dfC$n)
    clu <- dbGetQuery(con, "SELECT cluster_id, cluster_name FROM pan_gene_clusters WHERE cluster_type = 'core' ORDER BY gene_count DESC LIMIT 20")
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    pa  <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present = TRUE")
    mat <- matrix(0, nrow = nrow(clu), ncol = nrow(acc))
    for (i in seq_len(nrow(pa))) {
      r <- which(clu$cluster_id == pa$cluster_id[i]); cc <- which(acc$accession_id == pa$accession_id[i])
      if (length(r) == 1 && length(cc) == 1) mat[r, cc] <- 1
    }

    par(mfrow = c(2, 2), mar = c(6, 4, 4, 2), oma = c(2, 2, 4, 2),
        col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
        fg = FONT_COLOR, cex.axis = 1.35, font.axis = FONT_BOLD,
        font.lab = FONT_BOLD, font.main = FONT_BOLD, cex.main = 1.5)

    # ---- A. Pie with legend (no slice labels) ----
    palA <- c("#1a4d38", "#f5b342", "#d48c1a", "#8b5e9b", "#2b7a5e")
    legend_labels_A <- paste0(dfA$cluster_type, " (", format(dfA$n, big.mark = ","), ")")
    par(mar = c(4, 4, 4, 2))
    pie(dfA$n, labels = NA, col = palA[seq_len(nrow(dfA))], border = "white",
        radius = 0.7, main = "A. Cluster type distribution",
        col.main = FONT_COLOR, font.main = FONT_BOLD, cex.main = 1.5)
    legend(x = 1.30, y = 0.5, legend = legend_labels_A, fill = palA[seq_len(nrow(dfA))],
           cex = 1.1, bg = "white", bty = "n", xpd = NA)

    # ---- B. Horizontal barplot: genes per accession ----
    species_colors <- c("Medicago sativa" = "#1a4d38", "Medicago truncatula" = "#f5b342",
                        "Medicago ruthenica" = "#d48c1a", "Medicago arabica" = "#8b5e9b",
                        "Medicago polymorpha" = "#2b7a5e", "Medicago lupulina" = "#5a9b7e")
    bc <- species_colors[dfB$species]; bc[is.na(bc)] <- "#8fa7b3"
    par(mar = c(5, 15, 4, 2))
    barplot(dfB$n, names.arg = dfB$accession_name, las = 1, horiz = TRUE, col = bc,
            border = "white", main = "B. Genes per accession", xlab = "Gene count",
            cex.names = 1.15, font = 2,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD,
            font.main = FONT_BOLD, cex.axis = 1.35, cex.lab = 1.4, cex.main = 1.5)

    # ---- C. Stacked barplot: colored by accession with big legend ----
    if (nrow(dfC) > 0) {
      tab <- xtabs(n ~ contig_name + accession_name, data = dfC)
      par(mar = c(5, 5, 4, 3))
      barplot(t(tab), col = rainbow(ncol(tab)), border = NA,
              main = "C. Genes per chromosome", xlab = "", ylab = "Gene count",
              las = 1, xaxt = "n",
              col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
              font.axis = FONT_BOLD, font.lab = FONT_BOLD,
              font.main = FONT_BOLD, cex.axis = 1.35, cex.lab = 1.4, cex.main = 1.5)
      legend(x = ncol(tab) + 1.5, y = max(rowSums(tab)), legend = colnames(tab), fill = rainbow(ncol(tab)),
             cex = 0.95, bg = "white", bty = "n", xpd = TRUE)
    }

    # ---- D. Presence/absence grid, larger labels ----
    par(mar = c(10, 12, 4, 2))
    plot(NA, xlim = c(0, ncol(mat)), ylim = c(0, nrow(mat)),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = "D. Presence/absence of top 20 core clusters",
         bty = "n", col.main = FONT_COLOR, font.main = FONT_BOLD, cex.main = 1.5)
    for (i in 1:nrow(mat)) for (j in 1:ncol(mat)) {
      rect(j-1, i-1, j, i, col = if (mat[i,j] == 1) "#1a4d38" else "#ffffff",
           border = "#cccccc", lwd = 0.5)
    }
    axis(2, at = (1:nrow(mat)) - 0.5, labels = clu$cluster_name, las = 2,
         cex.axis = 0.85, tick = FALSE, col.axis = FONT_COLOR, font.axis = FONT_BOLD,
         line = -0.5)
    axis(1, at = (1:ncol(mat)) - 0.5, labels = acc$accession_name, las = 2,
         cex.axis = 1.0, tick = FALSE, col.axis = FONT_COLOR, font.axis = FONT_BOLD,
         line = -0.5)

    mtext("Figure 1 - Alfalfa Multi-Omics Pan-Genome Overview",
          outer = TRUE, cex = 1.7, font = 2, col = FONT_COLOR, line = 1)
  }, height = 900)
  output$dl_hero_figure <- downloadHandler(
    filename = function() paste0("Figure1_pan_genome_overview_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 2000, height = 1800, res = 160)
      build_hero()
      dev.off()
    }
  )

  output$accessions_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "
      SELECT accession_id, accession_name, species, subspecies, cultivar,
             genome_size, gc_content, n50, contig_count, gene_count
      FROM accessions ORDER BY species, accession_name")
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 12, scrollX = TRUE,
                             dom = 'ftip', autoWidth = TRUE)) %>%
      formatRound(c("genome_size","gc_content","n50"), 2) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$clusters_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    type_filter <- input$cluster_type_filter
    q <- "SELECT cluster_id, cluster_name, cluster_type, gene_count,
                 presence_across_accessions
          FROM pan_gene_clusters"
    if (!is.null(type_filter) && type_filter != "All") {
      q <- paste0(q, " WHERE cluster_type = '", type_filter, "'")
    }
    q <- paste0(q, " ORDER BY gene_count DESC LIMIT 500")
    df <- dbGetQuery(con, q)
    datatable(df, rownames = FALSE, filter = "top", selection = "single",
              options = list(pageLength = 15, scrollX = TRUE,
                             dom = 'ftip', autoWidth = TRUE,
                             columnDefs = list(list(visible = FALSE, targets = 0)))) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  observeEvent(input$clusters_table_rows_selected, {
    row <- input$clusters_table_rows_selected
    if (length(row) == 0) return()
    con <- connect_db(); on.exit(dbDisconnect(con))
    type_filter <- input$cluster_type_filter
    q <- "SELECT cluster_id, cluster_name, cluster_type, gene_count,
                 presence_across_accessions
          FROM pan_gene_clusters"
    if (!is.null(type_filter) && type_filter != "All") {
      q <- paste0(q, " WHERE cluster_type = '", type_filter, "'")
    }
    q <- paste0(q, " ORDER BY gene_count DESC LIMIT 500")
    df <- dbGetQuery(con, q)
    r <- df[row, ]
    show_cluster_detail(r$cluster_id, r$cluster_name, r$cluster_type,
                        r$gene_count, r$presence_across_accessions)
  })

  output$top_families_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "
      SELECT cluster_id, cluster_name, cluster_type, gene_count,
             presence_across_accessions
      FROM pan_gene_clusters ORDER BY gene_count DESC LIMIT 20")
    datatable(df, rownames = FALSE, selection = "single",
              options = list(pageLength = 20, scrollX = TRUE,
                             dom = 'ftip', autoWidth = TRUE,
                             columnDefs = list(list(visible = FALSE, targets = 0)))) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  observeEvent(input$top_families_table_rows_selected, {
    row <- input$top_families_table_rows_selected
    if (length(row) == 0) return()
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "
      SELECT cluster_id, cluster_name, cluster_type, gene_count,
             presence_across_accessions
      FROM pan_gene_clusters ORDER BY gene_count DESC LIMIT 20")
    r <- df[row, ]
    show_cluster_detail(r$cluster_id, r$cluster_name, r$cluster_type,
                        r$gene_count, r$presence_across_accessions)
  })

  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")
    updateSelectInput(session, "pa_accession", choices = acc$accession_name)
  })

  output$pa_table <- renderDT({
    req(input$pa_accession)
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, sprintf("
      SELECT pc.cluster_name, pc.cluster_type, gpa.is_present, gpa.copy_number
      FROM gene_presence_absence gpa
      JOIN pan_gene_clusters pc ON gpa.cluster_id = pc.cluster_id
      JOIN accessions a ON gpa.accession_id = a.accession_id
      WHERE a.accession_name = '%s'
      ORDER BY pc.cluster_name LIMIT 500", input$pa_accession))
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$search_table <- renderDT({
    req(input$gene_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- sprintf("
      SELECT g.gene_id, g.gene_name, g.gene_symbol, a.accession_name, a.species,
             c.contig_name, g.start_pos, g.end_pos, g.strand, g.gene_type
      FROM genes g
      JOIN accessions a ON g.accession_id = a.accession_id
      LEFT JOIN contigs c ON g.contig_id = c.contig_id
      WHERE g.gene_name ILIKE '%%%s%%'
      ORDER BY g.gene_name LIMIT 200", input$gene_query)
    df <- dbGetQuery(con, q)
    datatable(df, rownames = FALSE, selection = "single", filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip',
                             columnDefs = list(list(visible = FALSE, targets = 0)))) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  observeEvent(input$search_table_rows_selected, {
    row <- input$search_table_rows_selected
    if (length(row) == 0) return()
    req(input$gene_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- sprintf("
      SELECT g.gene_id, g.gene_name, g.gene_symbol, a.accession_name, a.species,
             c.contig_name, g.start_pos, g.end_pos, g.strand, g.gene_type
      FROM genes g
      JOIN accessions a ON g.accession_id = a.accession_id
      LEFT JOIN contigs c ON g.contig_id = c.contig_id
      WHERE g.gene_name ILIKE '%%%s%%'
      ORDER BY g.gene_name LIMIT 200", input$gene_query)
    df <- dbGetQuery(con, q)
    g <- df[row, ]
    show_gene_detail(g)
  })

  output$dl_genes <- downloadHandler(
    filename = function() paste0("genes_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM genes LIMIT 100000"), file, row.names = FALSE)
    })
  output$dl_clusters <- downloadHandler(
    filename = function() paste0("pan_gene_clusters_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM pan_gene_clusters"), file, row.names = FALSE)
    })
  output$dl_accessions <- downloadHandler(
    filename = function() paste0("accessions_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM accessions"), file, row.names = FALSE)
    })
  output$dl_presence <- downloadHandler(
    filename = function() paste0("presence_absence_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM gene_presence_absence LIMIT 100000"), file, row.names = FALSE)
    })
  output$dl_go <- downloadHandler(
    filename = function() paste0("go_annotations_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM go_annotations LIMIT 100000"), file, row.names = FALSE)
    })
  output$dl_kegg <- downloadHandler(
    filename = function() paste0("kegg_ko_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM kegg_ko LIMIT 100000"), file, row.names = FALSE)
    })
  output$dl_path <- downloadHandler(
    filename = function() paste0("kegg_pathways_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM kegg_pathways LIMIT 100000"), file, row.names = FALSE)
    })
  output$dl_kog <- downloadHandler(
    filename = function() paste0("kog_annotations_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM kog_annotations LIMIT 100000"), file, row.names = FALSE)
    })
  output$dl_crispr <- downloadHandler(
    filename = function() paste0("crispr_guides_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- connect_db(); on.exit(dbDisconnect(con))
      write.csv(dbGetQuery(con, "SELECT * FROM crispr_guides LIMIT 100000"), file, row.names = FALSE)
    })

  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")
    updateSelectInput(session, "go_accession", choices = c("All", acc$accession_name))
  })

  output$go_distribution <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT go_term, COUNT(*)::text AS n FROM go_annotations"
    if (!is.null(input$go_accession) && input$go_accession != "All") {
      q <- paste0(q, " WHERE accession_id = (SELECT accession_id FROM accessions WHERE accession_name = '",
                  input$go_accession, "')")
    }
    q <- paste0(q, " GROUP BY go_term ORDER BY COUNT(*) DESC LIMIT 20")
    df <- dbGetQuery(con, q); df$n <- as.numeric(df$n)
    par(mar = c(12, 5, 4, 2))
    barplot(df$n, names.arg = df$go_term, las = 2, col = "#1a4d38",
            border = "white", main = "Top 20 GO Terms",
            ylab = "Count", cex.names = 0.75,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$go_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT ga.go_term, g.gene_name, a.accession_name
          FROM go_annotations ga
          JOIN genes g ON ga.gene_id = g.gene_id
          JOIN accessions a ON ga.accession_id = a.accession_id"
    where <- c()
    if (!is.null(input$go_query) && nchar(input$go_query) > 0) {
      where <- c(where, sprintf("(ga.go_term ILIKE '%%%s%%' OR g.gene_name ILIKE '%%%s%%')",
                                input$go_query, input$go_query))
    }
    if (!is.null(input$go_accession) && input$go_accession != "All") {
      where <- c(where, sprintf("a.accession_name = '%s'", input$go_accession))
    }
    if (length(where) > 0) q <- paste0(q, " WHERE ", paste(where, collapse = " AND "))
    q <- paste0(q, " LIMIT 500")
    df <- dbGetQuery(con, q)
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")
    updateSelectInput(session, "kegg_accession", choices = c("All", acc$accession_name))
  })

  output$ko_distribution <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT ko_number, COUNT(*)::text AS n FROM kegg_ko"
    if (!is.null(input$kegg_accession) && input$kegg_accession != "All") {
      q <- paste0(q, " WHERE accession_id = (SELECT accession_id FROM accessions WHERE accession_name = '",
                  input$kegg_accession, "')")
    }
    q <- paste0(q, " GROUP BY ko_number ORDER BY COUNT(*) DESC LIMIT 20")
    df <- dbGetQuery(con, q); df$n <- as.numeric(df$n)
    par(mar = c(12, 5, 4, 2))
    barplot(df$n, names.arg = df$ko_number, las = 2, col = "#d48c1a",
            border = "white", main = "Top 20 KO Numbers",
            ylab = "Count", cex.names = 0.75,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$path_distribution <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT pathway_code, COUNT(*)::text AS n FROM kegg_pathways"
    if (!is.null(input$kegg_accession) && input$kegg_accession != "All") {
      q <- paste0(q, " WHERE accession_id = (SELECT accession_id FROM accessions WHERE accession_name = '",
                  input$kegg_accession, "')")
    }
    q <- paste0(q, " GROUP BY pathway_code ORDER BY COUNT(*) DESC LIMIT 20")
    df <- dbGetQuery(con, q); df$n <- as.numeric(df$n)
    par(mar = c(12, 5, 4, 2))
    barplot(df$n, names.arg = df$pathway_code, las = 2, col = "#8b5e9b",
            border = "white", main = "Top 20 KEGG Pathways",
            ylab = "Count", cex.names = 0.75,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$kegg_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT kk.ko_number, g.gene_name, a.accession_name
          FROM kegg_ko kk
          JOIN genes g ON kk.gene_id = g.gene_id
          JOIN accessions a ON kk.accession_id = a.accession_id"
    where <- c()
    if (!is.null(input$kegg_query) && nchar(input$kegg_query) > 0) {
      where <- c(where, sprintf("(kk.ko_number ILIKE '%%%s%%' OR g.gene_name ILIKE '%%%s%%')",
                                input$kegg_query, input$kegg_query))
    }
    if (!is.null(input$kegg_accession) && input$kegg_accession != "All") {
      where <- c(where, sprintf("a.accession_name = '%s'", input$kegg_accession))
    }
    if (length(where) > 0) q <- paste0(q, " WHERE ", paste(where, collapse = " AND "))
    q <- paste0(q, " LIMIT 500")
    df <- dbGetQuery(con, q)
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")
    updateSelectInput(session, "kog_accession", choices = c("All", acc$accession_name))
  })

  output$kog_total <- renderText({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT COUNT(*)::text AS n FROM kog_annotations"
    if (!is.null(input$kog_accession) && input$kog_accession != "All") {
      q <- paste0(q, " WHERE accession_id = (SELECT accession_id FROM accessions WHERE accession_name = '",
                  input$kog_accession, "')")
    }
    format(as.numeric(dbGetQuery(con, q)$n), big.mark = ",")
  })

  output$kog_chart <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT cog_letter, COUNT(*)::text AS n FROM kog_annotations"
    if (!is.null(input$kog_accession) && input$kog_accession != "All") {
      q <- paste0(q, " WHERE accession_id = (SELECT accession_id FROM accessions WHERE accession_name = '",
                  input$kog_accession, "')")
    }
    q <- paste0(q, " GROUP BY cog_letter ORDER BY cog_letter")
    df <- dbGetQuery(con, q); df$n <- as.numeric(df$n)
    par(mar = c(5, 5, 4, 2))
    barplot(df$n, names.arg = df$cog_letter, las = 1, col = "#2b7a5e",
            border = "white", main = "KOG Category Distribution",
            xlab = "COG Letter", ylab = "Count",
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$kog_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT ka.cog_letter, g.gene_name, a.accession_name
          FROM kog_annotations ka
          JOIN genes g ON ka.gene_id = g.gene_id
          JOIN accessions a ON ka.accession_id = a.accession_id"
    if (!is.null(input$kog_accession) && input$kog_accession != "All") {
      q <- paste0(q, " WHERE a.accession_name = '", input$kog_accession, "'")
    }
    q <- paste0(q, " LIMIT 500")
    df <- dbGetQuery(con, q)
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$unified_gene <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, sprintf("
      SELECT g.gene_name, g.gene_symbol, a.accession_name, a.species,
             c.contig_name, g.start_pos, g.end_pos, g.strand, g.gene_type
      FROM genes g
      JOIN accessions a ON g.accession_id = a.accession_id
      LEFT JOIN contigs c ON g.contig_id = c.contig_id
      WHERE g.gene_name ILIKE '%%%s%%' LIMIT 50", input$unified_query))
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$unified_go <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, sprintf("
      SELECT ga.go_term, g.gene_name, a.accession_name
      FROM go_annotations ga
      JOIN genes g ON ga.gene_id = g.gene_id
      JOIN accessions a ON ga.accession_id = a.accession_id
      WHERE g.gene_name ILIKE '%%%s%%' LIMIT 100", input$unified_query))
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$unified_kegg <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, sprintf("
      SELECT kk.ko_number, g.gene_name, a.accession_name
      FROM kegg_ko kk
      JOIN genes g ON kk.gene_id = g.gene_id
      JOIN accessions a ON kk.accession_id = a.accession_id
      WHERE g.gene_name ILIKE '%%%s%%' LIMIT 100", input$unified_query))
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$unified_kog <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, sprintf("
      SELECT ka.cog_letter, g.gene_name, a.accession_name
      FROM kog_annotations ka
      JOIN genes g ON ka.gene_id = g.gene_id
      JOIN accessions a ON ka.accession_id = a.accession_id
      WHERE g.gene_name ILIKE '%%%s%%' LIMIT 100", input$unified_query))
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")
    updateSelectInput(session, "crispr_accession", choices = c("All", acc$accession_name))
  })

  output$crispr_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT cg.gene_name, cg.spacer_seq, cg.pam_seq, cg.gc_pct,
                 cg.uniqueness, cg.specificity_class, a.accession_name
          FROM crispr_guides cg
          JOIN accessions a ON cg.accession_id = a.accession_id"
    where <- c()
    if (!is.null(input$crispr_query) && nchar(input$crispr_query) > 0) {
      where <- c(where, sprintf("(cg.gene_name ILIKE '%%%s%%' OR cg.spacer_seq ILIKE '%%%s%%')",
                                input$crispr_query, input$crispr_query))
    }
    if (!is.null(input$crispr_accession) && input$crispr_accession != "All") {
      where <- c(where, sprintf("a.accession_name = '%s'", input$crispr_accession))
    }
    if (!is.null(input$crispr_uniqueness) && input$crispr_uniqueness != "All") {
      where <- c(where, sprintf("cg.uniqueness = '%s'", input$crispr_uniqueness))
    }
    if (length(where) > 0) q <- paste0(q, " WHERE ", paste(where, collapse = " AND "))
    q <- paste0(q, " LIMIT 500")
    df <- dbGetQuery(con, q)
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  ai_state <- reactiveValues(history = list(), last_ask = NULL)

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

    if (!isTRUE(res$ok)) {
      ai_state$history <- c(ai_state$history,
        list(list(role = "bot", text = NULL, sql = NULL, result = NULL, error = res$error)))
      ai_state$history <- ai_state$history
      return()
    }

    out <- tryCatch({
      con <- connect_db(); on.exit(dbDisconnect(con))
      dbGetQuery(con, res$sql)
    }, error = function(e) e)

    if (inherits(out, "error")) {
      ai_state$history <- c(ai_state$history,
        list(list(role = "bot", text = NULL, sql = res$sql, result = NULL,
                  error = paste("SQL execution failed:", conditionMessage(out)))))
    } else {
      ai_state$history <- c(ai_state$history,
        list(list(role = "bot", text = NULL, sql = res$sql, result = out, error = NULL)))
    }

    # Force the renderUI to re-run
    ai_state$history <- ai_state$history
  })

  observeEvent(input$ai_clear, {
    ai_state$history <- list()
    ai_state$last_ask <- NULL
  })

  output$ai_conversation <- renderUI({
    if (length(ai_state$history) == 0) return(NULL)

    # Register DTOutputs for any entry that has a result
    for (i in seq_along(ai_state$history)) {
      entry <- ai_state$history[[i]]
      if (!is.null(entry$result)) {
        local({
          idx <- i
          df <- entry$result
          output[[paste0("ai_result_", idx)]] <- renderDT({
            datatable(df, rownames = FALSE,
                      options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")) %>%
              formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
          })
        })
      }
    }

    # Build the bubbles
    out <- lapply(seq_along(ai_state$history), function(i) {
      entry <- ai_state$history[[i]]

      if (entry$role == "user") {
        return(div(class = "ai-msg-user", shiny::icon("user"), " ", entry$text))
      }

      items <- list()

      # Bot header
      items[[length(items) + 1]] <- div(class = "ai-msg-bot",
                                        shiny::icon("robot"), " Assistant:")

      # Error message (if any)
      if (!is.null(entry$error) && nzchar(entry$error)) {
        items[[length(items) + 1]] <- div(class = "ai-err",
                                          shiny::icon("triangle-exclamation"),
                                          " ", entry$error)
      }

      # SQL display
      if (!is.null(entry$sql) && nzchar(entry$sql)) {
        items[[length(items) + 1]] <- div(class = "ai-sql", entry$sql)
      }

      # Result table
      if (!is.null(entry$result)) {
        items[[length(items) + 1]] <- DTOutput(paste0("ai_result_", i))
      }

      # Fallback: if nothing was stored, show a friendly message
      if (is.null(entry$error) && is.null(entry$sql) && is.null(entry$result)) {
        items[[length(items) + 1]] <- div(class = "ai-msg-bot",
                                          " (no reply)")
      }

      do.call(tagList, items)
    })

    do.call(tagList, out)
  })

  output$stat_genes_per_acc <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, gene_count::text AS n FROM accessions ORDER BY gene_count DESC")
    df$n <- as.numeric(df$n)
    par(mar = c(8, 5, 4, 2))
    barplot(df$n, names.arg = df$accession_name, las = 2, col = "#1a4d38",
            border = "white", main = "Genes per Accession", ylab = "Gene count",
            cex.names = 0.8,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$stat_gc <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, gc_content::text AS gc FROM accessions ORDER BY gc_content DESC")
    df$gc <- as.numeric(df$gc)
    par(mar = c(8, 5, 4, 2))
    barplot(df$gc, names.arg = df$accession_name, las = 2, col = "#f5b342",
            border = "white", main = "GC Content per Accession", ylab = "GC (%)",
            cex.names = 0.8,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$stat_genes_per_chrom <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "
      SELECT c.contig_name, a.accession_name, COUNT(*)::text AS n
      FROM genes g
      JOIN contigs c ON g.contig_id = c.contig_id
      JOIN accessions a ON g.accession_id = a.accession_id
      GROUP BY c.contig_name, a.accession_name")
    df$n <- as.numeric(df$n)
    if (nrow(df) > 0) {
      tab <- xtabs(n ~ contig_name + accession_name, data = df)
      par(mar = c(6, 5, 4, 8))
      barplot(t(tab), col = rainbow(ncol(tab)), border = NA,
              main = "Genes per Chromosome (stacked)", xlab = "Chromosome",
              ylab = "Gene count", las = 2, cex.names = 0.7,
              col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
              font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
      legend("topright", legend = colnames(tab), fill = rainbow(ncol(tab)),
             cex = 0.6, bg = "white", inset = c(-0.15, 0))
    }
  })

  output$stat_cluster_types <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT cluster_type, COUNT(*)::text AS n FROM pan_gene_clusters GROUP BY cluster_type ORDER BY COUNT(*) DESC")
    df$n <- as.numeric(df$n)
    par(mar = c(5, 5, 4, 2))
    pie(df$n, labels = paste0(df$cluster_type, "\n(", format(df$n, big.mark = ","), ")"),
        col = c("#1a4d38", "#f5b342", "#d48c1a", "#8b5e9b", "#2b7a5e"),
        border = "white", main = "Cluster Type Distribution",
        col.main = FONT_COLOR, font.main = FONT_BOLD, cex = 0.9)
  })

  output$stat_contig_len <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT contig_length::text AS len FROM contigs")
    df$len <- as.numeric(df$len)
    par(mar = c(5, 5, 4, 2))
    hist(df$len, breaks = 40, col = "#2b7a5e", border = "white",
         main = "Contig Length Distribution", xlab = "Length (bp)", ylab = "Frequency",
         col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
         font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$qc_genome_size <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, genome_size::text AS n FROM accessions ORDER BY genome_size DESC")
    df$n <- as.numeric(df$n) / 1e6
    par(mar = c(8, 5, 4, 2))
    barplot(df$n, names.arg = df$accession_name, las = 2, col = "#1a4d38",
            border = "white", main = "Genome Size", ylab = "Mb", cex.names = 0.8,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$qc_n50 <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, n50::text AS n FROM accessions ORDER BY n50 DESC")
    df$n <- as.numeric(df$n) / 1e6
    par(mar = c(8, 5, 4, 2))
    barplot(df$n, names.arg = df$accession_name, las = 2, col = "#f5b342",
            border = "white", main = "N50", ylab = "Mb", cex.names = 0.8,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$qc_gc <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, gc_content::text AS n FROM accessions ORDER BY gc_content DESC")
    df$n <- as.numeric(df$n)
    par(mar = c(8, 5, 4, 2))
    barplot(df$n, names.arg = df$accession_name, las = 2, col = "#d48c1a",
            border = "white", main = "GC Content", ylab = "%", cex.names = 0.8,
            col.axis = FONT_COLOR, col.lab = FONT_COLOR, col.main = FONT_COLOR,
            font.axis = FONT_BOLD, font.lab = FONT_BOLD, font.main = FONT_BOLD)
  })

  output$qc_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "
      SELECT accession_name, species, genome_size, gc_content, n50,
             contig_count, gene_count
      FROM accessions ORDER BY species, accession_name")
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 12, scrollX = TRUE, dom = 'ftip')) %>%
      formatRound(c("genome_size", "gc_content", "n50"), 2) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$species_matrix_plot <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    clu <- dbGetQuery(con, "SELECT cluster_id FROM pan_gene_clusters ORDER BY gene_count DESC LIMIT 100")
    pa <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present = TRUE")
    mat <- matrix(0, nrow = nrow(clu), ncol = nrow(acc))
    for (i in seq_len(nrow(pa))) {
      r <- which(clu$cluster_id == pa$cluster_id[i])
      c <- which(acc$accession_id == pa$accession_id[i])
      if (length(r) == 1 && length(c) == 1) mat[r, c] <- 1
    }
    par(mar = c(10, 3, 4, 2))
    image(1:nrow(acc), 1:nrow(clu), t(mat),
          col = c("#ffffff", "#d62728"),
          xaxt = "n", yaxt = "n", xlab = "", ylab = "",
          main = "Cross-Accession Cluster Sharing (Top 100 Clusters)",
          col.main = FONT_COLOR, font.main = FONT_BOLD, cex.main = 1.1)
    axis(1, at = 1:nrow(acc), labels = acc$accession_name, las = 2, cex.axis = 0.75,
         col.axis = FONT_COLOR, font.axis = FONT_BOLD)
  })

  output$species_matrix_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    clu <- dbGetQuery(con, "SELECT cluster_id, cluster_name FROM pan_gene_clusters ORDER BY gene_count DESC LIMIT 100")
    pa <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present = TRUE")
    mat <- matrix(0, nrow = nrow(clu), ncol = nrow(acc))
    for (i in seq_len(nrow(pa))) {
      r <- which(clu$cluster_id == pa$cluster_id[i])
      c <- which(acc$accession_id == pa$accession_id[i])
      if (length(r) == 1 && length(c) == 1) mat[r, c] <- 1
    }
    df <- as.data.frame(mat)
    colnames(df) <- acc$accession_name
    df <- cbind(cluster_name = clu$cluster_name, df)
    datatable(df, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip')) %>%
      formatStyle(columns = 1:ncol(df), color = FONT_COLOR, fontWeight = "600")
  })

  output$presence_heatmap <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    type_filter <- input$heatmap_type
    if (is.null(type_filter) || type_filter == "All") {
      q <- "SELECT cluster_id, cluster_name FROM pan_gene_clusters ORDER BY gene_count DESC LIMIT 100"
    } else {
      q <- sprintf("SELECT cluster_id, cluster_name FROM pan_gene_clusters WHERE cluster_type = '%s' ORDER BY gene_count DESC LIMIT 100", type_filter)
    }
    clu <- dbGetQuery(con, q)
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    pa <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present = TRUE")
    mat <- matrix(0, nrow = nrow(clu), ncol = nrow(acc))
    for (i in seq_len(nrow(pa))) {
      r <- which(clu$cluster_id == pa$cluster_id[i])
      c <- which(acc$accession_id == pa$accession_id[i])
      if (length(r) == 1 && length(c) == 1) mat[r, c] <- 1
    }
    par(mar = c(10, 3, 4, 2))
    image(1:nrow(acc), 1:nrow(clu), t(mat),
          col = c("#ffffff", "#d62728"),
          xaxt = "n", yaxt = "n", xlab = "", ylab = "",
          main = paste0("Presence/Absence Heatmap - ", type_filter, " clusters"),
          col.main = FONT_COLOR, font.main = FONT_BOLD, cex.main = 1.1)
    axis(1, at = 1:nrow(acc), labels = acc$accession_name, las = 2, cex.axis = 0.7,
         col.axis = FONT_COLOR, font.axis = FONT_BOLD)
  })
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)
