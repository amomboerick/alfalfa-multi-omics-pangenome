# Alfalfa Multi-Omics Pan-Genome Database - R Shiny App
# Publication-ready version with masthead + citation modal + animated counters

library(shiny)
library(shinythemes)
library(DT)
library(DBI)
library(RPostgres)
library(dplyr)
library(httr)
library(jsonlite)

# ============================================================
# Database configuration
# ============================================================
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

# ============================================================
# AI: Groq NL -> SQL
# ============================================================
DB_SCHEMA_PROMPT <- "
You are an expert PostgreSQL assistant. Given a user question, return ONLY a
valid PostgreSQL SQL query that answers it. Do NOT include explanations,
markdown code fences, or any text other than the SQL.

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

Notes:
- cluster_type values: 'core', 'soft_core', 'dispensable', 'private', 'singleton'
- 12 accessions, 823,838 genes, ~217,000 clusters
- 5.8M GO annotations, 449K KEGG KOs, 2M KEGG pathways, 856K KOG
- 3.56M CRISPR guides targeting 518,858 genes
- Always use LIMIT (default 100) unless the user asks for aggregate counts.
"

ask_groq_for_sql <- function(question) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (nchar(api_key) < 10) return(list(ok = FALSE, error = "GROQ_API_KEY not set"))
  body <- list(
    model = "openai/gpt-oss-120b",
    messages = list(
      list(role = "system", content = DB_SCHEMA_PROMPT),
      list(role = "user",   content = question)
    ),
    temperature = 0.1,
    max_tokens = 800
  )
  res <- tryCatch(
    POST("https://api.groq.com/openai/v1/chat/completions",
         add_headers(Authorization = paste("Bearer", api_key)),
         content_type_json(),
         body = toJSON(body, auto_unbox = TRUE),
         encode = "json", timeout(30)),
    error = function(e) NULL
  )
  if (is.null(res)) return(list(ok = FALSE, error = "Network error"))
  if (status_code(res) != 200) {
    msg <- tryCatch(content(res, "parsed")$error$message, error = function(e) "Unknown error")
    return(list(ok = FALSE, error = paste("Groq API error:", msg)))
  }
  txt <- content(res, "parsed")$choices[[1]]$message$content
  txt <- gsub("^```sql\\s*", "", txt, ignore.case = TRUE)
  txt <- gsub("^```\\s*",     "", txt)
  txt <- gsub("```\\s*$",     "", txt)
  txt <- trimws(txt)
  list(ok = TRUE, sql = txt)
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
    tags$script(src = "counters.js"),
    tags$style(HTML("
    html, body { min-height: 100%; }
    body {
      background: #e8efe9;
      display: flex;
      flex-direction: column;
      min-height: 100vh;
      font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
    }
    .main-wrapper { flex: 1 0 auto; }

    .masthead {
      background: linear-gradient(135deg, #0B3B2C 0%, #1A4D38 60%, #2B7A5E 100%);
      color: white;
      padding: 2rem 2.5rem 1.5rem 2.5rem;
      border-bottom: 6px solid #F5B342;
      position: relative;
      overflow: hidden;
      margin-bottom: 20px;
    }
    .masthead::before {
      content: '';
      position: absolute;
      top: -50%; right: -10%;
      width: 400px; height: 400px;
      background: radial-gradient(circle, rgba(245,179,66,0.12) 0%, transparent 70%);
      pointer-events: none;
    }
    .masthead-inner {
      max-width: 1400px;
      margin: 0 auto;
      position: relative;
      z-index: 2;
    }
    .masthead h1 {
      font-weight: 800;
      font-size: 2.3rem;
      letter-spacing: -0.02em;
      margin: 0;
      color: white;
      display: flex;
      align-items: center;
      gap: 16px;
    }
    .masthead h1 .logo-leaf { color: #F5B342; font-size: 1.9rem; }
    .masthead .subtitle {
      font-size: 1.05rem;
      color: #d4e3db;
      margin-top: 8px;
      font-weight: 400;
    }
    .masthead .meta-row {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      margin-top: 16px;
      align-items: center;
    }
    .meta-badge {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      background: rgba(255,255,255,0.1);
      border: 1px solid rgba(245,179,66,0.4);
      color: #F5B342;
      padding: 4px 14px;
      border-radius: 40px;
      font-size: 0.78rem;
      font-weight: 600;
    }
    .meta-badge .val { color: white; margin-left: 4px; }
    .cite-btn {
      background: #F5B342;
      color: #0B3B2C !important;
      border: none;
      padding: 8px 22px;
      border-radius: 40px;
      font-weight: 700;
      font-size: 0.85rem;
      cursor: pointer;
      transition: all 0.2s ease;
      display: inline-flex;
      align-items: center;
      gap: 8px;
      box-shadow: 0 4px 12px rgba(245,179,66,0.3);
    }
    .cite-btn:hover {
      background: #fccf6b;
      transform: translateY(-2px);
      box-shadow: 0 8px 20px rgba(245,179,66,0.4);
    }

    .stat-box {
      background: white;
      border-left: 8px solid #1a4d38;
      border-radius: 12px;
      padding: 15px;
      text-align: center;
      box-shadow: 0 4px 12px rgba(0,0,0,0.05);
      margin-bottom: 15px;
    }
    .stat-box h2 { color: #1a4d38; font-size: 2.2rem; margin: 0; font-weight: 800; }
    .stat-box p  { color: #08261a; margin: 0; font-weight: 600; font-size: 0.9rem; }

    .quick-card {
      background: white;
      border-left: 6px solid #f5b342;
      border-radius: 10px;
      padding: 16px;
      margin-bottom: 16px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.04);
      transition: 0.15s;
      cursor: pointer;
    }
    .quick-card:hover { transform: translateY(-3px); box-shadow: 0 8px 20px rgba(0,0,0,0.08); }
    .quick-card h4 { color: #1a4d38; margin: 0 0 6px 0; font-weight: 700; }
    .quick-card p  { color: #3d5a4a; margin: 0; font-size: 0.9rem; }
    .quick-card i  { color: #d48c1a; font-size: 1.6rem; margin-right: 10px; }

    .badge { display:inline-block; padding:2px 10px; border-radius:40px;
             font-size:0.75rem; font-weight:700; margin-left:6px; }
    .badge-core { background:#1a4d38; color:white; }
    .badge-soft_core { background:#f5b342; color:#08261a; }
    .badge-dispensable { background:#d48c1a; color:white; }
    .badge-private { background:#8b5e9b; color:white; }
    .badge-singleton { background:#8fa7b3; color:white; }

    .ai-msg-user { background: #e8efe9; border-left: 4px solid #1a4d38;
                   padding: 10px 14px; border-radius: 8px; margin-bottom: 10px; }
    .ai-msg-bot { background: #fff7e6; border-left: 4px solid #f5b342;
                  padding: 10px 14px; border-radius: 8px; margin-bottom: 10px; }
    .ai-sql { background: #08261a; color: #b8e3b0; padding: 12px; border-radius: 6px;
              font-family: monospace; font-size: 0.85rem; white-space: pre-wrap; margin: 8px 0; }

    .footer {
      flex-shrink: 0;
      background: #08261a;
      color: #cde0d3;
      padding: 2rem 2rem 1rem 2rem;
      margin-top: 40px;
      border-top: 6px solid #f5b342;
    }
    .footer h4 { color: #f5b342; font-size: 1rem; font-weight: 700;
                 margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px; }
    .footer p, .footer li { font-size: 0.85rem; color: #a7beb1; }
    .footer a { color: #cde0d3; text-decoration: none; }
    .footer a:hover { color: #f5b342; }
    .footer ul { list-style: none; padding: 0; }
    .footer ul li { padding: 3px 0; }
    .footer-bottom {
      border-top: 1px solid #1f4a38; margin-top: 20px; padding-top: 12px;
      text-align: center; font-size: 0.8rem; color: #7a9587;
    }
    .footer-bottom strong { color: #f5b342; }
  "))),

  div(class = "main-wrapper",

    div(class = "masthead",
      div(class = "masthead-inner",
        h1(
          span(class = "logo-leaf", shiny::icon("leaf")),
          "Alfalfa Multi-Omics Pan-Genome Database"
        ),
        div(class = "subtitle",
          shiny::icon("dna"),
          " A comprehensive genomic, transcriptomic, and CRISPR resource for the Medicago genus"
        ),
        div(class = "meta-row",
          span(class = "meta-badge",
            shiny::icon("code-branch"), "Version",
            span(class = "val", "2.0")),
          span(class = "meta-badge",
            shiny::icon("calendar"), "Updated",
            span(class = "val", as.character(Sys.Date()))),
          span(class = "meta-badge",
            shiny::icon("globe"), "Species",
            span(class = "val", "6")),
          span(class = "meta-badge",
            shiny::icon("dna"), "Genes",
            span(class = "val", "823,838")),
          span(class = "meta-badge",
            shiny::icon("microscope"), "Annotations",
            span(class = "val", "13M+")),
          span(class = "meta-badge",
            shiny::icon("cut"), "CRISPR guides",
            span(class = "val", "3.5M")),
          actionButton("cite_btn",
                       label = tagList(shiny::icon("quote-left"), "Cite This Resource"),
                       class = "cite-btn")
        )
      )
    ),

    div(class = "container-fluid",
      tabsetPanel(id = "tabs",

        # ============ HOME ============
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
          div(style = "background:white; padding:20px; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.05);",
            plotOutput("hero_figure", height = "820px")
          ),
          br(),
          downloadButton("dl_hero_figure", "Download Figure 1 as PNG",
                         style = "background:#1a4d38; color:white; font-weight:700; padding:8px 20px; border-radius:8px; border:none;"),
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

        # ============ DATA TABS ============
        tabPanel("Accessions",
          br(), h3("\U0001F331 The 12 Medicago Accessions"),
          DTOutput("accessions_table")),

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
          helpText("Search genes by GO term. Example: GO:0006952 (defense response)."),
          br(),
          fluidRow(
            column(6, textInput("go_query", "Search by GO term or gene name:",
                                placeholder = "e.g. GO:0006952 or Mara000007")),
            column(6, selectInput("go_accession", "Filter by accession:",
                                  choices = c("All"), selected = "All"))
          ),
          br(), h4("GO Term Distribution"),
          plotOutput("go_distribution", height = "300px"),
          br(), h4("Matching Annotations"),
          DTOutput("go_table")),

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
            column(6, h4("KO Number Distribution"), plotOutput("ko_distribution", height = "300px")),
            column(6, h4("Pathway Distribution (top 20)"), plotOutput("path_distribution", height = "300px"))
          ),
          br(), h4("Matching Annotations"),
          DTOutput("kegg_table")),

        tabPanel("KOG Classes",
          br(), h3("\U0001F9EC KOG / COG Functional Categories"),
          br(),
          fluidRow(
            column(6, selectInput("kog_accession", "Filter by accession:",
                                  choices = c("All"), selected = "All")),
            column(6, br(), br(), h4("Total genes with KOG:", textOutput("kog_total", inline = TRUE)))
          ),
          br(), h4("KOG Category Distribution"),
          plotOutput("kog_chart", height = "500px"),
          br(), h4("KOG Annotations Table"),
          DTOutput("kog_table")),

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
          br(), h4("Matching Guides"),
          DTOutput("crispr_table")),

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
                         style = "background:#f5b342; color:#08261a; font-weight:700; border:2px solid #d4942e; border-radius:8px; padding:8px 20px;"),
            actionButton("ai_clear", label = tagList(shiny::icon("trash"), "Clear"),
                         style = "background:#e0e8e4; color:#08261a; font-weight:600; border:2px solid #c8d5cd; border-radius:8px; padding:8px 20px; margin-left:8px;")
          ),
          br(),
          uiOutput("ai_conversation")),

        tabPanel("Statistics",
          br(), h3("\U0001F4CA Pan-Genome Statistics"), br(),
          fluidRow(
            column(6, div(class = "stat-box", h4("Genes per Accession"),
                          plotOutput("stat_genes_per_acc", height = "400px"))),
            column(6, div(class = "stat-box", h4("GC Content per Accession"),
                          plotOutput("stat_gc", height = "400px")))
          ),
          fluidRow(
            column(6, div(class = "stat-box", h4("Genes per Chromosome (stacked)"),
                          plotOutput("stat_genes_per_chrom", height = "400px"))),
            column(6, div(class = "stat-box", h4("Cluster Type Distribution"),
                          plotOutput("stat_cluster_types", height = "400px")))
          ),
          fluidRow(
            column(12, div(class = "stat-box", h4("Contig Length Distribution"),
                           plotOutput("stat_contig_len", height = "320px")))
          )),

        tabPanel("Assembly Quality",
          br(), h3("\U0001F4CA Assembly Quality Summary"), br(),
          fluidRow(
            column(4, div(class = "stat-box", h4("Genome Size (Mb)"),
                          plotOutput("qc_genome_size", height = "400px"))),
            column(4, div(class = "stat-box", h4("N50 (Mb)"),
                          plotOutput("qc_n50", height = "400px"))),
            column(4, div(class = "stat-box", h4("GC Content (%)"),
                          plotOutput("qc_gc", height = "400px")))
          ),
          br(), h4("Detailed Assembly Metrics"), DTOutput("qc_table")),

        tabPanel("Species Comparison",
          br(), h3("\U0001F30D Cross-Accession Cluster Sharing"),
          br(),
          plotOutput("species_matrix_plot", height = "700px"),
          br(), h4("Shared Cluster Counts (Table View)"),
          DTOutput("species_matrix_table")),

        tabPanel("Heatmap",
          br(), h3("\U0001F7E9 Presence/Absence Heatmap (Top 100 Clusters)"),
          br(),
          selectInput("heatmap_type", "Filter clusters by type:",
                      choices = c("core", "soft_core", "dispensable", "All"),
                      selected = "core"),
          plotOutput("presence_heatmap", height = "900px"))
      )
    )
  ),

  div(class = "footer",
    div(class = "container-fluid",
      fluidRow(
        column(4,
          h4(shiny::icon("leaf"), " Alfalfa Multi-Omics Pan-Genome Database"),
          p("A comprehensive multi-omics resource for the Medicago genus."),
          p(style = "color:#7a9587; margin-top:10px; font-size:0.8rem;",
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
          tags$li(tags$a(href = "#", "Citation")),
          tags$li(tags$a(href = "#", "License"))))
      ),
      div(class = "footer-bottom",
        HTML(paste0(
          "\u00A9 2026 <strong>Alfalfa Multi-Omics Pan-Genome Database Consortium</strong> \u00B7 ",
          "Version 2.0 \u00B7 Last updated: ", Sys.Date(),
          " \u00B7 Built with <strong>R Shiny</strong> + <strong>PostgreSQL</strong> + <strong>Groq AI</strong>"
        ))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---------- Citation modal ----------
  observeEvent(input$cite_btn, {
    showModal(modalDialog(
      title = tagList(shiny::icon("quote-left"), " Cite the Alfalfa Multi-Omics Pan-Genome Database"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML(paste0(
        "<h4 style='color:#0B3B2C; margin-bottom:14px;'>Plain Text Citation</h4>",
        "<div style='background:#f7faf5; padding:16px; border-left:6px solid #F5B342;",
        "     border-radius:8px; font-family:monospace; font-size:0.9rem; line-height:1.6;'>",
        "Alfalfa Multi-Omics Pan-Genome Database Consortium (2026). ",
        "<i>A comprehensive multi-omics pan-genome resource for the Medicago genus.</i> ",
        "Version 2.0. Available at: https://github.com/amomboerick/alfalfa-multi-omics-pangenome",
        "</div>",
        "<h4 style='color:#0B3B2C; margin-top:24px; margin-bottom:14px;'>BibTeX</h4>",
        "<div style='background:#0B3B2C; color:#b8e3b0; padding:16px; border-radius:8px;",
        "     font-family:monospace; font-size:0.85rem; white-space:pre; overflow-x:auto;'>",
        "@misc{alfalfa_pangenome_2026,\n",
        "  title        = {Alfalfa Multi-Omics Pan-Genome Database},\n",
        "  author       = {{Alfalfa Multi-Omics Pan-Genome Database Consortium}},\n",
        "  year         = {2026},\n",
        "  version      = {2.0},\n",
        "  howpublished = {\\url{https://github.com/amomboerick/alfalfa-multi-omics-pangenome}},\n",
        "  note         = {12 accessions, 823,838 genes, 217,122 pan-gene clusters}\n",
        "}",
        "</div>",
        "<h4 style='color:#0B3B2C; margin-top:24px; margin-bottom:14px;'>Contact</h4>",
        "<p style='font-size:0.9rem;'>For questions: <b>Erick Amombo</b> \u2014 amomboeric@gmail.com</p>"
      ))
    ))
  })

  # ---------- Animated stat outputs ----------
  make_counter_ui <- function(numeric_value) {
    formatted <- format(numeric_value, big.mark = ",", scientific = FALSE)
    session$sendCustomMessage("runCounters", list())
    tags$span(
      class = "counter-value",
      `data-animate-counter` = "true",
      `data-target` = numeric_value,
      formatted
    )
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

  # ---------- Navigation ----------
  observeEvent(input$nav_to, {
    target <- input$nav_to
    if (target == "core_clusters") {
      updateTabsetPanel(session, "tabs", selected = "Pan-Gene Clusters")
      updateSelectInput(session, "cluster_type_filter", selected = "core")
    } else if (target == "top_families") {
      updateTabsetPanel(session, "tabs", selected = "Top Gene Families")
    } else if (target == "heatmap") {
      updateTabsetPanel(session, "tabs", selected = "Heatmap")
    } else if (target == "ai") {
      updateTabsetPanel(session, "tabs", selected = "AI Assistant")
    } else if (target == "go") {
      updateTabsetPanel(session, "tabs", selected = "GO Browser")
    } else if (target == "kegg") {
      updateTabsetPanel(session, "tabs", selected = "KEGG Pathways")
    } else if (target == "crispr") {
      updateTabsetPanel(session, "tabs", selected = "CRISPR Guides")
    }
  })

  # ---------- HERO FIGURE ----------
  output$hero_figure <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    dfA <- dbGetQuery(con, "SELECT cluster_type, COUNT(*)::text AS n FROM pan_gene_clusters GROUP BY cluster_type ORDER BY COUNT(*) DESC")
    dfA$n <- as.numeric(dfA$n)
    dfB <- dbGetQuery(con, "SELECT accession_name, species, gene_count::text AS n FROM accessions ORDER BY gene_count DESC")
    dfB$n <- as.numeric(dfB$n)
    dfC <- dbGetQuery(con, "
      SELECT c.contig_name, a.accession_name, COUNT(*)::text AS n
      FROM genes g
      JOIN contigs c ON g.contig_id = c.contig_id
      JOIN accessions a ON g.accession_id = a.accession_id
      GROUP BY c.contig_name, a.accession_name")
    dfC$n <- as.numeric(dfC$n)
    clu <- dbGetQuery(con, "SELECT cluster_id, cluster_name FROM pan_gene_clusters WHERE cluster_type='core' ORDER BY gene_count DESC LIMIT 20")
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    pa  <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present=TRUE")
    acc_order <- acc$accession_id
    mat <- matrix(0, nrow=nrow(clu), ncol=length(acc_order))
    for (i in seq_len(nrow(pa))) {
      r <- which(clu$cluster_id == pa$cluster_id[i])
      cc <- which(acc_order == pa$accession_id[i])
      if (length(r)==1 && length(cc)==1) mat[r,cc] <- 1
    }
    par(mfrow = c(2,2), mar = c(4,12,3,2), oma = c(0,0,3,0))

    palA <- c("#1a4d38","#f5b342","#d48c1a","#8b5e9b","#2b7a5e")
    pct <- round(dfA$n/sum(dfA$n)*100, 1)
    labelsA <- paste0(dfA$cluster_type, " (", format(dfA$n, big.mark=","), ")")
    pie(dfA$n, labels=labelsA, col=palA[seq_len(nrow(dfA))],
        main="A. Cluster type distribution", cex=0.7, border="white")

    species_colors <- c("Medicago sativa"="#1a4d38","Medicago truncatula"="#f5b342",
                        "Medicago ruthenica"="#d48c1a","Medicago arabica"="#8b5e9b",
                        "Medicago polymorpha"="#2b7a5e","Medicago lupulina"="#5a9b7e")
    bc <- species_colors[dfB$species]; bc[is.na(bc)] <- "#8fa7b3"
    par(mar=c(4,12,3,2))
    barplot(dfB$n, names.arg=dfB$accession_name, las=1, horiz=TRUE,
            col=bc, border="white", main="B. Genes per accession",
            xlab="Gene count", cex.names=0.7)

    if (nrow(dfC) > 0) {
      tab <- xtabs(n ~ contig_name + accession_name, data=dfC)
      par(mar=c(4,5,3,2))
      barplot(t(tab), col=rainbow(ncol(tab)), border=NA,
              main="C. Genes per chromosome", xlab="Chromosome",
              ylab="Gene count", las=2, cex.names=0.6)
    }

    par(mar=c(4,12,3,2))
    plot(NA, xlim=c(0,ncol(mat)), ylim=c(0,nrow(mat)), xaxt="n", yaxt="n",
         xlab="", ylab="", main="D. Presence/absence of top 20 core clusters", bty="n")
    for (i in 1:nrow(mat)) for (j in 1:ncol(mat)) {
      rect(j-1, i-1, j, i, col=if(mat[i,j]==1) "#1a4d38" else "#e6eae8",
           border="white", lwd=0.4)
    }
    axis(2, at=(1:nrow(mat))-0.5, labels=clu$cluster_name, las=2, cex.axis=0.55, tick=FALSE)
    axis(1, at=(1:ncol(mat))-0.5, labels=acc$accession_name, las=2, cex.axis=0.6, tick=FALSE)

    mtext("Figure 1 - Alfalfa Multi-Omics Pan-Genome Overview",
          outer=TRUE, cex=1.15, font=2, col="#0B3B2C", line=0.5)
  }, height = 820)

  output$dl_hero_figure <- downloadHandler(
    filename = function() paste0("Figure1_pan_genome_overview_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 150)
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
      par(mfrow = c(2,2), mar = c(4,12,3,2), oma = c(0,0,3,0))
      palA <- c("#1a4d38","#f5b342","#d48c1a","#8b5e9b","#2b7a5e")
      labelsA <- paste0(dfA$cluster_type, " (", format(dfA$n, big.mark=","), ")")
      pie(dfA$n, labels=labelsA, col=palA[seq_len(nrow(dfA))],
          main="A. Cluster type distribution", cex=0.7, border="white")
      species_colors <- c("Medicago sativa"="#1a4d38","Medicago truncatula"="#f5b342",
                          "Medicago ruthenica"="#d48c1a","Medicago arabica"="#8b5e9b",
                          "Medicago polymorpha"="#2b7a5e","Medicago lupulina"="#5a9b7e")
      bc <- species_colors[dfB$species]; bc[is.na(bc)] <- "#8fa7b3"
      par(mar=c(4,12,3,2))
      barplot(dfB$n, names.arg=dfB$accession_name, las=1, horiz=TRUE,
              col=bc, border="white", main="B. Genes per accession", xlab="Gene count", cex.names=0.7)
      if (nrow(dfC) > 0) {
        tab <- xtabs(n ~ contig_name + accession_name, data=dfC)
        par(mar=c(4,5,3,2))
        barplot(t(tab), col=rainbow(ncol(tab)), border=NA, main="C. Genes per chromosome",
                xlab="Chromosome", ylab="Gene count", las=2, cex.names=0.6)
      }
      par(mar=c(4,12,3,2))
      plot(NA, xlim=c(0,ncol(mat)), ylim=c(0,nrow(mat)), xaxt="n", yaxt="n", xlab="", ylab="",
           main="D. Presence/absence of top 20 core clusters", bty="n")
      for (i in 1:nrow(mat)) for (j in 1:ncol(mat)) {
        rect(j-1, i-1, j, i, col=if(mat[i,j]==1) "#1a4d38" else "#e6eae8", border="white", lwd=0.4)
      }
      axis(2, at=(1:nrow(mat))-0.5, labels=clu$cluster_name, las=2, cex.axis=0.55, tick=FALSE)
      axis(1, at=(1:ncol(mat))-0.5, labels=acc$accession_name, las=2, cex.axis=0.6, tick=FALSE)
      mtext("Figure 1 - Alfalfa Multi-Omics Pan-Genome Overview", outer=TRUE, cex=1.3, font=2, col="#0B3B2C", line=0.7)
      dev.off()
    }
  )

  # ---------- Data tables ----------
  output$accessions_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, species, subspecies, cultivar, genome_size, gc_content, contig_count, gene_count FROM accessions ORDER BY species, accession_name")
    datatable(df, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$clusters_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- "SELECT cluster_id, cluster_name, cluster_type, gene_count, presence_across_accessions FROM pan_gene_clusters"
    if (!is.null(input$cluster_type_filter) && input$cluster_type_filter != "All") {
      q <- paste0(q, " WHERE cluster_type = '", input$cluster_type_filter, "'")
    }
    q <- paste0(q, " ORDER BY gene_count DESC LIMIT 5000")
    datatable(dbGetQuery(con, q), selection = "single", options = list(pageLength = 20, scrollX = TRUE))
  })

  output$top_families_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT cluster_id, cluster_name, cluster_type, gene_count, presence_across_accessions FROM pan_gene_clusters ORDER BY gene_count DESC, presence_across_accessions DESC LIMIT 20")
    datatable(df, selection = "single", options = list(pageLength = 20, scrollX = TRUE, dom = 't'))
  })

  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")$accession_name
    updateSelectInput(session, "pa_accession", choices = acc)
  })
  output$pa_table <- renderDT({
    req(input$pa_accession)
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, sprintf("
      SELECT pc.cluster_name, pc.cluster_type, pc.gene_count
      FROM gene_presence_absence gpa
      JOIN pan_gene_clusters pc ON gpa.cluster_id = pc.cluster_id
      JOIN accessions a ON gpa.accession_id = a.accession_id
      WHERE a.accession_name = '%s' LIMIT 5000", input$pa_accession))
    datatable(df, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$search_table <- renderDT({
    req(input$gene_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- sprintf("SELECT g.gene_name, a.accession_name, a.species, c.contig_name, g.start_pos, g.end_pos, g.strand FROM genes g JOIN accessions a ON g.accession_id = a.accession_id JOIN contigs c ON g.contig_id = c.contig_id WHERE g.gene_name ILIKE '%%%s%%' LIMIT 500", input$gene_query)
    datatable(dbGetQuery(con, q), options = list(pageLength = 20, scrollX = TRUE))
  })

  # ---------- GO Browser ----------
  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")$accession_name
    updateSelectInput(session, "go_accession", choices = c("All", acc))
  })
  output$go_table <- renderDT({
    req(input$go_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc_filter <- if (!is.null(input$go_accession) && input$go_accession != "All") sprintf(" AND a.accession_name = '%s'", input$go_accession) else ""
    q <- sprintf("SELECT g.gene_name, a.accession_name, a.species, go.go_term FROM go_annotations go JOIN genes g ON go.gene_id = g.gene_id JOIN accessions a ON go.accession_id = a.accession_id WHERE (go.go_term ILIKE '%%%s%%' OR g.gene_name ILIKE '%%%s%%') %s LIMIT 1000", input$go_query, input$go_query, acc_filter)
    datatable(dbGetQuery(con, q), options = list(pageLength = 20, scrollX = TRUE))
  })
  output$go_distribution <- renderPlot({
    req(input$go_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc_filter <- if (!is.null(input$go_accession) && input$go_accession != "All") sprintf(" AND a.accession_name = '%s'", input$go_accession) else ""
    q <- sprintf("SELECT a.accession_name, COUNT(*)::text AS n FROM go_annotations go JOIN genes g ON go.gene_id = g.gene_id JOIN accessions a ON go.accession_id = a.accession_id WHERE (go.go_term ILIKE '%%%s%%' OR g.gene_name ILIKE '%%%s%%') %s GROUP BY a.accession_name ORDER BY COUNT(*) DESC", input$go_query, input$go_query, acc_filter)
    df <- dbGetQuery(con, q)
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No results"); return() }
    df$n <- as.numeric(df$n)
    par(mar = c(5, 14, 3, 1))
    barplot(df$n, names.arg = df$accession_name, las = 1, horiz = TRUE, col = "#2b7a5e", main = "GO term hits per accession", xlab = "Count", cex.names = 0.8)
  })

  # ---------- KEGG ----------
  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")$accession_name
    updateSelectInput(session, "kegg_accession", choices = c("All", acc))
  })
  output$kegg_table <- renderDT({
    req(input$kegg_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc_filter <- if (!is.null(input$kegg_accession) && input$kegg_accession != "All") sprintf(" AND a.accession_name = '%s'", input$kegg_accession) else ""
    q <- sprintf("SELECT g.gene_name, a.accession_name, kk.ko_number, kp.pathway_code FROM kegg_ko kk JOIN genes g ON kk.gene_id = g.gene_id JOIN accessions a ON kk.accession_id = a.accession_id LEFT JOIN kegg_pathways kp ON kp.gene_id = kk.gene_id AND kp.accession_id = kk.accession_id WHERE (kk.ko_number ILIKE '%%%s%%' OR g.gene_name ILIKE '%%%s%%') %s LIMIT 1000", input$kegg_query, input$kegg_query, acc_filter)
    datatable(dbGetQuery(con, q), options = list(pageLength = 20, scrollX = TRUE))
  })
  output$ko_distribution <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT ko_number, COUNT(*)::text AS n FROM kegg_ko GROUP BY ko_number ORDER BY COUNT(*) DESC LIMIT 20")
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return() }
    df$n <- as.numeric(df$n); par(mar = c(5, 8, 3, 1))
    barplot(df$n, names.arg = df$ko_number, las = 2, cex.names = 0.7, col = "#1a4d38", main = "Top 20 KEGG KO numbers", ylab = "Count")
  })
  output$path_distribution <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT pathway_code, COUNT(*)::text AS n FROM kegg_pathways GROUP BY pathway_code ORDER BY COUNT(*) DESC LIMIT 20")
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return() }
    df$n <- as.numeric(df$n); par(mar = c(5, 8, 3, 1))
    barplot(df$n, names.arg = df$pathway_code, las = 2, cex.names = 0.7, col = "#f5b342", main = "Top 20 KEGG pathways", ylab = "Count")
  })

  # ---------- KOG ----------
  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")$accession_name
    updateSelectInput(session, "kog_accession", choices = c("All", acc))
  })
  output$kog_total <- renderText({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc_filter <- if (!is.null(input$kog_accession) && input$kog_accession != "All") sprintf(" WHERE a.accession_name = '%s'", input$kog_accession) else ""
    q <- paste0("SELECT COUNT(*)::text AS n FROM kog_annotations ko JOIN accessions a ON ko.accession_id = a.accession_id", acc_filter)
    n <- dbGetQuery(con, q)$n; format(as.numeric(n), big.mark = ",")
  })
  output$kog_chart <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc_filter <- if (!is.null(input$kog_accession) && input$kog_accession != "All") sprintf(" WHERE a.accession_name = '%s'", input$kog_accession) else ""
    q <- paste0("SELECT ko.cog_letter, COUNT(*)::text AS n FROM kog_annotations ko JOIN accessions a ON ko.accession_id = a.accession_id ", acc_filter, " GROUP BY ko.cog_letter ORDER BY ko.cog_letter")
    df <- dbGetQuery(con, q)
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return() }
    df$n <- as.numeric(df$n); par(mar = c(5, 5, 3, 1))
    barplot(df$n, names.arg = df$cog_letter, las = 1, col = "#8b5e9b", main = "KOG Category Distribution", xlab = "COG letter", ylab = "Gene count")
  })
  output$kog_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc_filter <- if (!is.null(input$kog_accession) && input$kog_accession != "All") sprintf(" WHERE a.accession_name = '%s'", input$kog_accession) else ""
    q <- paste0("SELECT g.gene_name, a.accession_name, ko.cog_letter FROM kog_annotations ko JOIN genes g ON ko.gene_id = g.gene_id JOIN accessions a ON ko.accession_id = a.accession_id ", acc_filter, " LIMIT 2000")
    datatable(dbGetQuery(con, q), options = list(pageLength = 20, scrollX = TRUE))
  })

  # ---------- Unified search ----------
  output$unified_gene <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- sprintf("SELECT g.gene_name, a.accession_name, a.species, c.contig_name, g.start_pos, g.end_pos, g.strand FROM genes g JOIN accessions a ON g.accession_id = a.accession_id JOIN contigs c ON g.contig_id = c.contig_id WHERE g.gene_name ILIKE '%%%s%%' LIMIT 50", input$unified_query)
    datatable(dbGetQuery(con, q), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$unified_go <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- sprintf("SELECT g.gene_name, a.accession_name, go.go_term FROM go_annotations go JOIN genes g ON go.gene_id = g.gene_id JOIN accessions a ON go.accession_id = a.accession_id WHERE g.gene_name ILIKE '%%%s%%' LIMIT 200", input$unified_query)
    datatable(dbGetQuery(con, q), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$unified_kegg <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- sprintf("SELECT g.gene_name, a.accession_name, kk.ko_number, kp.pathway_code FROM kegg_ko kk JOIN genes g ON kk.gene_id = g.gene_id JOIN accessions a ON kk.accession_id = a.accession_id LEFT JOIN kegg_pathways kp ON kp.gene_id = kk.gene_id WHERE g.gene_name ILIKE '%%%s%%' LIMIT 200", input$unified_query)
    datatable(dbGetQuery(con, q), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$unified_kog <- renderDT({
    req(input$unified_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    q <- sprintf("SELECT g.gene_name, a.accession_name, ko.cog_letter FROM kog_annotations ko JOIN genes g ON ko.gene_id = g.gene_id JOIN accessions a ON ko.accession_id = a.accession_id WHERE g.gene_name ILIKE '%%%s%%' LIMIT 200", input$unified_query)
    datatable(dbGetQuery(con, q), options = list(pageLength = 10, scrollX = TRUE))
  })

  # ---------- CRISPR ----------
  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")$accession_name
    updateSelectInput(session, "crispr_accession", choices = c("All", acc))
  })
  output$crispr_table <- renderDT({
    req(input$crispr_query)
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc_filter <- if (!is.null(input$crispr_accession) && input$crispr_accession != "All") sprintf(" AND a.accession_name = '%s'", input$crispr_accession) else ""
    uniq_filter <- if (!is.null(input$crispr_uniqueness) && input$crispr_uniqueness != "All") sprintf(" AND cg.uniqueness = '%s'", input$crispr_uniqueness) else ""
    q <- sprintf("SELECT cg.gene_name, a.accession_name, a.species, cg.rank_in_gene, cg.spacer_seq, cg.pam_seq, cg.chrom, cg.spacer_start, cg.cut_site, cg.gc_pct, cg.uniqueness, cg.specificity_class FROM crispr_guides cg JOIN accessions a ON cg.accession_id = a.accession_id WHERE (cg.gene_name ILIKE '%%%s%%' OR cg.spacer_seq ILIKE '%%%s%%') %s %s ORDER BY cg.gene_name, cg.rank_in_gene LIMIT 1000", input$crispr_query, input$crispr_query, acc_filter, uniq_filter)
    datatable(dbGetQuery(con, q), options = list(pageLength = 20, scrollX = TRUE))
  })

  # ---------- AI Assistant ----------
  ai_history <- reactiveVal(list())
  observeEvent(input$ai_ask, {
    req(input$ai_question)
    q <- trimws(input$ai_question)
    if (nchar(q) == 0) return()
    h <- ai_history()
    h <- c(h, list(list(role = "user", text = q)))
    ai_history(h)
    out <- ask_groq_for_sql(q)
    if (!out$ok) {
      h <- c(h, list(list(role = "bot", error = out$error)))
      ai_history(h); return()
    }
    sql <- out$sql
    con <- connect_db(); on.exit(dbDisconnect(con), add = TRUE)
    result <- tryCatch(dbGetQuery(con, sql), error = function(e) paste("SQL error:", e$message))
    h <- c(h, list(list(role = "bot", sql = sql, result = result)))
    ai_history(h)
    updateTextAreaInput(session, "ai_question", value = "")
  })
  observeEvent(input$ai_clear, { ai_history(list()) })
  output$ai_conversation <- renderUI({
    h <- ai_history()
    if (length(h) == 0) {
      return(div(style = "color:#7a9587; font-style:italic; padding:20px; text-align:center;", "No questions yet. Try one above!"))
    }
    blocks <- lapply(seq_along(h), function(i) {
      m <- h[[i]]
      if (m$role == "user") {
        div(class = "ai-msg-user", shiny::icon("user"), " ", m$text)
      } else if (!is.null(m$error)) {
        div(class = "ai-msg-bot", shiny::icon("robot"), " ", div(style = "color:#8b1a1a;", strong("Error: "), m$error))
      } else {
        tbl_html <- ""
        if (is.data.frame(m$result) && nrow(m$result) > 0) {
          tbl_html <- datatable(m$result, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        } else if (is.character(m$result)) {
          tbl_html <- div(style = "color:#8b1a1a;", m$result)
        } else {
          tbl_html <- div(style = "color:#7a9587; font-style:italic;", "No rows returned.")
        }
        div(class = "ai-msg-bot", shiny::icon("robot"), " ",
          div(style = "font-size:0.85rem; color:#5a6b62; margin-top:4px;",
              strong("Generated SQL:"), div(class = "ai-sql", m$sql)),
          div(style = "margin-top:8px;", tbl_html))
      }
    })
    do.call(tagList, blocks)
  })

  # ---------- Statistics ----------
  output$stat_genes_per_acc <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, gene_count::text AS gc FROM accessions ORDER BY gene_count DESC")
    df$gc <- as.numeric(df$gc); par(mar = c(5, 14, 3, 1))
    barplot(df$gc, names.arg = df$accession_name, las = 1, horiz = TRUE, col = "#1a4d38", main = "Genes per Accession", xlab = "Gene count", cex.names = 0.8)
  })
  output$stat_gc <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, gc_content::text AS gc FROM accessions ORDER BY gc_content DESC")
    df$gc <- as.numeric(df$gc); par(mar = c(5, 14, 3, 1))
    barplot(df$gc, names.arg = df$accession_name, las = 1, horiz = TRUE, col = "#f5b342", main = "GC Content per Accession (%)", xlab = "GC %", cex.names = 0.8)
  })
  output$stat_genes_per_chrom <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT c.contig_name, a.accession_name, COUNT(*)::text AS n FROM genes g JOIN contigs c ON g.contig_id = c.contig_id JOIN accessions a ON g.accession_id = a.accession_id GROUP BY c.contig_name, a.accession_name")
    df$n <- as.numeric(df$n)
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return() }
    tab <- xtabs(n ~ contig_name + accession_name, data = df); pal <- rainbow(ncol(tab))
    par(mar = c(5, 5, 3, 12), xpd = TRUE)
    barplot(t(tab), col = pal, border = NA, main = "Genes per Chromosome", xlab = "Chromosome", ylab = "Gene count", las = 2, cex.names = 0.7)
    legend("topright", inset = c(-0.18, 0), legend = colnames(tab), fill = pal, bty = "n", cex = 0.65, xpd = TRUE)
  })
  output$stat_cluster_types <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT cluster_type, COUNT(*)::text AS n FROM pan_gene_clusters GROUP BY cluster_type ORDER BY COUNT(*) DESC")
    df$n <- as.numeric(df$n)
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return() }
    pal <- c("#8b5e9b","#d48c1a","#8fa7b3","#1a4d38","#f5b342")
    par(mar = c(5, 14, 3, 1))
    barplot(df$n, names.arg = df$cluster_type, las = 1, horiz = TRUE, col = pal[seq_len(nrow(df))], main = "Cluster Type Distribution", xlab = "Count", cex.names = 0.85)
  })
  output$stat_contig_len <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT contig_length::text AS len FROM contigs WHERE contig_length > 0")
    df$len <- as.numeric(df$len) / 1e6
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return() }
    par(mar = c(5, 5, 3, 1))
    hist(df$len, breaks = 30, col = "#2b7a5e", border = "white", main = "Contig Length Distribution", xlab = "Length (Mb)", ylab = "Number of contigs")
  })

  # ---------- Assembly Quality ----------
  output$qc_genome_size <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, genome_size::text AS v FROM accessions ORDER BY genome_size DESC")
    df$v <- as.numeric(df$v) / 1e6; par(mar = c(5, 14, 3, 1))
    barplot(df$v, names.arg = df$accession_name, las = 1, horiz = TRUE, col = "#2b7a5e", main = "Genome Size (Mb)", xlab = "Mb", cex.names = 0.75)
  })
  output$qc_n50 <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, n50::text AS v FROM accessions WHERE n50 IS NOT NULL ORDER BY n50 DESC")
    df$v <- as.numeric(df$v) / 1e6
    if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No N50 data"); return() }
    par(mar = c(5, 14, 3, 1))
    barplot(df$v, names.arg = df$accession_name, las = 1, horiz = TRUE, col = "#8b5e9b", main = "N50 (Mb)", xlab = "Mb", cex.names = 0.75)
  })
  output$qc_gc <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, gc_content::text AS v FROM accessions ORDER BY gc_content DESC")
    df$v <- as.numeric(df$v); par(mar = c(5, 14, 3, 1))
    barplot(df$v, names.arg = df$accession_name, las = 1, horiz = TRUE, col = "#f5b342", main = "GC Content (%)", xlab = "GC %", cex.names = 0.75)
  })
  output$qc_table <- renderDT({
    con <- connect_db(); on.exit(dbDisconnect(con))
    df <- dbGetQuery(con, "SELECT accession_name, species, genome_size, n50, gc_content, contig_count, gene_count FROM accessions ORDER BY genome_size DESC")
    datatable(df, options = list(pageLength = 15, scrollX = TRUE))
  })

  # ---------- Species Comparison ----------
  shared_matrix <- reactive({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    pa  <- dbGetQuery(con, "SELECT cluster_id, accession_id FROM gene_presence_absence WHERE is_present = TRUE")
    sets <- split(pa$cluster_id, pa$accession_id)
    acc_order <- acc$accession_id; acc_names <- acc$accession_name; n <- length(acc_order)
    mat <- matrix(0L, n, n, dimnames = list(acc_names, acc_names))
    for (i in seq_len(n)) for (j in seq_len(n)) {
      si <- sets[[as.character(acc_order[i])]]; sj <- sets[[as.character(acc_order[j])]]
      mat[i, j] <- length(intersect(si, sj))
    }
    list(mat = mat, names = acc_names)
  })
  output$species_matrix_plot <- renderPlot({
    m <- shared_matrix(); mat <- m$mat; n <- nrow(mat)
    par(mar = c(8, 14, 4, 2), xpd = TRUE)
    plot(NA, xlim = c(0, n), ylim = c(0, n), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", main = "Shared Pan-Gene Clusters Between Accessions")
    maxv <- max(mat)
    for (i in 1:n) for (j in 1:n) {
      v <- mat[i, j]
      col <- if (i == j) "#08261a" else { intensity <- v / maxv; rgb(0.9 - 0.75*intensity, 0.95 - 0.5*intensity, 0.9 - 0.6*intensity) }
      rect(j - 1, n - i, j, n - i + 1, col = col, border = "white")
      text_col <- if (i == j || v > maxv * 0.6) "white" else "#08261a"
      text(j - 0.5, n - i + 0.5, labels = formatC(v, format="d", big.mark=","), cex = 0.55, col = text_col, font = 2)
    }
    axis(1, at = (1:n) - 0.5, labels = m$names, las = 2, tick = FALSE, cex.axis = 0.7)
    axis(2, at = (n:1) - 0.5, labels = m$names, las = 2, tick = FALSE, cex.axis = 0.7)
  })
  output$species_matrix_table <- renderDT({
    m <- shared_matrix(); df <- as.data.frame(m$mat); df <- cbind(Accession = rownames(df), df)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE))
  })

  # ---------- Heatmap ----------
  output$presence_heatmap <- renderPlot({
    con <- connect_db(); on.exit(dbDisconnect(con))
    type_filter <- input$heatmap_type
    where_clause <- if (!is.null(type_filter) && type_filter != "All") paste0("WHERE pc.cluster_type = '", type_filter, "'") else ""
    clu <- dbGetQuery(con, paste0("SELECT pc.cluster_id, pc.cluster_name, pc.cluster_type, pc.presence_across_accessions::text AS pres FROM pan_gene_clusters pc ", where_clause, " ORDER BY pc.presence_across_accessions DESC, pc.gene_count DESC, pc.cluster_name LIMIT 100"))
    if (nrow(clu) == 0) { plot.new(); text(0.5, 0.5, "No clusters", cex = 1.4); return() }
    acc <- dbGetQuery(con, "SELECT accession_id, accession_name FROM accessions ORDER BY species, accession_name")
    acc_order <- acc$accession_name
    pa <- dbGetQuery(con, "SELECT gpa.cluster_id, a.accession_name FROM gene_presence_absence gpa JOIN accessions a ON gpa.accession_id = a.accession_id WHERE gpa.is_present = TRUE")
    mat <- matrix(0, nrow = nrow(clu), ncol = length(acc_order))
    rownames(mat) <- clu$cluster_name; colnames(mat) <- acc_order
    for (i in seq_len(nrow(pa))) {
      r <- which(clu$cluster_id == pa$cluster_id[i]); cc <- which(acc_order == pa$accession_name[i])
      if (length(r) == 1 && length(cc) == 1) mat[r, cc] <- 1
    }
    n <- nrow(mat); m <- ncol(mat)
    par(mar = c(7, 20, 3, 1))
    plot(NA, xlim = c(0, m), ylim = c(0, n), xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = paste0("Top 100 ", input$heatmap_type, " clusters"), bty = "n")
    for (i in 1:n) for (j in 1:m) {
      rect(j - 1, i - 1, j, i, col = if (mat[i, j] == 1) "#1a4d38" else "#e6eae8", border = "white", lwd = 0.5)
    }
    axis(1, at = (1:m) - 0.5, labels = colnames(mat), las = 2, cex.axis = 0.75, tick = FALSE)
    axis(2, at = (1:n) - 0.5, labels = rownames(mat), las = 2, cex.axis = 0.55, tick = FALSE)
    legend("topright", inset = c(-0.22, 0), legend = c("Present", "Absent"), fill = c("#1a4d38", "#e6eae8"), bty = "n", cex = 0.8, xpd = TRUE)
  })

  # ---------- Downloads ----------
  output$dl_genes <- downloadHandler("genes.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM genes"), f, row.names = FALSE) })
  output$dl_clusters <- downloadHandler("pan_gene_clusters.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM pan_gene_clusters"), f, row.names = FALSE) })
  output$dl_accessions <- downloadHandler("accessions.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM accessions"), f, row.names = FALSE) })
  output$dl_presence <- downloadHandler("gene_presence_absence.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM gene_presence_absence"), f, row.names = FALSE) })
  output$dl_go <- downloadHandler("go_annotations.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM go_annotations LIMIT 100000"), f, row.names = FALSE) })
  output$dl_kegg <- downloadHandler("kegg_ko.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM kegg_ko"), f, row.names = FALSE) })
  output$dl_path <- downloadHandler("kegg_pathways.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM kegg_pathways LIMIT 100000"), f, row.names = FALSE) })
  output$dl_kog <- downloadHandler("kog_annotations.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM kog_annotations"), f, row.names = FALSE) })
  output$dl_crispr <- downloadHandler("crispr_guides.csv", function(f) { con <- connect_db(); on.exit(dbDisconnect(con)); write.csv(dbGetQuery(con, "SELECT * FROM crispr_guides LIMIT 500000"), f, row.names = FALSE) })
}

shinyApp(ui, server)