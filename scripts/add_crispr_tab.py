# scripts/add_crispr_tab.py
# Add CRISPR tab + AI schema update to app.R

import os

APP = r"C:\Users\Erick.Amombo\alfalfa_pangenome\app.R"

with open(APP, 'r', encoding='utf-8') as f:
    content = f.read()

# ============================================================
# 1. Add CRISPR tab to the UI (before the AI Assistant tab)
# ============================================================
crispr_tab = '''
        # ============================================================
        # CRISPR GUIDES
        # ============================================================
        tabPanel("CRISPR Guides",
          br(), h3("\\U0001F9EC CRISPR Guide RNA Browser"),
          helpText("3.5 million guide RNAs targeting 518,858 genes across 12 accessions."),
          br(),
          fluidRow(
            column(4,
              textInput("crispr_query", "Search by gene name or guide sequence:",
                        placeholder = "e.g. Mara000001 or GTTCAACCTGT")
            ),
            column(4,
              selectInput("crispr_accession", "Filter by accession:",
                          choices = c("All"), selected = "All")
            ),
            column(4,
              selectInput("crispr_uniqueness", "Filter by uniqueness:",
                          choices = c("All", "UNIQUE", "MULTI"), selected = "All")
            )
          ),
          br(),
          h4("Matching Guides"),
          DTOutput("crispr_table")
        ),
'''

marker = '''        # ============================================================
        # AI ASSISTANT
        # ============================================================'''
content = content.replace(marker, crispr_tab + '\n' + marker, 1)

# ============================================================
# 2. Add CRISPR Download button
# ============================================================
old_dl = '''          downloadButton("dl_kog",  "KOG (CSV)")),'''
new_dl = '''          downloadButton("dl_kog",  "KOG (CSV)"),
          br(), br(),
          downloadButton("dl_crispr", "CRISPR Guides (CSV)")),'''
content = content.replace(old_dl, new_dl, 1)

# ============================================================
# 3. Update AI schema prompt
# ============================================================
old_schema = '''  kog_annotations(kog_id, gene_id, accession_id, cog_letter)'''
new_schema = '''  kog_annotations(kog_id, gene_id, accession_id, cog_letter)
  crispr_guides(guide_id, gene_id, accession_id, genome, gene_name,
                rank_in_gene, chrom, strand, gene_strand, spacer_seq, pam_seq,
                spacer_start, spacer_end, pam_start, pam_end, cut_site,
                gc_pct, n_genome_hits, uniqueness, specificity_class)'''
content = content.replace(old_schema, new_schema, 1)

old_notes = '''- 5.8M GO annotations, 449K KEGG KOs, 2M KEGG pathways, 856K KOG'''
new_notes = '''- 5.8M GO annotations, 449K KEGG KOs, 2M KEGG pathways, 856K KOG
- 3.56M CRISPR guides (3.5M UNIQUE, 48K MULTI) targeting 518,858 genes
- CRISPR fields: spacer_seq=20nt guide, pam_seq=PAM, uniqueness=UNIQUE/MULTI,
  gc_pct=GC%, rank_in_gene=rank among guides for that gene'''
content = content.replace(old_notes, new_notes, 1)

# ============================================================
# 4. Add CRISPR server logic (before AI ASSISTANT server section)
# ============================================================
crispr_server = '''
  # ============================================================
  # CRISPR GUIDES
  # ============================================================
  observe({
    con <- connect_db(); on.exit(dbDisconnect(con))
    acc <- dbGetQuery(con, "SELECT accession_name FROM accessions ORDER BY accession_name")$accession_name
    updateSelectInput(session, "crispr_accession", choices = c("All", acc))
  })

  output$crispr_table <- renderDT({
    req(input$crispr_query)
    con <- connect_db(); on.exit(dbDisconnect(con))

    acc_filter <- ""
    if (!is.null(input$crispr_accession) && input$crispr_accession != "All") {
      acc_filter <- sprintf(" AND a.accession_name = '%s'", input$crispr_accession)
    }

    uniq_filter <- ""
    if (!is.null(input$crispr_uniqueness) && input$crispr_uniqueness != "All") {
      uniq_filter <- sprintf(" AND cg.uniqueness = '%s'", input$crispr_uniqueness)
    }

    q <- sprintf("
      SELECT cg.gene_name, a.accession_name, a.species,
             cg.rank_in_gene, cg.spacer_seq, cg.pam_seq,
             cg.chrom, cg.spacer_start, cg.cut_site,
             cg.gc_pct, cg.uniqueness, cg.specificity_class
      FROM crispr_guides cg
      JOIN accessions a ON cg.accession_id = a.accession_id
      WHERE (cg.gene_name ILIKE '%%%s%%' OR cg.spacer_seq ILIKE '%%%s%%')
      %s %s
      ORDER BY cg.gene_name, cg.rank_in_gene
      LIMIT 1000
    ", input$crispr_query, input$crispr_query, acc_filter, uniq_filter)

    datatable(dbGetQuery(con, q), options = list(pageLength = 20, scrollX = TRUE))
  })

'''

marker_server = '''  # ============================================================
  # AI ASSISTANT
  # ============================================================'''
content = content.replace(marker_server, crispr_server + '\n' + marker_server, 1)

# ============================================================
# 5. Add CRISPR download handler (before the final closing brace)
# ============================================================
crispr_dl = '''
  output$dl_crispr <- downloadHandler("crispr_guides.csv", function(f) {
    con <- connect_db(); on.exit(dbDisconnect(con))
    write.csv(dbGetQuery(con, "SELECT * FROM crispr_guides LIMIT 500000"), f, row.names = FALSE)
  })
'''

marker_end = '''}

# ============================================================
shinyApp(ui, server)'''

content = content.replace(marker_end, crispr_dl + '\n' + marker_end, 1)

# ============================================================
# Write back
# ============================================================
with open(APP, 'w', encoding='utf-8') as f:
    f.write(content)

print("SUCCESS: app.R updated with CRISPR tab, download, and AI schema")
print("File size:", os.path.getsize(APP), "bytes")