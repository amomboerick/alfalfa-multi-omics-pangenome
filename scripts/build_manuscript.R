# ============================================================
# build_manuscript.R
# Rebuild AlfaPan.docx with proper TPJ-style structure
# Output: manuscript/AlfaPan_v2.docx
# ============================================================

suppressPackageStartupMessages({
  library(officer)
})

setwd("C:/Users/Erick.Amombo/alfalfa_pangenome/manuscript")

doc <- read_docx()

# ---- Helper functions ----
H1 <- function(text) block_list(fpar(ftext(text, fp_text(font.size = 14, bold = TRUE)), style = "heading 1"))
H2 <- function(text) block_list(fpar(ftext(text, fp_text(font.size = 12, bold = TRUE)), style = "heading 2"))
P  <- function(text) block_list(fpar(ftext(text, fp_text(font.size = 12))))

# ============================================================
# FRONT MATTER
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("AlfaPan: An AI-Enabled Multi-Omics Pan-Genome of Medicago with Insights into Gene Family Evolution, Homeolog Retention, and Functional Adaptation",
             fp_text(font.size = 16, bold = TRUE))),
  style = "centered")

doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("[AUTHOR NAMES]",
             fp_text(font.size = 12, bold = TRUE))),
  style = "Normal")

doc <- body_add_par(doc, "[AFFILIATION 1]", style = "Normal")
doc <- body_add_par(doc, "[AFFILIATION 2 — if applicable, otherwise delete]", style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Correspondence: ", fp_text(font.size = 12, bold = TRUE)),
       ftext("[CORRESPONDING AUTHOR EMAIL]", fp_text(font.size = 12))),
  style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("ORCID: ", fp_text(font.size = 12, bold = TRUE)),
       ftext("[ORCID]", fp_text(font.size = 12))),
  style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Running head: ", fp_text(font.size = 12, bold = TRUE)),
       ftext("[RUNNING HEAD — max 60 characters]", fp_text(font.size = 12))),
  style = "Normal")

doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# ABSTRACT
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Summary", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc,
  "Alfalfa (Medicago sativa L.) is the world's most important perennial forage legume, yet its autotetraploid genome and extensive genetic diversity remain difficult to navigate for functional genomics and crop improvement. Here we present AlfaPan, a comprehensive AI-enabled multi-omics pan-genome resource integrating genomic, functional annotation, and CRISPR guide RNA data across 12 Medicago accessions representing six species. The resource comprises 823,838 annotated genes, 217,122 pan-gene clusters, 5.8 million Gene Ontology annotations, 449,222 KEGG KO assignments, 2 million KEGG pathway annotations, 855,743 KOG categories, and 3.56 million CRISPR guide RNAs targeting 518,858 genes. To improve accessibility for researchers without bioinformatics expertise, AlfaPan incorporates a natural language query interface that translates plain English questions into SQL queries using a large language model (Qwen 3.8 27B via Groq), achieving a 96.7% successful execution rate across validated test queries. Beyond the database, we demonstrate its utility through a suite of comparative genomic analyses. We identify 2,300 gene families expanded in tetraploid M. sativa relative to its diploid ancestor, significantly enriched for DNA repair, homologous recombination, and MAPK signaling functions. We show that core genes exhibit 2.2-fold higher copy number variation than dispensable genes, and that private genes are significantly less CRISPR-targetable (95.4% versus 99.3% UNIQUE guide rates, p = 6.5e-44). We identify 895 homeolog-retained clusters with tetraploid/diploid ratios of 7.7–8.2, representing priority loci for expression asymmetry studies. A maximum-likelihood phylogeny based on 561 single-copy orthologs (105,518 aligned amino acid columns) resolves the relationships among the 12 accessions with bootstrap support of 89–100 at all major nodes. Integration of specificity, functional annotation, and editability metrics prioritizes 592 stress-adaptive, editable candidate genes for functional validation. AlfaPan is freely available under MIT and CC-BY 4.0 licenses, providing the Medicago research community with an integrated platform for comparative genomics, functional annotation, and genome editing applications.",
  style = "Normal")

doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Keywords: ", fp_text(font.size = 12, bold = TRUE)),
       ftext("Medicago sativa, pan-genome, multi-omics database, CRISPR guide RNA, polyploidy, homeolog retention, natural language interface, comparative genomics",
             fp_text(font.size = 12))),
  style = "Normal")

doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# SIGNIFICANCE STATEMENT
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Significance Statement", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc,
  "AlfaPan integrates genomic, functional, and CRISPR data across 12 Medicago accessions into a queryable AI-enabled resource and reveals through comparative analyses that autotetraploid alfalfa has preferentially retained and expanded gene families involved in DNA repair and stress signaling. The resource provides both a database with natural language access and a suite of analytical tools that identify homeolog-retained candidates and prioritize stress-adaptive genes for functional validation in forage crop improvement.",
  style = "Normal")

doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# INTRODUCTION
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Introduction", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

intro_paras <- c(
  "Alfalfa (Medicago sativa L.) occupies a unique position in global agriculture as the most widely cultivated perennial forage legume, earning the designation \"Queen of Forages\" due to its exceptional nutritional quality, nitrogen-fixing capacity, and beneficial effects on soil ecology (Small, 2011). Its role as a cornerstone of sustainable livestock production is well-established, with global cultivation exceeding 30 million hectares. China alone requires substantial quantities of alfalfa hay to support its expanding dairy industry, yet domestic production remains insufficient, creating a strategic dependency on imports (Shen et al., 2020). Developing improved alfalfa cultivars with higher yield, stress tolerance, and nutritional value is therefore a critical priority for agricultural security, and genomic resources that accelerate breeding efforts are urgently needed.",
  "The genus Medicago comprises 87 species spanning diploid, tetraploid, and hexaploid cytotypes adapted to diverse geographic and climatic conditions (Small, 2011). Within this genus, the model diploid species M. truncatula has served as a reference for legume biology for two decades, while cultivated alfalfa (M. sativa subsp. sativa) represents the autotetraploid lineage of greatest agronomic importance (Young et al., 2011). Compared to its diploid ancestor (M. sativa subsp. caerulea), cultivated tetraploid alfalfa exhibits superior yield, enhanced stress tolerance, and broader environmental adaptation (Biazzi et al., 2017; Biazzi et al., 2021). However, the genomic basis of these adaptive advantages has remained difficult to dissect due to the complexity of the autotetraploid genome, which combines high heterozygosity, extensive repetitive sequences, and dynamic subgenome interactions.",
  "Recent advances in long-read sequencing technologies have enabled the generation of haplotype-resolved and allele-aware assemblies for complex polyploid genomes (Chen et al., 2020; Long et al., 2022). The release of chromosome-scale Medicago genomes, including M. truncatula, M. polymorpha, M. ruthenica, and multiple M. sativa subspecies, has created new opportunities for comparative and pan-genome analyses (Tang et al., 2014; Cui et al., 2021; Shang et al., 2022). A recent super-pangenome integrating 13 genomes from seven taxa has revealed that core genes retained on all four allelic copies in autotetraploid alfalfa are enriched for climate-adaptation-associated functions, while paradoxically carrying a high genetic burden from deleterious mutations (Zhang et al., 2025). Further work has shown that tetraploid core genes exhibit higher overall expression across all four alleles, but that one allele is typically more highly expressed than the others—a phenomenon known as homeolog expression bias—suggesting subgenome-level regulatory divergence following polyploidization (Chen et al., 2024).",
  "The concept of the pan-genome—the complete set of genes present across all individuals of a species—has emerged as a powerful framework for capturing genetic diversity that is missed by single-reference approaches (Tettelin et al., 2005; Bayer et al., 2020). In crops such as soybean, maize, and rice, pan-genome analyses have revealed extensive presence/absence variation (PAV) affecting agronomically important traits, with dispensable genes often enriched for functions related to biotic and abiotic stress responses (Li et al., 2014; Gao et al., 2019; Liu et al., 2020). Super-pangenomes, which integrate wild relatives of a crop species, provide an expanded framework for germplasm conservation and for discovering unique alleles through accelerated breeding approaches (Khan et al., 2020). These frameworks have not yet been systematically applied to Medicago, leaving a significant opportunity for a comprehensive multi-omics pan-genome resource.",
  "The availability of genome-wide CRISPR guide RNA designs targeting hundreds of thousands of genes provides a foundation for functional validation studies in alfalfa (Zhang et al., 2021). The design of guide RNAs for plant genome editing requires careful consideration of PAM accessibility, scaffold sequences, gRNA and Cas9 concentrations, epigenetic features, and off-target prediction. However, no Medicago resource currently integrates CRISPR guide designs with pan-genome context and functional annotations in a way that allows researchers to prioritize targets based on conservation, specificity, and annotation simultaneously.",
  "Despite these advances, alfalfa research has lagged in developing integrated bioinformatics resources. While multi-omics databases exist for model crops such as barley (BarleyOmics), peanut (PeanutOmics), and tobacco (NMOD), no comprehensive platform integrates the diverse data types now available for Medicago. Existing resources such as MODMS (Fang et al., 2024) and MsGVD have provided valuable genomic data, but none have combined the breadth of multi-omics data with an accessible natural language query interface. Meanwhile, the volume and complexity of genomic data now available for Medicago has grown beyond what most researchers can efficiently navigate using command-line tools, creating a barrier to entry for experimental biologists who would benefit most from the data.",
  "Here we describe AlfaPan, a multi-omics pan-genome database for Medicago sativa with an integrated AI-powered natural language query interface. The database addresses three key needs: (1) centralized access to multi-omics data for the Medicago research community, (2) computational tools for comparative and functional analysis, and (3) accessibility for researchers without bioinformatics expertise through plain English queries. We demonstrate its utility through a suite of seven comparative genomic analyses addressing gene family expansion, homeolog retention, copy number variation, CRISPR targetability, chromosomal distribution, phylogenetic relationships, and adaptive priority ranking. Together, these analyses reveal genome-wide signatures of polyploidy-driven genome evolution and identify high-priority candidate genes for functional validation in alfalfa improvement."
)
for (p in intro_paras) { doc <- body_add_par(doc, p, style = "Normal") }

doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# MATERIALS AND METHODS
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Materials and Methods", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

methods <- list(
  H2 = c("Database Architecture and Implementation", "R Shiny Application",
         "Natural Language Query Interface", "Data Sources and Processing",
         "Comparative Genomic Analyses"),
  P = list(
    "Database Architecture and Implementation" = c(
      "AlfaPan was implemented using PostgreSQL 15 as the relational database management system, chosen for its robustness, support for complex queries, and mature ecosystem of R integration packages. The database schema comprises 11 primary tables designed to capture distinct data types while maintaining referential integrity through foreign key relationships.",
      "The accessions table stores metadata for the 12 Medicago accessions, including species, subspecies, cultivar names, genome size, GC content, N50 contig length, contig count, and gene count. The genes table contains 823,838 annotated gene models with chromosomal coordinates, strand orientation, gene symbols, biotypes, and functional descriptions. The contigs table stores chromosome and scaffold metadata. The pan_gene_clusters table defines 217,122 homologous gene families categorized by cluster type: core (present in all accessions), soft_core (present in most accessions), dispensable (present in a subset), private (unique to one accession), and singleton (single-copy genes). The cluster_membership and gene_presence_absence tables link genes to clusters and record presence/absence patterns across accessions.",
      "Functional annotation data are stored in four specialized tables: go_annotations (5.8 million Gene Ontology term assignments), kegg_ko (449,222 KEGG Orthology identifiers), kegg_pathways (2 million pathway annotations), and kog_annotations (855,743 KOG/COG functional categories). The crispr_guides table contains 3.56 million guide RNA designs targeting 518,858 genes, with fields including spacer sequence, PAM sequence, genomic coordinates, GC content, uniqueness classification (UNIQUE/MULTI), and specificity class.",
      "All tables are indexed on their primary keys and frequently joined columns. The database was tuned using standard PostgreSQL parameters for read-heavy workloads, and all queries are executed read-only from the analysis scripts."
    ),
    "R Shiny Application" = c(
      "The web interface was developed using the R Shiny framework. The user interface comprises 16 tabbed panels providing distinct analytical functions: Home (summary statistics and pan-genome overview figure), Accessions (sortable, filterable metadata table), Pan-Gene Clusters (browser with type filtering and interactive detail modals), Top Gene Families (ranking with member gene display), Presence/Absence (accession-specific presence queries), Search (gene name search with cluster membership details), Download (bulk CSV export of all tables), GO Browser (Gene Ontology term distribution and annotation queries), KEGG Pathways (KO number and pathway distribution), KOG Classes (COG functional category distribution), Annotation Search (unified search across all annotation types), CRISPR Guides (browser with search and filtering), AI Assistant (natural language query), Statistics (genome-wide statistics and visualizations), Assembly Quality (metrics comparison), Species Comparison (cross-accession cluster sharing), and Heatmap (presence/absence visualization).",
      "Data tables are rendered using the DT package, providing sorting, filtering, and pagination capabilities. Interactive modals display detailed cluster and gene information on user selection, with presence/absence matrices and complete member gene lists. The application is deployed on a local R Shiny server and can be accessed through any modern web browser."
    ),
    "Natural Language Query Interface" = c(
      "To improve accessibility for researchers without SQL expertise, we implemented a natural language query interface that leverages a large language model (Groq-hosted Qwen 3.8 27B) to translate user questions into database queries. The system architecture follows a three-step pipeline.",
      "First, in schema-aware SQL generation, user questions are submitted to the language model along with a comprehensive schema description including table names, column definitions, data types, and example values. The model returns SQL queries in JSON format, which enforces structured output and eliminates the risk of markdown formatting artifacts. The system prompt includes specific guidance to avoid common errors, such as using incorrect table names or omitting necessary JOIN clauses.",
      "Second, query execution is performed against the PostgreSQL database using DBI and RPostgres packages. Queries are validated for SQL injection safety before execution, and result sets are limited to 100 rows to prevent memory issues.",
      "Third, result presentation returns query results to the user interface as interactive data tables. The generated SQL is shown alongside results for transparency, allowing users to learn SQL syntax through example.",
      "User prompts are processed with a temperature setting of 0.1 to maximize determinism. The interface was tested against 30 gold-standard question-SQL pairs and achieved a successful execution rate of 96.7%."
    ),
    "Data Sources and Processing" = c(
      "Genomic and annotation data for the 12 Medicago accessions were obtained from publicly available assemblies. Gene models were processed through a standardized annotation pipeline to ensure consistency across accessions. Orthologous gene families were inferred using OrthoFinder (Emms and Kelly, 2019), and presence/absence patterns were derived from cluster membership assignments. Recent studies have demonstrated that identifying allelic copies based on a 95% protein sequence identity threshold effectively distinguishes true allelic variation from homologous genes (Chen et al., 2024).",
      "CRISPR guide RNA designs were generated using a custom computational pipeline that scans each gene for protospacer adjacent motif (PAM) sites (NGG), evaluates spacer sequence properties including GC content and self-complementarity, and assesses genome-wide specificity through sequence alignment. Guides were classified as UNIQUE (single genomic hit) or MULTI (multiple hits), with specificity classes assigned based on off-target prediction scores."
    ),
    "Comparative Genomic Analyses" = c(
      "Retention bias analysis. Diploid M. sativa subsp. caerulea accessions (n=2) were compared to tetraploid M. sativa accessions (n=3). For each pan-gene cluster, mean gene count per accession was computed, and the retention ratio was calculated as the ratio of tetraploid mean to diploid mean. Clusters were binned as lost (< 0.5), neutral (0.5–1.5), expanded (1.5–3), or highly expanded (> 3). Functional enrichment analysis was performed by Fisher's exact test with Benjamini–Hochberg correction against the full annotated gene set.",
      "Species-specific expansion analysis. Accessions were assigned to seven groups: sativa_tetraploid (XJDY, ZM1, ZM4_hap4), sativa_diploid (caerulea_landa, caerulea_long), ruthenica (landa, zhiwusuo), truncatula (A17, HM078), arabica (1 accession), lupulina (1 accession), and polymorpha (1 accession). Mean gene count per accession per cluster was computed for each group. Clusters with ≥3× the pan-genome mean in a given species were classified as species-expanded.",
      "Copy number variation analysis. The coefficient of variation (CV) was calculated as the standard deviation of gene count across accessions divided by the mean, for each cluster with ≥3 genes and presence in ≥3 accessions. Kruskal–Wallis tests compared CV distributions across cluster types.",
      "CRISPR targetability analysis. For each cluster, the percentage of genes with at least one UNIQUE guide was computed. Kruskal–Wallis and pairwise Wilcoxon tests were used to compare targetability across cluster types.",
      "Chromosomal distribution analysis. Genes were binned into 1 Mb windows per chromosome per accession. The proportion of core, dispensable, and private genes was calculated per bin, and bins were classified as chromosome edges (0–20%, 80–100%) or centers (20–80%). Only bins with ≥5 classified genes were retained.",
      "Phylogenetic analysis. Single-copy orthologs were identified as clusters with exactly one gene in each of the 12 accessions. The longest protein per cluster per accession was selected as representative, and protein sequences were extracted with four accession-specific header normalization rules. Sequences were aligned with MAFFT (Katoh and Standley, 2013), trimmed by taking every 10th column to reduce computational load, and concatenated into a supermatrix (12 sequences, 105,518 columns, 22,736 parsimony-informative sites). Maximum-likelihood analysis was performed with IQ-TREE 2 (Minh et al., 2020) using the LG+I+G substitution model with 1,000 ultrafast bootstrap replicates.",
      "Composite Adaptive Priority Index. A composite index was computed as: API = 0.4 × specificity + 0.4 × stress_GO + 0.2 × editability, where specificity = 1 − (n_accessions_present / 12), stress_GO = presence of stress-related GO annotations, and editability = log-normalized count of UNIQUE guides. Stress-related GO terms included GO:0006950 (response to stress), GO:0006952 (defense response), GO:0006979 (response to oxidative stress), GO:0009408 (response to heat), GO:0009409 (response to cold), GO:0009414 (response to water deprivation), and ten additional terms."
    )
  )
)
for (h in methods$H2) {
  doc <- body_add_fpar(doc, fpar(ftext(h, fp_text(font.size = 12, bold = TRUE))), style = "heading 2")
  for (p in methods$P[[h]]) { doc <- body_add_par(doc, p, style = "Normal") }
  doc <- body_add_par(doc, "", style = "Normal")
}

# ============================================================
# RESULTS
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Results", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

results <- list(
  "Phylogenetic Relationships" = c(
    "Maximum-likelihood phylogenetic analysis of 561 single-copy orthologs (539 retained after quality filtering) with 1,000 ultrafast bootstrap replicates resolved the relationships among the 12 accessions. The final alignment contained 105,518 amino acid columns with 22,736 parsimony-informative sites. All major nodes had bootstrap support of 89–100, indicating strong support. The topology placed M. polymorpha as the deepest-diverging lineage, with M. truncatula accessions (A17 and HM078) clustering together (bootstrap 100). Tetraploid M. sativa accessions nested within a clade containing the diploid M. caerulea ancestor, with ZM4_hap4 and ZM1 forming a tight subclade (bootstrap 100). M. ruthenica accessions clustered together (bootstrap 89), as did M. arabica and M. lupulina near the base of the tree. This topology is consistent with published Medicago phylogenies and confirms the utility of the single copy ortholog approach for resolving relationships in this genus."
  ),
  "Database Content and Statistics" = c(
    "AlfaPan integrates data across 12 Medicago accessions representing six species: M. sativa (five accessions including subsp. sativa, subsp. caerulea, and cultivars ZM1, ZM4, XJDY), M. truncatula (three accessions: A17, HM078, R108), M. ruthenica (two accessions), M. arabica, M. polymorpha, and M. lupulina. The complete dataset comprises 823,838 gene models, with gene counts per accession ranging from 39,497 (M. lupulina) to 168,043 (M. sativa ZM4 haplotype 4). Pan-gene clustering identified 217,122 homologous gene families across all accessions. Cluster sizes ranged from singletons (single genes) to large families exceeding 500 members. Cluster type distribution showed 561 core clusters (present in all accessions), 300 soft-core clusters (present in 11 of 12 accessions), 10,794 dispensable clusters, and 202,311 private/singleton clusters. The largest clusters were predominantly core genes, with the most abundant family comprising 500 genes. Functional annotation coverage was comprehensive: 5,832,931 GO term assignments, 449,222 KEGG KO identifiers, 2,025,290 KEGG pathway annotations, and 855,743 KOG categories. These annotations enable functional enrichment analysis and pathway-level interpretation of gene sets identified through database queries. The CRISPR guide RNA dataset comprises 3,559,072 guides targeting 518,858 genes. Guide classification identified 3,510,548 UNIQUE guides (single genomic hit, high specificity) and 48,524 MULTI guides (multiple genomic hits, requiring careful off-target assessment). The average number of guides per gene was 6.9, with rank-1 guides (highest on-target score) available for all targeted genes."
  ),
  "Web Interface Functionality" = c(
    "The R Shiny application provides interactive access to all data types through its 16 modules. Benchmarking on a standard Windows workstation showed query response times of less than 2 seconds for all interactive features, with the exception of the pan-genome heatmap visualization, which requires approximately 5 seconds to render for 100 clusters.",
    "The cluster detail modal, accessible by clicking any row in the Pan-Gene Clusters or Top Gene Families tables, displays the presence/absence matrix for all 12 accessions alongside the complete member gene list. This feature enables rapid assessment of gene family conservation and identification of lineage-specific gene losses. The gene detail modal displays gene metadata including accession, species, chromosomal position, and strand, together with the associated pan-gene cluster and its complete membership across accessions."
  ),
  "Natural Language Query Performance" = c(
    "The AI Assistant module was evaluated using a test set of 30 natural language questions spanning all database tables. Example queries and their generated SQL demonstrate the system's capabilities. The query \"How many core clusters are there?\" generated SELECT COUNT(*) FROM pan_gene_clusters WHERE cluster_type = 'core'; and returned 561. The query \"Show me the top 10 largest pan-gene families\" returned 10 rows with cluster sizes ranging from 500 to 500 genes. The query \"How many UNIQUE CRISPR guides are there?\" returned 3,510,548. The natural language interface achieved a 96.7% successful execution rate (29 of 30 queries) and 93.3% non-empty result rate. The single failed query involved an ambiguous reference to \"the best gene,\" which the model correctly interpreted as requiring additional specification."
  ),
  "Polyploidy-Driven Gene Retention Bias" = c(
    "Comparison of diploid M. caerulea and tetraploid M. sativa revealed extensive retention differences across the pan-genome. Of 217,122 pan-gene clusters, 4,137 were classified as lost (retention ratio < 0.5), 1,342 were neutral (0.5–1.5), 1,511 were expanded (1.5–3), and 789 were highly expanded (> 3). An additional 151,221 clusters were tetraploid-only (present in tetraploid M. sativa but absent from diploid M. caerulea), and 58,122 were absent from both ploidy groups (belonging to other Medicago species).",
    "Functional enrichment analysis of expanded clusters (n = 2,300) revealed significant enrichment (FDR < 0.05) for nucleic acid binding (46,422 genes), protein binding (39,868 genes), ATP binding (23,138 genes), zinc ion binding (23,290 genes), and catalytic activity (18,331 genes). KEGG pathway analysis showed enrichment for homologous recombination (5,088 genes), nucleotide excision repair (4,957 genes), MAPK signaling (4,916 genes), biosynthesis of secondary metabolites (21,750 genes), and microbial metabolism in diverse environments (5,306 genes). Lost clusters (n = 4,137) were enriched for protein binding (758 genes), nucleic acid binding (569 genes), and catalytic activity (420 genes).",
    "The enrichment of DNA repair and signaling functions in the expanded set suggests that the tetraploid genome has preferentially retained duplicates in pathways that enhance genomic stability and environmental sensing."
  ),
  "Species-Specific Gene Family Expansions" = c(
    "Analysis across seven species groups revealed substantial variation in the number of species-expanded gene families: sativa_tetraploid (83), truncatula (41), arabica (24), polymorpha (15), ruthenica (11), lupulina (4), and sativa_diploid (1). After accounting for differences in accession counts per group (via mean gene count per accession), the 83 expanded families in tetraploid M. sativa represent 7.7-fold more expansions than its own diploid ancestor (sativa_diploid, 1 family). This finding indicates that polyploidization has been accompanied by substantial gene family expansion beyond the simple duplication expected from tetraploidy."
  ),
  "Copy Number Variation Landscape" = c(
    "Analysis of 4,370 clusters with ≥3 genes and presence in ≥3 accessions revealed that core genes have significantly higher copy number variation than dispensable genes. Mean CV was 0.611 for core clusters (n = 561), 0.553 for soft_core clusters (n = 300), and 0.279 for dispensable clusters (n = 3,509). The proportion of clusters with CV > 1 was 4.28% for core, 4.00% for soft_core, and 1.40% for dispensable. The 2.2-fold higher CV in core versus dispensable clusters is counterintuitive but biologically meaningful. In the pan-genome sense, \"core\" genes are present in all accessions, but in autotetraploids, they are often present as multiple homeologs, resulting in high copy number variation across accessions. This reflects the dynamic nature of polyploid genomes, where subgenome dominance and homeolog silencing create substantial copy number variation even for conserved genes."
  ),
  "CRISPR Targetability Versus Conservation" = c(
    "Of 5,592 clusters with guides and ≥3 genes, core genes had 99.3% mean UNIQUE guide rate, soft_core 99.4%, dispensable 98.5%, and private 95.4%. Kruskal–Wallis test showed highly significant differences across cluster types (p = 1.88e-321), with pairwise comparisons showing that private genes are significantly less targetable than core genes (p = 6.51e-44). The practical implication is that genes most valuable for trait discovery—private, species-specific genes—are also the hardest to edit safely with CRISPR. This trade-off should inform the design of functional validation experiments, where the choice of target genes must balance biological priority against experimental tractability."
  ),
  "Chromosomal Distribution of Gene Classes" = c(
    "Binning genes into 1 Mb windows revealed that core genes enrich at chromosome edges (44.6% at ends versus 41.7% at centers), while private genes enrich at chromosome centers (30.9% at centers versus 26.6% at ends). Dispensable genes showed no significant end-versus-center bias (9.85% at ends, 10.1% at centers). This pattern is consistent with the \"euchromatic islands\" model observed in other plant genomes, where conserved essential genes cluster in gene-rich, recombination-active chromosome ends while variable genes occupy repeat-rich pericentromeric regions where recombination is suppressed. This is the first genome-wide confirmation of this pattern in Medicago."
  ),
  "Homeolog Retention Candidates" = c(
    "Of 217,122 pan-gene clusters, 895 showed ≥3 gene copies in each of three tetraploid M. sativa accessions while retaining ≥1 copy in the diploid ancestor. These clusters were predominantly core (547) and soft_core (239), with 106 dispensable. The median tetraploid/diploid ratio was 7.7 to 8.2, and the highest ratios reached 291 (291 genes in tetraploid versus 1 in diploid). These clusters represent priority loci for future RNA-seq validation of homeolog expression dominance, as the retention of multiple homeologs is a prerequisite for observing expression asymmetry."
  ),
  "Composite Adaptive Priority Index" = c(
    "Integration of specificity, stress annotation, and editability metrics identified 1,617 clusters with API > 0.5 and 592 clusters with API > 0.7. All top-ranked candidates were private clusters carrying stress-related GO annotations with abundant UNIQUE CRISPR guides, representing a prioritized shortlist of genes for functional validation. The top 10 candidates all belonged to M. ruthenica or tetraploid M. sativa, with API scores ranging from 0.850 to 0.873. Each candidate carried at least one stress-related GO annotation and had between 30 and 78 UNIQUE CRISPR guides available, making them immediately actionable for genome editing experiments."
  ),
  "Annotation Coverage Varies Systematically with Cluster Conservation" = c(
    "To test whether annotation density differed systematically across pan-gene cluster types, we quantified the proportion of genes with at least one functional annotation for each of three orthogonal annotation layers — Gene Ontology (GO), KEGG Orthology (KO), and euKaryotic Orthologous Groups (KOG) — across the four populated cluster types (core, soft_core, dispensable, private; 548,164 clustered genes total; singleton clusters excluded as they had no entries in the cluster membership table).",
    "Annotation coverage declined monotonically from conserved to variable clusters across all three layers (Figure 3). GO coverage ranged from 83.6% in soft_core clusters and 80.5% in core clusters to 70.2% in dispensable and 63.1% in private clusters. KEGG KO coverage followed the same pattern (55.7% soft_core, 52.5% core, 46.3% dispensable, 41.0% private). KOG coverage was uniformly higher — as expected given that KOG assignments rely on broader homology rather than specific functional evidence — but preserved the same gradient (94.7% soft_core, 92.9% core, 85.4% dispensable, 81.9% private).",
    "This conservation-dependent annotation gradient indicates that genes restricted to fewer accessions are systematically less well annotated than those present across the species complex. Two non-exclusive explanations are plausible: (i) conserved genes are more likely to have well-characterised homologues in reference databases such as Arabidopsis thaliana and Medicago truncatula, biasing annotation toward ancient, conserved functions; and (ii) dispensable and private genes may encode rapidly evolving or lineage-specific functions for which no curated annotation exists. Either way, this bias should be considered when interpreting functional enrichment results — particularly our analyses of retained versus lost gene families and species-specific expansions — since under-annotated dispensable genes may be invisible to GO- or KEGG-based tests."
  )
)
for (h in names(results)) {
  doc <- body_add_fpar(doc, fpar(ftext(h, fp_text(font.size = 12, bold = TRUE))), style = "heading 2")
  for (p in results[[h]]) { doc <- body_add_par(doc, p, style = "Normal") }
  doc <- body_add_par(doc, "", style = "Normal")
}

# ============================================================
# FIGURE LEGENDS
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Figure Legends", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_fpar(doc,
  fpar(ftext("Figure 1. ", fp_text(font.size = 12, bold = TRUE)),
       ftext("Evolutionary, structural, and functional landscape of the Medicago pan-genome.", fp_text(font.size = 12, bold = TRUE))),
  style = "Normal")

fig1_parts <- c(
  "(A) Maximum-likelihood phylogeny of 12 Medicago accessions inferred from 561 single-copy orthologs (105,518 amino-acid sites; LG+I+G; scale bar, 0.005 substitutions per site). Tip labels are colour-coded by species and ploidy, with M. sativa tetraploid accessions (XJDY, ZM1, ZM4_hap4) in orange/red shades, M. sativa ssp. caerulea diploids in blue shades, M. ruthenica in green shades, M. truncatula in purple shades, and the remaining species in pink (M. arabica), teal (M. lupulina) and gold (M. polymorpha).",
  "(B) Copy number variation across pan-gene clusters, measured as the coefficient of variation (CV) of gene copy number per cluster across accessions, shown separately for core, soft_core and dispensable cluster types (violin plots with embedded boxplots). Clusters with fewer than three genes or present in fewer than three accessions were excluded to ensure meaningful CV estimates.",
  "(C) Annotation coverage across the four populated pan-gene cluster types (core, soft_core, dispensable, private), showing the percentage of member genes with at least one annotation in each of three functional layers: Gene Ontology (GO; blue), KEGG Orthology (KO; orange) and euKaryotic Orthologous Groups (KOG; green). Coverage declines monotonically from conserved to variable clusters across all three layers.",
  "(D) Composite Adaptive Priority Index (API) for pan-gene clusters, plotted against the number of accessions in which each cluster is present (x-axis) and the number of cluster-specific UNIQUE CRISPR guides (y-axis, log₁₀). Point colour indicates whether the cluster carries stress- or defence-related GO annotations (red) or not (blue-grey); point size is proportional to the API score."
)
for (p in fig1_parts) { doc <- body_add_par(doc, p, style = "Normal") }

doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Figure 2. ", fp_text(font.size = 12, bold = TRUE)),
       ftext("Phylogenetic relationships among 12 Medicago accessions inferred from 561 single-copy orthologs.", fp_text(font.size = 12, bold = TRUE))),
  style = "Normal")

fig2_parts <- c(
  "(A) Maximum-likelihood phylogeny reconstructed from a concatenated supermatrix of 561 single-copy orthologous genes (105,518 amino-acid sites) using IQ-TREE 2.3.6 under the LG+I+G substitution model. Branch lengths are drawn to scale and represent the expected number of amino-acid substitutions per site (scale bar, 0.005 substitutions per site).",
  "(B) Bootstrap consensus tree derived from 1,000 ultrafast bootstrap replicates. Node labels indicate bootstrap support values (%). All major nodes received bootstrap support of 89–100%, confirming a strongly resolved topology. The outgroup M. polymorpha was placed at the base of the tree (100% support)."
)
for (p in fig2_parts) { doc <- body_add_par(doc, p, style = "Normal") }

doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Figure 3. ", fp_text(font.size = 12, bold = TRUE)),
       ftext("Annotation coverage across pan-gene cluster types.", fp_text(font.size = 12, bold = TRUE))),
  style = "Normal")

doc <- body_add_par(doc,
  "For each of the four populated cluster types (core, soft_core, dispensable, private), bars show the percentage of member genes with at least one annotation in each of three functional layers: Gene Ontology (GO; blue), KEGG Orthology (KO; orange), and euKaryotic Orthologous Groups (KOG; green). Numerical values are printed above each bar. Singleton clusters were excluded because they have no entries in the cluster membership table. Total clustered genes analysed: 548,164. Coverage declines monotonically from conserved to variable cluster types across all three annotation layers, indicating a conservation-dependent annotation bias.",
  style = "Normal")

doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# DISCUSSION
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Discussion", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

discussion <- list(
  "Significance of the Resource" = c(
    "AlfaPan addresses a critical gap in the bioinformatics infrastructure available for Medicago research. While genomic resources for model legumes have expanded rapidly, the integration of these data into accessible, queryable formats has lagged. Existing databases such as MODMS (Fang et al., 2024) and MsGVD have provided valuable genomic data, but none have combined the breadth of multi-omics data with an accessible natural language query interface. AlfaPan fills this gap by providing both a comprehensive data resource and an interface designed for experimental biologists.",
    "The inclusion of 3.56 million CRISPR guide RNAs distinguishes AlfaPan from existing plant genomics databases. Genome editing has emerged as a transformative technology for functional genomics and crop improvement, and the availability of pre-computed guide designs significantly reduces the computational burden for researchers planning editing experiments (Zhang et al., 2021). The guide classification system (UNIQUE versus MULTI) provides immediate information about specificity, enabling users to prioritize high-confidence guides.",
    "The natural language query interface represents a significant accessibility advance. Traditional bioinformatics databases require users to understand SQL syntax, table schemas, and join relationships. The AI Assistant module eliminates this barrier by translating plain English questions into database queries. This approach has been validated in other domains, including clinical trial databases and pharmacology resources (Wang et al., 2023), but its application to plant genomics is relatively novel."
  ),
  "Polyploidy-Driven Gene Family Evolution" = c(
    "Our analyses reveal that autotetraploid alfalfa has preferentially retained and expanded gene families involved in DNA repair, homologous recombination, and MAPK signaling. This pattern is consistent with the hypothesis that polyploid genomes retain duplicates in pathways that enhance genomic stability and environmental sensing. The 83 expanded families in tetraploid M. sativa—compared to only 1 in its diploid ancestor—indicate that polyploidization has been accompanied by substantial gene family expansion beyond the simple duplication expected from tetraploidy.",
    "This finding complements recent work on the Medicago super-pangenome, which demonstrated that core genes retained on all four allelic copies in autotetraploid alfalfa are enriched for climate-adaptation-associated functions (Zhang et al., 2025). Our results extend this observation by showing that gene family expansion specifically targets DNA repair and signaling pathways, which may contribute to the enhanced stress tolerance of tetraploid alfalfa.",
    "The finding that core genes have 2.2-fold higher copy number variation than dispensable genes is counterintuitive but biologically meaningful. In autotetraploids, core genes are often present as multiple homeologs, and subgenome dominance creates substantial copy number variation. This is consistent with the observation that one allelic copy is typically more highly expressed than the others, suggesting homeolog-dependent gene silencing or genetic regulation (Chen et al., 2024)."
  ),
  "Homeolog Retention and Expression Asymmetry" = c(
    "The identification of 895 homeolog-retained candidate clusters provides a foundation for future expression studies. These clusters retain the full complement of homeologs in tetraploid M. sativa, making them priority loci for RNA-seq-based analysis of homeolog expression dominance. The median tetraploid/diploid ratio of 7.7–8.2 suggests that these clusters have undergone substantial expansion, possibly through tandem duplication events in addition to the whole-genome duplication. The enrichment of these clusters in core and soft_core types suggests that they represent conserved functions with multiple homeologs, consistent with the observation that tetra-copy core genes are enriched for stress-adaptation functions."
  ),
  "Functional Implications for Breeding" = c(
    "The observation that private genes are significantly less CRISPR-targetable than core genes (95.4% versus 99.3% UNIQUE guide rates) has practical implications for genome editing. Genes that are most valuable for trait discovery—private, species-specific genes—are also the hardest to edit safely. This trade-off should inform the design of functional validation experiments, where the choice of target genes must balance biological priority against experimental tractability.",
    "The Composite Adaptive Priority Index provides a practical tool for prioritizing candidate genes for functional studies. The 592 top-tier candidates represent a prioritized shortlist that combines biological specificity with experimental tractability. These candidates are immediately actionable for genome editing experiments in alfalfa improvement programs."
  ),
  "Limitations and Future Directions" = c(
    "Several limitations should be acknowledged. First, the 12 accessions included in the pan-genome analysis represent only a fraction of the genetic diversity within the Medicago genus. Integration of additional accessions, particularly wild relatives with valuable stress tolerance alleles, would enhance the resource's utility for breeding applications (Khan et al., 2020). The recent release of a super-pangenome comprising 13 genomes from seven taxa provides an expanded framework for future updates (Zhang et al., 2025).",
    "Second, the functional annotations rely primarily on computational predictions and homology-based transfer. Experimental validation through transcriptomics, proteomics, and metabolomics would substantially increase the value of the resource. Future updates could integrate expression data across tissues and stress conditions, enabling co-expression analysis and regulatory network inference. The lack of RNA-seq data in the current version is a notable limitation for homeolog expression analysis, though the identification of candidate clusters provides a foundation for future work.",
    "Third, the natural language interface, while functional, has limitations in handling complex queries involving multiple joins and nested subqueries. Ongoing advances in large language model capabilities and the development of retrieval-augmented generation approaches may improve performance on such queries (Lewis et al., 2020).",
    "Fourth, the current implementation runs on a local PostgreSQL instance, limiting concurrent access and requiring users to set up their own database environment. Cloud deployment through platforms such as Posit Connect Cloud would enable broader access without installation requirements."
  )
)
for (h in names(discussion)) {
  doc <- body_add_fpar(doc, fpar(ftext(h, fp_text(font.size = 12, bold = TRUE))), style = "heading 2")
  for (p in discussion[[h]]) { doc <- body_add_par(doc, p, style = "Normal") }
  doc <- body_add_par(doc, "", style = "Normal")
}

# ============================================================
# CONCLUSION
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Conclusion", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc,
  "AlfaPan represents a comprehensive, accessible resource for Medicago genomics research. By integrating genomic, functional annotation, and CRISPR guide RNA data across 12 accessions, the database provides a foundation for comparative analysis, functional genomics, and genome editing applications. Our analyses reveal genome-wide signatures of polyploidy-driven gene family expansion, biased subgenome retention, and adaptive functional variation. The natural language query interface lowers barriers to access, enabling researchers without bioinformatics expertise to explore the data. The resource is freely available under open licenses, and future updates will expand the number of accessions, integrate additional omics layers, and enhance the query interface. We anticipate that this resource will accelerate alfalfa improvement by providing the research community with the data and tools needed for functional characterization of genes controlling agronomically important traits.",
  style = "Normal")

doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# BACK MATTER
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("Data Availability", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc,
  "AlfaPan is available at https://github.com/amomboerick/alfalfa-multi-omics-pangenome. The PostgreSQL database dump is available upon reasonable request. Source code is licensed under MIT; data are licensed under CC-BY 4.0. All analysis scripts and figure-generating code are provided in the analysis/ directory of the repository.",
  style = "Normal")

doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Funding", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc, "[FUNDING SOURCE — to be added]", style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Author Contributions", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc,
  "E.A. conceived the project, developed the database and web application, performed all analyses, and wrote the manuscript.",
  style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Conflict of Interest", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc, "The authors declare no competing interests.", style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(doc,
  fpar(ftext("Acknowledgements", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

doc <- body_add_par(doc,
  "We thank the developers of PostgreSQL, R, Shiny, IQ-TREE, MAFFT, and the open-source packages that made this resource possible. We acknowledge the researchers who generated and publicly released the Medicago genome assemblies and annotations used in this study.",
  style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# REFERENCES (TPJ style: author-year, abbreviated journals, bold volume)
# ============================================================
doc <- body_add_fpar(doc,
  fpar(ftext("References", fp_text(font.size = 14, bold = TRUE))),
  style = "heading 1")

refs <- c(
  "Bayer, P.E., Golicz, A.A., Scheben, A., Batley, J. and Edwards, D. (2020) Plant pan-genomes are the new reference. Nat. Plants 6: 914–920.",
  "Biazzi, E., Nazzicari, N., Pecetti, L., et al. (2017) Genome-wide association mapping of agronomic traits in alfalfa. Plant Genome 10: 1–14.",
  "Chen, H., Zeng, Y., Yang, Y., et al. (2020) Haplotype-resolved genome assembly of autotetraploid alfalfa. Nat. Genet. 52: 1065–1075.",
  "Chen, H., Zhang, F., Wei, C., et al. (2024) Super-pangenome analysis reveals structural variation and allelic expression in Medicago. Nat. Commun. 15: 1–18.",
  "Cui, J., Lu, Z., Wang, T., et al. (2021) The genome of Medicago ruthenica provides insights into adaptation to harsh environments. Mol. Ecol. Resour. 21: 1655–1668.",
  "Emms, D.M. and Kelly, S. (2019) OrthoFinder: phylogenetic orthology inference for comparative genomics. Genome Biol. 20: 238.",
  "Fang, L., Liu, Y., Wang, X., et al. (2024) MODMS: a multi-omics database for facilitating biological studies on alfalfa (Medicago sativa L.). Hortic. Res. 11: uhad245.",
  "Gao, L., Gonda, I., Sun, H., et al. (2019) The tomato pan-genome uncovers new genes and a rare allele regulating fruit flavor. Nat. Genet. 51: 1044–1051.",
  "Katoh, K. and Standley, D.M. (2013) MAFFT multiple sequence alignment software version 7: improvements in performance and usability. Mol. Biol. Evol. 30: 772–780.",
  "Khan, A.W., Garg, V., Roorkiwal, M., et al. (2020) Super-pangenome by integrating the wild side of a species for accelerated crop improvement. Trends Plant Sci. 25: 148–158.",
  "Lewis, P., Perez, E., Piktus, A., et al. (2020) Retrieval-augmented generation for knowledge-intensive NLP tasks. Adv. Neural Inf. Process. Syst. 33: 9459–9474.",
  "Li, Y.H., Zhou, G., Ma, J., et al. (2014) De novo assembly of soybean wild relatives for pan-genome analysis of diversity and agronomic traits. Nat. Biotechnol. 32: 1045–1052.",
  "Liu, Y., Du, H., Li, P., et al. (2020) Pan-genome of wild and cultivated soybeans. Cell 182: 162–176.",
  "Long, Y., Zhang, F., Wei, C., et al. (2022) Haplotype-resolved genome assembly of autotetraploid alfalfa provides insights into stress adaptation. Plant J. 110: 490–505.",
  "Minh, B.Q., Schmidt, H.A., Chernomor, O., et al. (2020) IQ-TREE 2: New models and efficient methods for phylogenetic inference in the genomic era. Mol. Biol. Evol. 37: 1530–1534.",
  "Shang, H., Li, Y., Zhang, X., et al. (2022) The genome of Medicago polymorpha provides insights into adaptive evolution. Plant Physiol. 189: 765–781.",
  "Shen, C., Wang, X., Zhang, Y., et al. (2020) Global alfalfa production and trade: challenges and opportunities. J. Integr. Agric. 19: 2669–2681.",
  "Small, E. (2011) Alfalfa and Relatives: Evolution and Classification of Medicago. Ottawa: NRC Research Press.",
  "Tang, H., Krishnakumar, V., Bidwell, S., et al. (2014) An improved genome release (version Mt4.0) for the model legume Medicago truncatula. BMC Genomics 15: 312.",
  "Tettelin, H., Masignani, V., Cieslewicz, M.J., et al. (2005) Genome analysis of multiple pathogenic isolates of Streptococcus agalactiae: implications for the microbial \"pan-genome\". Proc. Natl Acad. Sci. USA 102: 13950–13955.",
  "Wang, Y., Li, X., Chen, Z., et al. (2023) Natural language interfaces for biomedical databases. Brief. Bioinform. 24: bbad045.",
  "Young, N.D., Debellé, F., Oldroyd, G.E., et al. (2011) The Medicago genome provides insight into the evolution of rhizobial symbioses. Nature 480: 520–524.",
  "Zhang, F., Wei, C., Shi, X., et al. (2025) Medicago super-pangenome reveals adaptive advantages and evolutionary constraints in autotetraploid alfalfa. Nat. Commun. 17: 1–18.",
  "Zhang, Y., Massel, K., Godwin, I.D. and Qi, Y. (2021) CRISPR guide RNA design for plant genome editing. Plant Biotechnol. J. 19: 678–691."
)
for (r in refs) { doc <- body_add_par(doc, r, style = "Normal") }

# ============================================================
# Save
# ============================================================
print(doc, target = "AlfaPan_v2.docx")

cat("\n========================================\n")
cat("Manuscript rebuilt: AlfaPan_v2.docx\n")
cat("Location: C:/Users/Erick.Amombo/alfalfa_pangenome/manuscript/AlfaPan_v2.docx\n")
cat("========================================\n")