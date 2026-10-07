-- ============================================================
-- Multi-omics annotation tables
-- Links to existing genes(gene_id) and accessions(accession_id)
-- ============================================================

-- GO (Gene Ontology) annotations
DROP TABLE IF EXISTS go_annotations CASCADE;
CREATE TABLE go_annotations (
    go_ann_id    SERIAL PRIMARY KEY,
    gene_id      INTEGER REFERENCES genes(gene_id) ON DELETE CASCADE,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    go_term      VARCHAR(20) NOT NULL
);
CREATE INDEX idx_go_gene      ON go_annotations(gene_id);
CREATE INDEX idx_go_accession ON go_annotations(accession_id);
CREATE INDEX idx_go_term      ON go_annotations(go_term);

-- KEGG KO numbers
DROP TABLE IF EXISTS kegg_ko CASCADE;
CREATE TABLE kegg_ko (
    ko_id        SERIAL PRIMARY KEY,
    gene_id      INTEGER REFERENCES genes(gene_id) ON DELETE CASCADE,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    ko_number    VARCHAR(20) NOT NULL
);
CREATE INDEX idx_ko_gene      ON kegg_ko(gene_id);
CREATE INDEX idx_ko_accession ON kegg_ko(accession_id);
CREATE INDEX idx_ko_number    ON kegg_ko(ko_number);

-- KEGG Pathways
DROP TABLE IF EXISTS kegg_pathways CASCADE;
CREATE TABLE kegg_pathways (
    pathway_id   SERIAL PRIMARY KEY,
    gene_id      INTEGER REFERENCES genes(gene_id) ON DELETE CASCADE,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    pathway_code VARCHAR(20) NOT NULL
);
CREATE INDEX idx_path_gene      ON kegg_pathways(gene_id);
CREATE INDEX idx_path_accession ON kegg_pathways(accession_id);
CREATE INDEX idx_path_code      ON kegg_pathways(pathway_code);

-- KOG / COG categories
DROP TABLE IF EXISTS kog_annotations CASCADE;
CREATE TABLE kog_annotations (
    kog_id       SERIAL PRIMARY KEY,
    gene_id      INTEGER REFERENCES genes(gene_id) ON DELETE CASCADE,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    cog_letter   CHAR(1) NOT NULL
);
CREATE INDEX idx_kog_gene      ON kog_annotations(gene_id);
CREATE INDEX idx_kog_accession ON kog_annotations(accession_id);
CREATE INDEX idx_kog_letter    ON kog_annotations(cog_letter);