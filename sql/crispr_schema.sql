-- ============================================================
-- CRISPR guide RNA table
-- ============================================================

DROP TABLE IF EXISTS crispr_guides CASCADE;

CREATE TABLE crispr_guides (
    guide_id            SERIAL PRIMARY KEY,
    gene_id             INTEGER REFERENCES genes(gene_id) ON DELETE CASCADE,
    accession_id        INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    genome              VARCHAR(50),
    gene_name           VARCHAR(50),
    rank_in_gene        INTEGER,
    chrom               VARCHAR(50),
    strand              CHAR(1),
    gene_strand         CHAR(1),
    spacer_seq          VARCHAR(30) NOT NULL,
    pam_seq             VARCHAR(10),
    spacer_start        BIGINT,
    spacer_end          BIGINT,
    pam_start           BIGINT,
    pam_end             BIGINT,
    cut_site            BIGINT,
    gc_pct              DECIMAL(5,2),
    n_genome_hits       INTEGER,
    uniqueness          VARCHAR(20),
    specificity_class   VARCHAR(10)
);

CREATE INDEX idx_crispr_gene       ON crispr_guides(gene_id);
CREATE INDEX idx_crispr_accession  ON crispr_guides(accession_id);
CREATE INDEX idx_crispr_spacer     ON crispr_guides(spacer_seq);
CREATE INDEX idx_crispr_gene_name  ON crispr_guides(gene_name);
CREATE INDEX idx_crispr_uniqueness ON crispr_guides(uniqueness);