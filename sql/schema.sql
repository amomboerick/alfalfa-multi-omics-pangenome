-- ============================================
-- MEDICAGO SUPER-PAN-GENOME SCHEMA
-- FASTA + GFF only, multi-species
-- ============================================

CREATE TABLE IF NOT EXISTS accessions (
    accession_id SERIAL PRIMARY KEY,
    accession_name VARCHAR(200) UNIQUE NOT NULL,
    species VARCHAR(150),
    subspecies VARCHAR(150),
    cultivar VARCHAR(100),
    ploidy VARCHAR(20),
    genome_size BIGINT,
    gc_content DECIMAL(5,2),
    n50 INTEGER,
    contig_count INTEGER,
    gene_count INTEGER DEFAULT 0,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS contigs (
    contig_id SERIAL PRIMARY KEY,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    contig_name VARCHAR(200) NOT NULL,
    contig_length BIGINT,
    gc_content DECIMAL(5,2),
    sequence TEXT,
    is_chromosome BOOLEAN DEFAULT FALSE,
    UNIQUE(accession_id, contig_name)
);

CREATE TABLE IF NOT EXISTS genes (
    gene_id SERIAL PRIMARY KEY,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    contig_id INTEGER REFERENCES contigs(contig_id) ON DELETE CASCADE,
    gene_name VARCHAR(200),
    gene_symbol VARCHAR(100),
    start_pos BIGINT,
    end_pos BIGINT,
    strand CHAR(1),
    gene_type VARCHAR(50),
    biotype VARCHAR(50),
    score DECIMAL(10,2),
    phase INTEGER,
    attributes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS pan_gene_clusters (
    cluster_id SERIAL PRIMARY KEY,
    cluster_name VARCHAR(50) UNIQUE NOT NULL,
    cluster_type VARCHAR(20),
    gene_count INTEGER DEFAULT 0,
    presence_across_accessions INTEGER DEFAULT 0,
    functional_annotation TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS cluster_membership (
    membership_id SERIAL PRIMARY KEY,
    cluster_id INTEGER REFERENCES pan_gene_clusters(cluster_id) ON DELETE CASCADE,
    gene_id INTEGER REFERENCES genes(gene_id) ON DELETE CASCADE,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    UNIQUE(cluster_id, gene_id, accession_id)
);

CREATE TABLE IF NOT EXISTS gene_presence_absence (
    pa_id SERIAL PRIMARY KEY,
    cluster_id INTEGER REFERENCES pan_gene_clusters(cluster_id) ON DELETE CASCADE,
    accession_id INTEGER REFERENCES accessions(accession_id) ON DELETE CASCADE,
    is_present BOOLEAN DEFAULT TRUE,
    copy_number INTEGER DEFAULT 1,
    UNIQUE(cluster_id, accession_id)
);

CREATE INDEX IF NOT EXISTS idx_contigs_accession ON contigs(accession_id);
CREATE INDEX IF NOT EXISTS idx_genes_accession ON genes(accession_id);
CREATE INDEX IF NOT EXISTS idx_genes_contig ON genes(contig_id);
CREATE INDEX IF NOT EXISTS idx_genes_name ON genes(gene_name);
CREATE INDEX IF NOT EXISTS idx_genes_pos ON genes(start_pos, end_pos);
CREATE INDEX IF NOT EXISTS idx_cm_cluster ON cluster_membership(cluster_id);
CREATE INDEX IF NOT EXISTS idx_cm_gene ON cluster_membership(gene_id);
CREATE INDEX IF NOT EXISTS idx_gpa_cluster ON gene_presence_absence(cluster_id);
CREATE INDEX IF NOT EXISTS idx_gpa_accession ON gene_presence_absence(accession_id);