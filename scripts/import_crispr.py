# scripts/import_crispr.py
# Import CRISPR guide RNA TSV into PostgreSQL

import os, re, csv, time
import psycopg2

DB = {
    'host': 'localhost', 'port': 5433,
    'database': 'alfalfa_pangenome',
    'user': 'postgres', 'password': 'postgres'
}

TSV = r"C:\Users\Erick.Amombo\alfalfa_pangenome\Medicago_pangenome_annotation\Medicago_CRISPR_gRNA_compact.tsv"

ACCESSION_MAP = {
    "M_arabica":                    "M_arabica",
    "M_lupulina":                   "M_lupulina",
    "M_polymorpha":                 "M_polymorpha",
    "M_ruthenica_landa":            "M_ruthenica_landa",
    "M_ruthenica_zhiwusuo":         "M_ruthenica_zhiwusuo",
    "M_sativa_ssp_caerulea_landa":  "M_sativa_ssp_caerulea_landa",
    "M_sativa_ssp_caerulea_long":   "M_sativa_ssp_caerulea_long",
    "M_sativa_ZM1":                 "M_sativa_ZM1",
    "M_truncatula_A17":             "M_truncatula_A17",
    "M_truncatula_HM078":           "M_truncatula_HM078",
    "M_truncatula_R108":            "M_truncatula_R108",
    "XJDY":                         "M_sativa_XJDY",
    "ZM4":                          "M_sativa_ZM4_hap4",
}

def norm(gid):
    gid = gid.strip()
    gid = re.sub(r'\.t\d+$', '', gid)
    gid = re.sub(r'-mRNA-\d+$', '', gid)
    return gid

# ---------- Connect ----------
conn = psycopg2.connect(**DB)
conn.autocommit = False
cur = conn.cursor()
print("Connected to PostgreSQL\n")

# ---------- Load gene lookup ----------
print("Loading gene lookup...")
cur.execute("SELECT gene_name, gene_id, accession_id FROM genes")
gene_lookup = {}
for gname, gid, aid in cur.fetchall():
    gene_lookup[gname] = (gid, aid)
print("  {:,} genes in DB\n".format(len(gene_lookup)))

# ---------- Accession lookup ----------
cur.execute("SELECT accession_name, accession_id FROM accessions")
acc_lookup = dict(cur.fetchall())
print("Loaded {} accessions\n".format(len(acc_lookup)))

# ---------- Read + insert ----------
print("=" * 60)
print("Importing CRISPR guides")
print("=" * 60)

total = 0
matched = 0
unmatched = 0
batch = []
t0 = time.time()

with open(TSV, 'r', encoding='utf-8', errors='ignore') as fh:
    reader = csv.DictReader(fh, delimiter='\t')

    for row in reader:
        total += 1

        raw_gid = row.get('gene_id', '').strip()
        if not raw_gid:
            continue

        key = norm(raw_gid)
        gl = gene_lookup.get(key)
        if gl is None:
            unmatched += 1
            continue

        gid, aid_from_gene = gl

        # Use genome column to determine accession, fall back to gene's accession
        genome = row.get('genome', '').strip()
        acc_name = ACCESSION_MAP.get(genome)
        aid = acc_lookup.get(acc_name) if acc_name else aid_from_gene
        if aid is None:
            aid = aid_from_gene

        def parse_int(x):
            try: return int(x) if x not in ('', '.', 'NA') else None
            except: return None

        def parse_dec(x):
            try: return float(x) if x not in ('', '.', 'NA') else None
            except: return None

        batch.append((
            gid,
            aid,
            genome,
            raw_gid,
            parse_int(row.get('rank_in_gene')),
            row.get('chrom'),
            row.get('strand'),
            row.get('gene_strand'),
            row.get('spacer_seq', '').strip().upper(),
            row.get('pam_seq', '').strip().upper(),
            parse_int(row.get('spacer_start')),
            parse_int(row.get('spacer_end')),
            parse_int(row.get('pam_start')),
            parse_int(row.get('pam_end')),
            parse_int(row.get('cut_site')),
            parse_dec(row.get('gc_pct')),
            parse_int(row.get('n_genome_hits')),
            row.get('uniqueness'),
            row.get('specificity_class'),
        ))
        matched += 1

        if len(batch) >= 5000:
            cur.executemany("""
                INSERT INTO crispr_guides
                (gene_id, accession_id, genome, gene_name, rank_in_gene,
                 chrom, strand, gene_strand, spacer_seq, pam_seq,
                 spacer_start, spacer_end, pam_start, pam_end, cut_site,
                 gc_pct, n_genome_hits, uniqueness, specificity_class)
                VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
            """, batch)
            conn.commit()
            batch = []
            elapsed = time.time() - t0
            print("  {:,} rows processed, {:,} inserted, {:,} unmatched ({:.0f}s)".format(
                total, matched, unmatched, elapsed))

if batch:
    cur.executemany("""
        INSERT INTO crispr_guides
        (gene_id, accession_id, genome, gene_name, rank_in_gene,
         chrom, strand, gene_strand, spacer_seq, pam_seq,
         spacer_start, spacer_end, pam_start, pam_end, cut_site,
         gc_pct, n_genome_hits, uniqueness, specificity_class)
        VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
    """, batch)
    conn.commit()

# ---------- Summary ----------
print("\n" + "=" * 60)
print("FINAL COUNTS")
print("=" * 60)
cur.execute("SELECT COUNT(*) FROM crispr_guides")
print("  Total guides:      {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(DISTINCT gene_id) FROM crispr_guides")
print("  Genes with guides: {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(DISTINCT accession_id) FROM crispr_guides")
print("  Accessions:        {}".format(cur.fetchone()[0]))
cur.execute("SELECT uniqueness, COUNT(*) FROM crispr_guides GROUP BY uniqueness ORDER BY 2 DESC")
print("\n  Uniqueness breakdown:")
for u, n in cur.fetchall():
    print("    {:<15} {:>12,}".format(str(u), n))

cur.close()
conn.close()
print("\nDone.")