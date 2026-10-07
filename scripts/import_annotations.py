# scripts/import_annotations.py
# Import GO, KEGG KO, KEGG Pathway, KOG annotations into PostgreSQL

import os, glob, re
import psycopg2

DB = {
    'host': 'localhost', 'port': 5433,
    'database': 'alfalfa_pangenome',
    'user': 'postgres', 'password': 'postgres'
}

BASE = r"C:\Users\Erick.Amombo\alfalfa_pangenome\Medicago_pangenome_annotation\Medicago_pangenome_annotation"

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

try:
    conn = psycopg2.connect(**DB)
    conn.autocommit = False
    cur = conn.cursor()
    print("Connected to PostgreSQL\n")
except Exception as e:
    print("Cannot connect:", e)
    raise SystemExit(1)

print("Loading gene lookup table...")
cur.execute("SELECT gene_name, gene_id, accession_id FROM genes")
gene_lookup = {}
for gname, gid, aid in cur.fetchall():
    gene_lookup[gname] = (gid, aid)
print("  {:,} genes in DB\n".format(len(gene_lookup)))

cur.execute("SELECT accession_name FROM accessions ORDER BY accession_name")
db_acc = set(r[0] for r in cur.fetchall())
print("Accessions found in annotation dir:")
for f, a in ACCESSION_MAP.items():
    mark = "OK " if a in db_acc else "MISSING"
    print("  [{}] {} -> {}".format(mark, f, a))
print()

def import_table(annotation_dir, file_glob, table, value_col):
    print("=" * 60)
    print("Importing:", table)
    print("=" * 60)

    files = sorted(glob.glob(os.path.join(annotation_dir, file_glob)))
    print("  Found {} files".format(len(files)))

    total_inserted = 0
    total_unmatched = 0

    for fp in files:
        fname = os.path.basename(fp)
        prefix = fname.split('.')[0]

        acc_name = ACCESSION_MAP.get(prefix)
        if acc_name is None:
            print("  SKIP {} (unknown prefix)".format(fname))
            continue

        cur.execute("SELECT accession_id FROM accessions WHERE accession_name = %s", (acc_name,))
        row = cur.fetchone()
        if not row:
            print("  SKIP {} (accession not in DB)".format(fname))
            continue
        acc_id = row[0]

        batch = []
        n_lines = 0
        n_matched = 0
        n_unmatched = 0

        with open(fp, 'r', encoding='utf-8', errors='ignore') as fh:
            for line in fh:
                line = line.rstrip('\r\n')
                if not line or line.startswith('#'):
                    continue
                parts = line.split('\t')
                if len(parts) < 2:
                    continue
                raw_gid = parts[0].strip()
                val     = parts[1].strip()
                if not val:
                    continue
                n_lines += 1
                key = norm(raw_gid)
                gl = gene_lookup.get(key)
                if gl is None:
                    n_unmatched += 1
                    continue
                n_matched += 1
                batch.append((gl[0], acc_id, val))
                if len(batch) >= 5000:
                    cur.executemany(
                        "INSERT INTO {} (gene_id, accession_id, {}) VALUES (%s, %s, %s)".format(table, value_col),
                        batch)
                    total_inserted += len(batch)
                    batch = []

        if batch:
            cur.executemany(
                "INSERT INTO {} (gene_id, accession_id, {}) VALUES (%s, %s, %s)".format(table, value_col),
                batch)
            total_inserted += len(batch)

        total_unmatched += n_unmatched
        print("  {} -> {:,} lines, {:,} matched, {:,} unmatched".format(
            fname, n_lines, n_matched, n_unmatched))

    conn.commit()
    print("  TOTAL inserted:  {:,}".format(total_inserted))
    print("  TOTAL unmatched: {:,}\n".format(total_unmatched))
    return total_inserted

# 1. GO
import_table(os.path.join(BASE, "GO"), "*.go.tsv", "go_annotations", "go_term")

# 2. KEGG KO
import_table(os.path.join(BASE, "KEGG"), "*.ko.tsv", "kegg_ko", "ko_number")

# 3. KEGG pathways
import_table(os.path.join(BASE, "KEGG"), "*.pathway.tsv", "kegg_pathways", "pathway_code")

# 4. KOG
import_table(os.path.join(BASE, "KOG"), "*.cog.tsv", "kog_annotations", "cog_letter")

print("=" * 60)
print("FINAL COUNTS")
print("=" * 60)
cur.execute("SELECT COUNT(*) FROM go_annotations")
print("  GO annotations:  {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(*) FROM kegg_ko")
print("  KEGG KO:         {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(*) FROM kegg_pathways")
print("  KEGG pathways:   {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(*) FROM kog_annotations")
print("  KOG categories:  {:,}".format(cur.fetchone()[0]))

print("\nDistinct values:")
cur.execute("SELECT COUNT(DISTINCT go_term) FROM go_annotations")
print("  GO terms:        {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(DISTINCT ko_number) FROM kegg_ko")
print("  KEGG KOs:        {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(DISTINCT pathway_code) FROM kegg_pathways")
print("  KEGG pathways:   {:,}".format(cur.fetchone()[0]))
cur.execute("SELECT COUNT(DISTINCT cog_letter) FROM kog_annotations")
print("  KOG letters:     {:,}".format(cur.fetchone()[0]))

cur.close()
conn.close()
print("\nDone.")