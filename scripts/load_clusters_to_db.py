# scripts/load_clusters_to_db.py
# Load DIAMOND cluster assignments into PostgreSQL

import os, sys
import psycopg2

DB_CONFIG = {
    'host': 'localhost',
    'port': 5433,
    'database': 'alfalfa_pangenome',
    'user': 'postgres',
    'password': 'postgres'
}

CLUSTERS_FILE = r"C:\Users\Erick.Amombo\alfalfa_pangenome\diamond_clusters.tsv"

try:
    conn = psycopg2.connect(**DB_CONFIG)
    conn.autocommit = False
    cur = conn.cursor()
    print("Connected to PostgreSQL")
except Exception as e:
    print("Cannot connect:", e); sys.exit(1)

print("Clearing old cluster data...")
cur.execute("TRUNCATE gene_presence_absence, cluster_membership, pan_gene_clusters RESTART IDENTITY CASCADE")
conn.commit()
print("Cleared.\n")

print("Loading gene lookup table...")
cur.execute("SELECT gene_id, gene_name, accession_id FROM genes")
gene_lookup = {}
for gid, gname, aid in cur.fetchall():
    gene_lookup[gname] = (gid, aid)
print("  Genes in DB: {:,}".format(len(gene_lookup)))

print("\nReading clusters file...")
cluster_members = {}
total_lines = 0
with open(CLUSTERS_FILE, encoding="utf-8", errors="ignore") as fh:
    for line in fh:
        line = line.rstrip("\r\n")
        if not line:
            continue
        parts = line.split("\t")
        if len(parts) < 2:
            continue
        gene_name = parts[0].strip()
        rep_id    = parts[1].strip()
        cluster_members.setdefault(rep_id, []).append(gene_name)
        total_lines += 1
print("  Cluster assignments: {:,}".format(total_lines))
print("  Unique clusters:     {:,}".format(len(cluster_members)))

cur.execute("SELECT COUNT(*) FROM accessions")
total_accessions = cur.fetchone()[0]
print("  Total accessions:    {}".format(total_accessions))

print("\nInserting clusters into pan_gene_clusters...")
cluster_id_map = {}
inserted = 0
for rep_id, members in cluster_members.items():
    accs = set()
    for gname in members:
        if gname in gene_lookup:
            accs.add(gene_lookup[gname][1])
    presence = len(accs)

    if presence == total_accessions:
        ctype = "core"
    elif presence >= int(total_accessions * 0.95):
        ctype = "soft_core"
    elif presence > 1:
        ctype = "dispensable"
    elif presence == 1:
        ctype = "private"
    else:
        ctype = "singleton"

    cur.execute("""
        INSERT INTO pan_gene_clusters (cluster_name, cluster_type, gene_count, presence_across_accessions)
        VALUES (%s, %s, %s, %s)
        RETURNING cluster_id
    """, (rep_id, ctype, len(members), presence))
    cluster_id_map[rep_id] = cur.fetchone()[0]
    inserted += 1
    if inserted % 10000 == 0:
        conn.commit()
        print("  Inserted {} clusters...".format(inserted))

conn.commit()
print("  Total clusters inserted: {:,}".format(inserted))

print("\nInserting cluster_membership...")
membership_rows = []
for rep_id, members in cluster_members.items():
    cid = cluster_id_map[rep_id]
    for gname in members:
        if gname in gene_lookup:
            gid, aid = gene_lookup[gname]
            membership_rows.append((cid, gid, aid))

batch_size = 10000
total_mem = len(membership_rows)
for i in range(0, total_mem, batch_size):
    batch = membership_rows[i:i+batch_size]
    cur.executemany("""
        INSERT INTO cluster_membership (cluster_id, gene_id, accession_id)
        VALUES (%s, %s, %s)
        ON CONFLICT DO NOTHING
    """, batch)
    conn.commit()
    print("  {:,} / {:,}".format(min(i+batch_size, total_mem), total_mem))

print("\nPopulating gene_presence_absence...")
cur.execute("""
    INSERT INTO gene_presence_absence (cluster_id, accession_id, is_present)
    SELECT DISTINCT cluster_id, accession_id, TRUE FROM cluster_membership
    ON CONFLICT DO NOTHING
""")
conn.commit()
print("  Done.")

print("\nUpdating pan_genome_stats...")
cur.execute("""
    INSERT INTO pan_genome_stats (accession_id, core_genes_count, soft_core_genes_count,
                                   dispensable_genes_count, private_genes_count, total_clusters_count)
    SELECT 
        cm.accession_id,
        COUNT(DISTINCT CASE WHEN pc.cluster_type = 'core' THEN cm.cluster_id END),
        COUNT(DISTINCT CASE WHEN pc.cluster_type = 'soft_core' THEN cm.cluster_id END),
        COUNT(DISTINCT CASE WHEN pc.cluster_type = 'dispensable' THEN cm.cluster_id END),
        COUNT(DISTINCT CASE WHEN pc.cluster_type = 'private' THEN cm.cluster_id END),
        COUNT(DISTINCT cm.cluster_id)
    FROM cluster_membership cm
    JOIN pan_gene_clusters pc ON cm.cluster_id = pc.cluster_id
    GROUP BY cm.accession_id
    ON CONFLICT (accession_id) DO UPDATE SET
        core_genes_count = EXCLUDED.core_genes_count,
        soft_core_genes_count = EXCLUDED.soft_core_genes_count,
        dispensable_genes_count = EXCLUDED.dispensable_genes_count,
        private_genes_count = EXCLUDED.private_genes_count,
        total_clusters_count = EXCLUDED.total_clusters_count
""")
conn.commit()
print("  Done.")

print("\nFinal counts:")
cur.execute("SELECT cluster_type, COUNT(*) FROM pan_gene_clusters GROUP BY cluster_type ORDER BY 2 DESC")
for ct, n in cur.fetchall():
    print("  {:<12} {:>8,}".format(ct, n))

cur.close()
conn.close()
print("\nAll done.")