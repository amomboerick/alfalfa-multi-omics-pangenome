# scripts/load_new_clusters.py
# Load diamond_clusters_new.tsv into PostgreSQL (pan_gene_clusters + cluster_membership)

import os, sys, re
import psycopg2

DB = {'host':'localhost','port':5433,'database':'alfalfa_pangenome',
      'user':'postgres','password':'postgres'}

TSV = r"C:\Users\Erick.Amombo\alfalfa_pangenome\diamond_clusters_pg.tsv"

conn = psycopg2.connect(**DB)
cur = conn.cursor()

print("Loading gene lookup...")
cur.execute("SELECT gene_id, gene_name, accession_id FROM genes")
gene_by_name = {}
for gid, gname, aid in cur.fetchall():
    gene_by_name[gname] = (gid, aid)
print("  Genes in DB: {:,}".format(len(gene_by_name)))

def variants(name):
    yield name
    m = re.match(r'^(.+?)\.t\d+$', name)
    if m: yield m.group(1)
    m = re.match(r'^(.+?)-mRNA-\d+$', name)
    if m: yield m.group(1)
    yield name.replace('.model.', '.TU.')
    yield name.replace('.TU.', '.model.')
    m = re.match(r'^[a-z]+\d*\.(.+)$', name)
    if m: yield m.group(1)
    yield name.upper()
    yield name.lower()

print("\nClearing old cluster data...")
cur.execute("TRUNCATE gene_presence_absence, cluster_membership, pan_gene_clusters RESTART IDENTITY CASCADE")
conn.commit()

print("\nReading TSV...")
clusters = {}
with open(TSV, encoding="utf-8", errors="ignore") as fh:
    for line in fh:
        line = line.rstrip("\r\n")
        if not line: continue
        p = line.split("\t")
        if len(p) < 2: continue
        pid, rep = p[0].strip(), p[1].strip()
        clusters.setdefault(rep, []).append(pid)
print("  Unique clusters: {:,}".format(len(clusters)))

print("\nMatching IDs...")
matched = {}
unmatched = {}
for rep, members in clusters.items():
    for pid in members:
        if pid in matched: continue
        found = None
        for cand in variants(pid):
            if cand in gene_by_name:
                found = gene_by_name[cand]; break
        if found: matched[pid] = found
        else: unmatched[pid] = 1
print("  Matched:   {:,}".format(len(matched)))
print("  Unmatched: {:,}".format(len(unmatched)))

cur.execute("SELECT COUNT(*) FROM accessions")
total_acc = cur.fetchone()[0]

print("\nInserting clusters...")
cluster_id_map = {}
inserted = 0
for rep, members in clusters.items():
    accs = set()
    for pid in members:
        if pid in matched:
            accs.add(matched[pid][1])
    presence = len(accs)
    if presence == 0: ctype = "singleton"
    elif presence == total_acc: ctype = "core"
    elif presence >= int(total_acc*0.95): ctype = "soft_core"
    elif presence > 1: ctype = "dispensable"
    else: ctype = "private"
    cur.execute("""
        INSERT INTO pan_gene_clusters (cluster_name, cluster_type, gene_count, presence_across_accessions)
        VALUES (%s, %s, %s, %s) RETURNING cluster_id
    """, (rep[:250], ctype, len(members), presence))
    cluster_id_map[rep] = cur.fetchone()[0]
    inserted += 1
    if inserted % 20000 == 0:
        conn.commit(); print("  {}".format(inserted))
conn.commit()
print("  Total clusters: {:,}".format(inserted))

print("\nInserting cluster_membership...")
rows = []
for rep, members in clusters.items():
    cid = cluster_id_map[rep]
    for pid in members:
        if pid in matched:
            gid, aid = matched[pid]
            rows.append((cid, gid, aid))
BATCH = 10000
for i in range(0, len(rows), BATCH):
    cur.executemany("""
        INSERT INTO cluster_membership (cluster_id, gene_id, accession_id)
        VALUES (%s, %s, %s) ON CONFLICT DO NOTHING
    """, rows[i:i+BATCH])
    conn.commit()
    print("  {:,}/{:,}".format(min(i+BATCH, len(rows)), len(rows)))

print("\nPopulating gene_presence_absence...")
cur.execute("""
    INSERT INTO gene_presence_absence (cluster_id, accession_id, is_present)
    SELECT DISTINCT cluster_id, accession_id, TRUE FROM cluster_membership
    ON CONFLICT DO NOTHING
""")
conn.commit()

print("\nUpdating pan_genome_stats...")
cur.execute("""
    INSERT INTO pan_genome_stats (accession_id, core_genes_count, soft_core_genes_count,
                                   dispensable_genes_count, private_genes_count, total_clusters_count)
    SELECT cm.accession_id,
        COUNT(DISTINCT CASE WHEN pc.cluster_type='core' THEN cm.cluster_id END),
        COUNT(DISTINCT CASE WHEN pc.cluster_type='soft_core' THEN cm.cluster_id END),
        COUNT(DISTINCT CASE WHEN pc.cluster_type='dispensable' THEN cm.cluster_id END),
        COUNT(DISTINCT CASE WHEN pc.cluster_type='private' THEN cm.cluster_id END),
        COUNT(DISTINCT cm.cluster_id)
    FROM cluster_membership cm
    JOIN pan_gene_clusters pc ON cm.cluster_id = pc.cluster_id
    GROUP BY cm.accession_id
    ON CONFLICT (accession_id) DO UPDATE SET
        core_genes_count=EXCLUDED.core_genes_count,
        soft_core_genes_count=EXCLUDED.soft_core_genes_count,
        dispensable_genes_count=EXCLUDED.dispensable_genes_count,
        private_genes_count=EXCLUDED.private_genes_count,
        total_clusters_count=EXCLUDED.total_clusters_count
""")
conn.commit()

print("\nFinal cluster breakdown:")
cur.execute("SELECT cluster_type, COUNT(*) FROM pan_gene_clusters GROUP BY cluster_type ORDER BY 2 DESC")
for ct, n in cur.fetchall():
    print("  {:<12} {:>10,}".format(ct, n))

cur.close()
conn.close()
print("\nDone.")