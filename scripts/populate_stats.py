# scripts/populate_stats.py
# Populate pan_genome_stats after clusters are loaded

import psycopg2

DB = {'host':'localhost','port':5433,'database':'alfalfa_pangenome',
      'user':'postgres','password':'postgres'}

conn = psycopg2.connect(**DB)
cur = conn.cursor()

print("Computing per-accession pan-genome stats...")

cur.execute("""
    INSERT INTO pan_genome_stats 
        (accession_id, core_genes_count, soft_core_genes_count,
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

print("\nFinal cluster breakdown:")
cur.execute("SELECT cluster_type, COUNT(*) FROM pan_gene_clusters GROUP BY cluster_type ORDER BY 2 DESC")
for ct, n in cur.fetchall():
    print("  {:<12} {:>10,}".format(ct, n))

cur.close()
conn.close()
print("\nAll done.")