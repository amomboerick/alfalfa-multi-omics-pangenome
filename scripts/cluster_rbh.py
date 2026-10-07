# scripts/cluster_rbh.py
# Reciprocal-best-hit clustering (solves the hairball problem)
# Input:  blast_all.tsv (27.9M hits)
# Output: diamond_clusters_rbh.tsv

import os, time
from collections import defaultdict, Counter

BLAST_TSV   = r"C:\Users\Erick.Amombo\alfalfa_pangenome\blast_all.tsv"
FASTA_CLEAN = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins_clean.faa"
OUTPUT_TSV  = r"C:\Users\Erick.Amombo\alfalfa_pangenome\diamond_clusters_rbh.tsv"

MIN_PIDENT   = 40.0
MIN_BITSCORE = 100.0

# ---------------------------------------------------------
# Step 1: Read protein lengths
# ---------------------------------------------------------
print("Step 1: reading protein lengths...")
lengths = {}
current = None
curlen = 0
with open(FASTA_CLEAN) as fh:
    for line in fh:
        if line.startswith(">"):
            if current:
                lengths[current] = curlen
            current = line[1:].split()[0]
            curlen = 0
        else:
            curlen += len(line.strip())
    if current:
        lengths[current] = curlen
print("  {:,} proteins".format(len(lengths)))

# ---------------------------------------------------------
# Step 2: Parse BLAST, keep ONLY reciprocal best hits
# ---------------------------------------------------------
print("\nStep 2: parsing BLAST and finding reciprocal best hits...")
t0 = time.time()

# For each query, keep its single best hit
best_for_query = {}   # q -> (s, bits, pident)

lines = 0
with open(BLAST_TSV) as fh:
    for line in fh:
        lines += 1
        p = line.rstrip("\n").split("\t")
        if len(p) < 6:
            continue
        q, s = p[0], p[1]
        if q == s:
            continue
        try:
            pident = float(p[2])
            bits   = float(p[5])
        except ValueError:
            continue
        if pident < MIN_PIDENT or bits < MIN_BITSCORE:
            continue

        # Keep only best hit per query
        cur_best = best_for_query.get(q)
        if cur_best is None or bits > cur_best[1]:
            best_for_query[q] = (s, bits, pident)

        if lines % 5_000_000 == 0:
            print("  {:,} lines, {:,} best hits, {:.0f}s".format(lines, len(best_for_query), time.time()-t0))

print("  Total lines:    {:,}".format(lines))
print("  Best hits:      {:,}".format(len(best_for_query)))

# Reciprocal check: keep only edges where A->B and B->A agree
reciprocal = []
for q, (s, bits, pident) in best_for_query.items():
    bs = best_for_query.get(s)
    if bs and bs[0] == q:
        reciprocal.append((q, s, bits, pident))

print("  Reciprocal hits: {:,}".format(len(reciprocal)))

# ---------------------------------------------------------
# Step 3: Union-Find on reciprocal hits only
# ---------------------------------------------------------
print("\nStep 3: building clusters from reciprocal hits...")
parent = {}
def find(x):
    root = x
    while parent[root] != root:
        root = parent[root]
    while parent[x] != root:
        parent[x], x = root, parent[x]
    return root
def union(a, b):
    ra, rb = find(a), find(b)
    if ra != rb:
        parent[ra] = rb
def get(x):
    if x not in parent:
        parent[x] = x
    return x

for q, s, _, _ in reciprocal:
    get(q); get(s)
    union(q, s)

# Collect clusters
clusters = defaultdict(list)
for prot in parent:
    clusters[find(prot)].append(prot)

print("  Clusters: {:,}".format(len(clusters)))

# ---------------------------------------------------------
# Step 4: Add singleton proteins
# ---------------------------------------------------------
print("\nStep 4: adding singletons...")
with open(FASTA_CLEAN) as fh:
    for line in fh:
        if line.startswith(">"):
            pid = line[1:].split()[0]
            if pid not in parent:
                clusters[pid].append(pid)
print("  Total clusters: {:,}".format(len(clusters)))

# ---------------------------------------------------------
# Step 5: Write TSV
# ---------------------------------------------------------
print("\nStep 5: writing TSV...")
with open(OUTPUT_TSV, "w") as out:
    for rep, members in clusters.items():
        for m in members:
            out.write("{}\t{}\n".format(m, rep))
print("  Wrote:", OUTPUT_TSV)

# Distribution
dist = Counter(len(v) for v in clusters.values())
print("\nCluster size distribution:")
for sz in sorted(dist.keys())[:25]:
    print("  size {:>4}   clusters {:>10,}".format(sz, dist[sz]))
print("  max cluster size: {}".format(max(dist.keys())))
print("\nDone.")