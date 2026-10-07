# scripts/cluster_from_blast.py
# Build pan-gene clusters from diamond blastp output using graph clustering.

import sys, time
from collections import defaultdict, Counter

BLAST_TSV   = r"C:\Users\Erick.Amombo\alfalfa_pangenome\blast_all.tsv"
FASTA_CLEAN = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins_clean.faa"
OUTPUT_TSV  = r"C:\Users\Erick.Amombo\alfalfa_pangenome\diamond_clusters_new.tsv"

MIN_PIDENT   = 50.0    # 50% identity
MIN_COVER    = 0.7     # 70% coverage
MIN_BITSCORE = 50.0

print("Step 1: reading protein lengths...")
lengths = {}
current = None
curlen = 0
with open(FASTA_CLEAN, "r") as fh:
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

# Union-Find
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

print("\nStep 2: parsing BLAST hits and building graph...")
t0 = time.time()
lines = edges = 0
with open(BLAST_TSV, "r") as fh:
    for line in fh:
        lines += 1
        p = line.rstrip("\n").split("\t")
        if len(p) < 6:
            continue
        q, s = p[0], p[1]
        if q == s:
            continue
        try:
            pident = float(p[2]); alen = int(p[3])
            evalue = float(p[4]); bits = float(p[5])
        except ValueError:
            continue
        if pident < MIN_PIDENT or bits < MIN_BITSCORE:
            continue
        Lq = lengths.get(q, 0); Ls = lengths.get(s, 0)
        if Lq == 0 or Ls == 0:
            continue
        cov = alen / min(Lq, Ls)
        if cov < MIN_COVER:
            continue
        get(q); get(s)
        union(q, s)
        edges += 1
        if lines % 5_000_000 == 0:
            print("  {:,} lines, {:,} edges, {:.0f}s".format(lines, edges, time.time()-t0))

print("  Total lines:   {:,}".format(lines))
print("  Edges kept:    {:,}".format(edges))
print("  Proteins:      {:,}".format(len(parent)))

print("\nStep 3: collecting clusters...")
clusters = defaultdict(list)
for prot in parent:
    clusters[find(prot)].append(prot)

print("Step 4: adding singleton proteins...")
with open(FASTA_CLEAN) as fh:
    for line in fh:
        if line.startswith(">"):
            pid = line[1:].split()[0]
            if pid not in parent:
                clusters[pid].append(pid)

print("  Total clusters: {:,}".format(len(clusters)))

print("\nStep 5: writing TSV...")
with open(OUTPUT_TSV, "w") as out:
    for rep, members in clusters.items():
        for m in members:
            out.write("{}\t{}\n".format(m, rep))
print("  Wrote:", OUTPUT_TSV)

dist = Counter(len(v) for v in clusters.values())
print("\nCluster size distribution:")
for sz in sorted(dist.keys())[:20]:
    print("  size {:>4}   clusters {:>10,}".format(sz, dist[sz]))
print("  max cluster size: {}".format(max(dist.keys())))
print("\nDone.")