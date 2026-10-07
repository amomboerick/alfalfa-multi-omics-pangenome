# scripts/cluster_pangenome_v2.py
# Optimized: track component sizes in a dict instead of counting each time.

import os, time
from collections import defaultdict, Counter

BLAST_TSV   = r"C:\Users\Erick.Amombo\alfalfa_pangenome\blast_all.tsv"
FASTA_CLEAN = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins_clean.faa"
OUTPUT_TSV  = r"C:\Users\Erick.Amombo\alfalfa_pangenome\diamond_clusters_pg.tsv"

RBH_MIN_PIDENT = 40.0
RBH_MIN_BITS   = 100.0
EXT_MIN_PIDENT = 70.0
EXT_MIN_BITS   = 200.0
EXT_MIN_COVER  = 0.7
MAX_COMPONENT  = 500

# Step 1 — read protein lengths
print("Step 1: reading protein lengths...")
lengths = {}
current = None; curlen = 0
with open(FASTA_CLEAN) as fh:
    for line in fh:
        if line.startswith(">"):
            if current: lengths[current] = curlen
            current = line[1:].split()[0]; curlen = 0
        else:
            curlen += len(line.strip())
    if current: lengths[current] = curlen
print("  {:,} proteins".format(len(lengths)))

# Step 2 — parse BLAST
print("\nStep 2: parsing BLAST...")
t0 = time.time()
best_for_query = {}
strong_hits    = []
lines = 0
with open(BLAST_TSV) as fh:
    for line in fh:
        lines += 1
        p = line.rstrip("\n").split("\t")
        if len(p) < 6: continue
        q, s = p[0], p[1]
        if q == s: continue
        try:
            pident = float(p[2]); alen = int(p[3])
            bits   = float(p[5])
        except ValueError:
            continue
        if pident >= RBH_MIN_PIDENT and bits >= RBH_MIN_BITS:
            cur = best_for_query.get(q)
            if cur is None or bits > cur[1]:
                best_for_query[q] = (s, bits)
        if pident >= EXT_MIN_PIDENT and bits >= EXT_MIN_BITS:
            Lq = lengths.get(q, 0); Ls = lengths.get(s, 0)
            if Lq and Ls:
                cov = alen / min(Lq, Ls)
                if cov >= EXT_MIN_COVER:
                    strong_hits.append((q, s))
        if lines % 5_000_000 == 0:
            print("  {:,} lines, {:,} best, {:,} strong, {:.0f}s".format(
                lines, len(best_for_query), len(strong_hits), time.time()-t0))
print("  Best hits:        {:,}".format(len(best_for_query)))
print("  Strong ext hits:  {:,}".format(len(strong_hits)))

# Step 3 — RBH seeds
print("\nStep 3: building RBH seeds...")
reciprocal = []
for q, (s, _) in best_for_query.items():
    bs = best_for_query.get(s)
    if bs and bs[0] == q:
        reciprocal.append((q, s))
print("  Reciprocal hits: {:,}".format(len(reciprocal)))

# Step 4 — Union-Find with efficient size tracking
print("\nStep 4: Union-Find (optimized)...")
parent = {}
size   = {}   # root -> component size

def find(x):
    r = x
    while parent[r] != r:
        r = parent[r]
    # path compression
    while parent[x] != r:
        parent[x], x = r, parent[x]
    return r

def add(x):
    if x not in parent:
        parent[x] = x
        size[x] = 1

def union(a, b):
    ra, rb = find(a), find(b)
    if ra == rb:
        return False
    sa, sb = size[ra], size[rb]
    if sa + sb > MAX_COMPONENT:
        return False
    # union by size (attach smaller to larger)
    if sa < sb:
        parent[ra] = rb
        size[rb] = sa + sb
        del size[ra]
    else:
        parent[rb] = ra
        size[ra] = sa + sb
        del size[rb]
    return True

# Seed with RBH
print("  Adding RBH seeds...")
for q, s in reciprocal:
    add(q); add(s)
    union(q, s)
print("  RBH edges processed, components: {:,}".format(len(size)))

# Extension pass
print("  Adding extension edges...")
added = 0
for i, (q, s) in enumerate(strong_hits):
    add(q); add(s)
    if union(q, s):
        added += 1
    if (i+1) % 2_000_000 == 0:
        print("    {:,} / {:,} extension edges, {:,} unions, {:,} components, {:.0f}s".format(
            i+1, len(strong_hits), added, len(size), time.time()-t0))
print("  Extension unions: {:,}".format(added))

# Step 5 — collect clusters
print("\nStep 5: collecting clusters...")
clusters = defaultdict(list)
for prot in parent:
    clusters[find(prot)].append(prot)
print("  Clusters with >1 member: {:,}".format(len(clusters)))

# Step 6 — singletons
print("\nStep 6: adding singletons...")
with open(FASTA_CLEAN) as fh:
    for line in fh:
        if line.startswith(">"):
            pid = line[1:].split()[0]
            if pid not in parent:
                clusters[pid].append(pid)
print("  Total clusters: {:,}".format(len(clusters)))

# Step 7 — write TSV
print("\nStep 7: writing TSV...")
with open(OUTPUT_TSV, "w") as out:
    for rep, members in clusters.items():
        for m in members:
            out.write("{}\t{}\n".format(m, rep))
print("  Wrote:", OUTPUT_TSV)

dist = Counter(len(v) for v in clusters.values())
print("\nCluster size distribution:")
for sz in sorted(dist.keys())[:25]:
    print("  size {:>4}   clusters {:>10,}".format(sz, dist[sz]))
print("  max cluster size: {}".format(max(dist.keys())))
print("\nDone.")