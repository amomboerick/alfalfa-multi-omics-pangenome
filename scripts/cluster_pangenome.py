# scripts/cluster_pangenome.py
# Pan-genome clustering: RBH + high-confidence extension
# Solves both the hairball (previous) and the too-strict RBH (previous) problems.

import os, time
from collections import defaultdict, Counter

BLAST_TSV   = r"C:\Users\Erick.Amombo\alfalfa_pangenome\blast_all.tsv"
FASTA_CLEAN = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins_clean.faa"
OUTPUT_TSV  = r"C:\Users\Erick.Amombo\alfalfa_pangenome\diamond_clusters_pg.tsv"

# Thresholds
RBH_MIN_PIDENT = 40.0
RBH_MIN_BITS   = 100.0
EXT_MIN_PIDENT = 70.0     # extension: stricter identity
EXT_MIN_BITS   = 200.0
EXT_MIN_COVER  = 0.7

# ---------------------------------------------------------
# Step 1: Read protein lengths
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# Step 2: Parse BLAST once; keep best hit per query AND
#         keep strong hits for the extension pass
# ---------------------------------------------------------
print("\nStep 2: parsing BLAST...")
t0 = time.time()
best_for_query = {}
strong_hits    = []   # (q, s, bits) for hits meeting EXT thresholds

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

        # Best-hit bookkeeping (RBH seeds)
        if pident >= RBH_MIN_PIDENT and bits >= RBH_MIN_BITS:
            cur = best_for_query.get(q)
            if cur is None or bits > cur[1]:
                best_for_query[q] = (s, bits)

        # Extension hits
        if pident >= EXT_MIN_PIDENT and bits >= EXT_MIN_BITS:
            Lq = lengths.get(q, 0); Ls = lengths.get(s, 0)
            if Lq and Ls:
                cov = alen / min(Lq, Ls)
                if cov >= EXT_MIN_COVER:
                    strong_hits.append((q, s, bits))

        if lines % 5_000_000 == 0:
            print("  {:,} lines, {:,} best hits, {:,} strong, {:.0f}s".format(
                lines, len(best_for_query), len(strong_hits), time.time()-t0))

print("  Total lines:       {:,}".format(lines))
print("  Best hits:         {:,}".format(len(best_for_query)))
print("  Strong ext. hits:  {:,}".format(len(strong_hits)))

# ---------------------------------------------------------
# Step 3: Build RBH seeds
# ---------------------------------------------------------
print("\nStep 3: building RBH seeds...")
reciprocal = []
for q, (s, bits) in best_for_query.items():
    bs = best_for_query.get(s)
    if bs and bs[0] == q:
        reciprocal.append((q, s))
print("  Reciprocal hits: {:,}".format(len(reciprocal)))

# ---------------------------------------------------------
# Step 4: Union-Find with all edges = RBH + strong extension
#         (RBH seeds first, then extension)
# ---------------------------------------------------------
print("\nStep 4: Union-Find on RBH + extension...")
parent = {}
def find(x):
    r = x
    while parent[r] != r: r = parent[r]
    while parent[x] != r: parent[x], x = r, parent[x]
    return r
def union(a,b):
    ra, rb = find(a), find(b)
    if ra != rb: parent[ra] = rb
def get(x):
    if x not in parent: parent[x] = x
    return x

for q, s in reciprocal:
    get(q); get(s); union(q, s)

# Extension pass — only connect if BOTH endpoints already exist AND
# they don't create pathologically large components. This is where we
# control the "hairball" risk.
MAX_COMPONENT = 500   # cap to prevent runaway chains

def comp_size(x):
    root = find(x)
    return sum(1 for p in parent if find(p) == root)

added = 0
for q, s, _ in strong_hits:
    get(q); get(s)
    rq, rs = find(q), find(s)
    if rq == rs:
        continue
    # Union only if combined size stays under cap
    size_q = sum(1 for p in parent if find(p) == rq)
    size_s = sum(1 for p in parent if find(p) == rs)
    if size_q + size_s > MAX_COMPONENT:
        continue
    union(q, s)
    added += 1

print("  Extension edges added: {:,}".format(added))

# ---------------------------------------------------------
# Step 5: Collect clusters
# ---------------------------------------------------------
print("\nStep 5: collecting clusters...")
clusters = defaultdict(list)
for prot in parent:
    clusters[find(prot)].append(prot)

print("\nStep 6: adding singletons...")
with open(FASTA_CLEAN) as fh:
    for line in fh:
        if line.startswith(">"):
            pid = line[1:].split()[0]
            if pid not in parent:
                clusters[pid].append(pid)
print("  Total clusters: {:,}".format(len(clusters)))

# ---------------------------------------------------------
# Step 7: Write output
# ---------------------------------------------------------
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