# scripts/check_cluster_sizes.py
from collections import Counter

TSV = r"C:\Users\Erick.Amombo\alfalfa_pangenome\diamond_clusters.tsv"

sizes = Counter()
total = 0
with open(TSV, encoding="utf-8", errors="ignore") as fh:
    for line in fh:
        line = line.rstrip("\r\n")
        if not line: continue
        p = line.split("\t")
        if len(p) < 2: continue
        rep = p[1].strip()
        sizes[rep] += 1
        total += 1

print("Total protein rows: {:,}".format(total))
print("Unique clusters:    {:,}".format(len(sizes)))
print()
print("Cluster size distribution:")
print("  size    num_clusters")
print("  ----    ------------")
for sz in sorted(sizes.values()):
    pass  # placeholder
dist = Counter(sizes.values())
for sz in sorted(dist.keys()):
    print("  {:>4}    {:>12,}".format(sz, dist[sz]))

print()
print("Largest cluster has {} proteins".format(max(sizes.values())))