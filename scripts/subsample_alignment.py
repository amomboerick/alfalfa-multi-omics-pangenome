# scripts/subsample_alignment.py
# Subsample supermatrix by taking every 10th column.

import os

BASE = r"C:\Users\Erick.Amombo\alfalfa_pangenome\analysis\phylogenetics"
IN   = os.path.join(BASE, "supermatrix.fasta")
OUT  = os.path.join(BASE, "supermatrix_subset.fasta")

seqs = {}
cur = None
with open(IN, "r") as f:
    for line in f:
        line = line.rstrip()
        if line.startswith(">"):
            cur = line
            seqs[cur] = ""
        else:
            seqs[cur] += line

n_seqs = len(seqs)
n_cols = len(next(iter(seqs.values())))
print(f"Loaded {n_seqs} sequences, {n_cols} columns each")

with open(OUT, "w") as f:
    for k, v in seqs.items():
        sub = v[::10]
        f.write(k + "\n")
        for j in range(0, len(sub), 80):
            f.write(sub[j:j+80] + "\n")

sub_len = len(next(iter(seqs.values()))[::10])
print(f"Wrote {OUT}")
print(f"New alignment: {n_seqs} sequences x {sub_len} columns")