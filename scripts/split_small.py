# scripts/split_small.py
import os, glob

IN  = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins_clean.faa"
OUT = r"C:\Users\Erick.Amombo\alfalfa_pangenome\batches"

if os.path.exists(OUT):
    for f in glob.glob(os.path.join(OUT, "*")):
        os.remove(f)
os.makedirs(OUT, exist_ok=True)

N = 40
total = 0
with open(IN) as fh:
    for line in fh:
        if line.startswith(">"):
            total += 1
per_batch = (total + N - 1) // N
print("Total: {:,}   Per batch: {:,}   Batches: {}".format(total, per_batch, N))

cur = 0
fh_out = None
count = 0
with open(IN) as fh:
    for line in fh:
        if line.startswith(">"):
            if count % per_batch == 0:
                if fh_out: fh_out.close()
                cur += 1
                path = os.path.join(OUT, "batch_{:03d}.faa".format(cur))
                fh_out = open(path, "w")
            count += 1
        fh_out.write(line)
if fh_out: fh_out.close()
print("Done. {} batches in {}".format(cur, OUT))