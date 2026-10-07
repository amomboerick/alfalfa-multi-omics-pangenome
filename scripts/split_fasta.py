# scripts/split_fasta.py
import os

IN  = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins_clean.faa"
OUT = r"C:\Users\Erick.Amombo\alfalfa_pangenome\batches"
N   = 8

os.makedirs(OUT, exist_ok=True)

total = 0
with open(IN) as fh:
    for line in fh:
        if line.startswith(">"):
            total += 1
per_batch = (total + N - 1) // N
print("Total sequences: {:,}".format(total))
print("Per batch:       {:,}".format(per_batch))

cur = 0
fh_out = None
count = 0
with open(IN) as fh:
    for line in fh:
        if line.startswith(">"):
            if count % per_batch == 0:
                if fh_out: fh_out.close()
                cur += 1
                path = os.path.join(OUT, "batch_{:02d}.faa".format(cur))
                fh_out = open(path, "w")
                print("  Writing", path)
            count += 1
        fh_out.write(line)
if fh_out: fh_out.close()
print("Done. {} batches written to {}".format(cur, OUT))