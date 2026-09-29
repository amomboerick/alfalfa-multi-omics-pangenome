# scripts/run_small_batches.py
import os, subprocess, glob, time

BASE     = r"C:\Users\Erick.Amombo\alfalfa_pangenome"
DIAMOND  = os.path.join(BASE, "bin", "diamond", "diamond.exe")
DB       = os.path.join(BASE, "protein_db.dmnd")
BATCHES  = os.path.join(BASE, "batches")
OUT      = os.path.join(BASE, "blast_all.tsv")

if os.path.exists(OUT):
    os.remove(OUT)

batch_files = sorted(glob.glob(os.path.join(BATCHES, "batch_*.faa")))
print("Found {} batches\n".format(len(batch_files)))

out_fh = open(OUT, "a", encoding="utf-8", buffering=1)
total_lines = 0
t_start = time.time()

for i, bf in enumerate(batch_files, 1):
    name = os.path.basename(bf)
    tsv  = bf.replace(".faa", ".tsv")

    if os.path.exists(tsv):
        os.remove(tsv)

    cmd = [
        DIAMOND, "blastp",
        "-d", DB,
        "-q", bf,
        "-o", tsv,
        "--fast",
        "-e", "1e-3",
        "--max-target-seqs", "50",
        "--threads", "4",
        "--outfmt", "6",
        "qseqid", "sseqid", "pident", "length", "evalue", "bitscore",
    ]

    t0 = time.time()
    rc = subprocess.call(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    dt = time.time() - t0

    if rc != 0 or not os.path.exists(tsv):
        print("[{:02d}/{}] {}  FAILED (rc={}, {:.0f}s)".format(i, len(batch_files), name, rc, dt))
        continue

    n = 0
    with open(tsv) as fh:
        for line in fh:
            out_fh.write(line)
            n += 1
    os.remove(tsv)
    total_lines += n
    elapsed = time.time() - t_start
    print("[{:02d}/{}] {}  OK  {:.0f}s  {:>9,} lines  (total {:>10,}, elapsed {:.0f}m)".format(
        i, len(batch_files), name, dt, n, total_lines, elapsed/60))

out_fh.close()
print()
print("=" * 60)
print("All batches done.")
print("Output: {}".format(OUT))
print("Total lines: {:,}".format(total_lines))