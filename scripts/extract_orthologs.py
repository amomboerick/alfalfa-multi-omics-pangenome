# scripts/extract_orthologs.py
# Extract protein sequences per cluster -- with accession-specific normalization.

import os, csv, re
from collections import defaultdict

BASE = r"C:\Users\Erick.Amombo\alfalfa_pangenome"
CSV  = os.path.join(BASE, "analysis", "phylogenetics", "representative_genes.csv")
OUT  = os.path.join(BASE, "analysis", "phylogenetics", "clusters")
PROT = os.path.join(BASE, "data", "proteins")

os.makedirs(OUT, exist_ok=True)

acc_files = {
    "M_arabica":                   "M_arabica.faa",
    "M_lupulina":                  "M_lupulina.faa",
    "M_polymorpha":                "M_polymorpha.faa",
    "M_ruthenica_landa":           "M_ruthenica_landa.faa",
    "M_ruthenica_zhiwusuo":        "M_ruthenica_zhiwusuo.faa",
    "M_sativa_ssp_caerulea_landa": "M_sativa_ssp_caerulea_landa.faa",
    "M_sativa_ssp_caerulea_long":  "M_sativa_ssp_caerulea_long.faa",
    "M_sativa_XJDY":               "M_sativa_XJDY.faa",
    "M_sativa_ZM1":                "M_sativa_ZM1.faa",
    "M_sativa_ZM4_hap4":           "M_sativa_ZM4_hap4.faa",
    "M_truncatula_A17":            "M_truncatula_A17.faa",
    "M_truncatula_HM078":          "M_truncatula_HM078.faa",
}

def normalize(header, acc):
    """Convert a FASTA header into the DB gene_name format for a given accession."""
    h = header.split()[0]  # take first token

    # A17: LOC_00026963-mRNA-1 -> LOC_00026963
    if acc == "M_truncatula_A17":
        h = re.sub(r"-mRNA-\d+$", "", h)
        return h

    # Lupulina: evm.model.ChrXX.N -> evm.TU.ChrXX.N
    if acc == "M_lupulina":
        h = h.replace("evm.model.", "evm.TU.")
        return h

    # All others: strip trailing .t<digits>
    h = re.sub(r"\.t\d+$", "", h)
    return h

# ---- Load cluster -> {accession: gene_name} ----
clusters = defaultdict(dict)
with open(CSV, "r", encoding="utf-8") as f:
    for row in csv.DictReader(f):
        clusters[row["cluster_id"]][row["accession_name"]] = row["gene_name"]

print(f"Loaded {len(clusters)} clusters")

needed = {(a, g) for per in clusters.values() for a, g in per.items()}
print(f"Looking for {len(needed)} sequences")

# ---- Read each FASTA, normalize headers ----
seqs_by_acc = {acc: {} for acc in acc_files}

for acc, fname in acc_files.items():
    path = os.path.join(PROT, fname)
    if not os.path.exists(path):
        print(f"  WARNING: {path} not found")
        continue
    wanted = {g for (a, g) in needed if a == acc}
    cur_name, cur_seq = None, []
    with open(path, "r", encoding="utf-8", errors="ignore") as fh:
        for line in fh:
            line = line.rstrip()
            if line.startswith(">"):
                if cur_name and cur_name in wanted:
                    seqs_by_acc[acc][cur_name] = "".join(cur_seq)
                cur_name = normalize(line[1:], acc)
                cur_seq = []
            else:
                cur_seq.append(line)
        if cur_name and cur_name in wanted:
            seqs_by_acc[acc][cur_name] = "".join(cur_seq)
    print(f"  {acc}: {len(seqs_by_acc[acc])} / {len(wanted)}")

# ---- Write per-cluster FASTA files ----
written, missing = 0, 0
missing_list = []
for cid, per_acc in clusters.items():
    with open(os.path.join(OUT, f"cluster_{cid}.faa"), "w", encoding="utf-8") as out:
        for acc, gene in per_acc.items():
            s = seqs_by_acc.get(acc, {}).get(gene)
            if s:
                out.write(f">{acc}|{gene}\n{s}\n")
                written += 1
            else:
                missing += 1
                missing_list.append((cid, acc, gene))

print(f"\nWritten: {written} sequences")
print(f"Missing: {missing} sequences")
print(f"Files:   {len(clusters)} FASTA files in {OUT}")

if missing_list[:5]:
    print("\nFirst 5 still missing:")
    for cid, acc, gene in missing_list[:5]:
        print(f"  cluster {cid}, {acc}, {gene}")