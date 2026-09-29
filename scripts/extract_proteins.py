# scripts/extract_proteins.py  (v3 - with contig name normalization)
import os, glob, re
from Bio import SeqIO
from Bio.Seq import Seq

BASE      = r"C:\Users\Erick.Amombo\alfalfa_pangenome\data"
FASTA_DIR = os.path.join(BASE, "accessions")
GFF_DIR   = os.path.join(BASE, "annotations")
OUT_DIR   = os.path.join(BASE, "proteins")
os.makedirs(OUT_DIR, exist_ok=True)

def norm(name):
    """Reduce chromosome naming to canonical form: chrN."""
    n = str(name).strip()
    n = re.sub(r'\.(fa|fasta|fna|fas|gff3?|gtf)$', '', n, flags=re.I)
    n = n.lower()
    m = re.search(r'(?:chromosome|chrom|chr)[_\-\s]*(\d+)', n)
    if m:
        return "chr" + str(int(m.group(1)))
    m = re.search(r'(\d+)\s*(?:\.\d+)?\s*$', n)
    if m:
        return "chr" + str(int(m.group(1)))
    return re.sub(r'[_.\-\s]', '', n)

def parse_attrs(attrs):
    adict = {}
    if "=" in attrs:
        for item in attrs.split(";"):
            item = item.strip()
            if not item:
                continue
            if "=" in item:
                k, v = item.split("=", 1)
                adict[k.strip()] = v.strip()
    else:
        for m in re.finditer(r'(\S+)\s+"([^"]*)"', attrs):
            adict[m.group(1)] = m.group(2)
    return adict

fasta_files = sorted(
    glob.glob(os.path.join(FASTA_DIR, "*.fa")) +
    glob.glob(os.path.join(FASTA_DIR, "*.fasta")) +
    glob.glob(os.path.join(FASTA_DIR, "*.fna")) +
    glob.glob(os.path.join(FASTA_DIR, "*.fas"))
)

print("Found {} FASTA files\n".format(len(fasta_files)))

for fasta_path in fasta_files:
    base = os.path.splitext(os.path.basename(fasta_path))[0]
    gff_path = None
    for ext in (".gff3", ".gff", ".gtf"):
        cand = os.path.join(GFF_DIR, base + ext)
        if os.path.exists(cand):
            gff_path = cand
            break
    if not gff_path:
        print("SKIP {}: no GFF".format(base))
        continue

    print(">>> {}".format(base))

    # ---- Load genome into memory (both raw IDs and normalized aliases) ----
    genome = {}
    for rec in SeqIO.parse(fasta_path, "fasta"):
        seqstr = str(rec.seq).upper()
        genome[rec.id]          = seqstr
        genome[norm(rec.id)]    = seqstr
    print("   Genome entries: {}  (raw + normalized aliases)".format(len(genome)))

    # ---- Parse GFF: collect CDS features grouped by transcript ----
    cds_by_transcript = {}
    cds_count = 0

    with open(gff_path, encoding="utf-8", errors="ignore") as fh:
        for line in fh:
            if line.startswith("#") or not line.strip():
                continue
            p = line.rstrip("\r\n").split("\t")
            if len(p) < 9:
                continue
            seqid = p[0].strip()
            ftype = p[2].strip().lower()
            if ftype != "cds":
                continue

            adict = parse_attrs(p[8])
            parent = (adict.get("Parent")
                      or adict.get("transcript_id")
                      or adict.get("mRNA")
                      or adict.get("ID"))
            if parent is None:
                continue

            start  = int(p[3]) - 1
            end    = int(p[4])
            strand = p[6]

            cds_by_transcript.setdefault(parent, []).append(
                (seqid, start, end, strand))
            cds_count += 1

    print("   CDS features: {}  |  transcripts: {}".format(
        cds_count, len(cds_by_transcript)))

    # ---- Translate each transcript ----
    out_path = os.path.join(OUT_DIR, base + ".faa")
    written  = 0
    skipped  = 0
    first_debug_done = False

    with open(out_path, "w") as out:
        for tid, pieces in cds_by_transcript.items():
            # Must all be on one contig
            contigs = set(p[0] for p in pieces)
            if len(contigs) != 1:
                skipped += 1
                continue

            contig = pieces[0][0]
            strand = pieces[0][3]

            # Debug the very first lookup
            if not first_debug_done:
                print("   First tid={!r}  contig={!r}".format(tid, contig))
                print("   Lookup found? {}".format(
                    (contig in genome) or (norm(contig) in genome)))
                first_debug_done = True

            # Try raw key first, then normalized alias
            seq = genome.get(contig) or genome.get(norm(contig))
            if seq is None:
                skipped += 1
                continue

            sorted_pieces = sorted(pieces, key=lambda x: x[1])
            try:
                cds_seq = "".join(seq[p[1]:p[2]] for p in sorted_pieces)
                if len(cds_seq) < 3:
                    skipped += 1
                    continue
                if strand == "-":
                    cds_seq = str(Seq(cds_seq).reverse_complement())
                # Trim to multiple of 3
                cds_seq = cds_seq[:len(cds_seq) - (len(cds_seq) % 3)]
                prot = str(Seq(cds_seq).translate(to_stop=True))
                if len(prot) < 10:
                    skipped += 1
                    continue
                out.write(">{}\n{}\n".format(tid, prot))
                written += 1
            except Exception:
                skipped += 1
                continue

    print("   Proteins written: {}  |  skipped: {}".format(written, skipped))

print("\nDone. Protein FASTAs are in: {}".format(OUT_DIR))