# scripts/import_fasta_gff_v2.py
# Robust multi-species Medicago FASTA + GFF importer

import os, sys, glob, re
import psycopg2
from Bio import SeqIO
from Bio.SeqUtils import gc_fraction

DB_CONFIG = {
    'host': 'localhost',
    'port': 5433,
    'database': 'alfalfa_pangenome',
    'user': 'postgres',
    'password': 'postgres'
}

BASE_DIR  = r"C:\Users\Erick.Amombo\alfalfa_pangenome"
FASTA_DIR = os.path.join(BASE_DIR, "data", "accessions")
GFF_DIR   = os.path.join(BASE_DIR, "data", "annotations")


def classify(name):
    n = name.lower()
    if "arabica"            in n: return ("Medicago arabica",    None, None)
    if "lupulina"           in n: return ("Medicago lupulina",   None, None)
    if "polymorpha"         in n: return ("Medicago polymorpha", None, None)
    if "ruthenica_landa"    in n: return ("Medicago ruthenica",  None, "Landa")
    if "ruthenica_zhiwusuo" in n: return ("Medicago ruthenica",  None, "Zhiwusuo")
    if "caerulea_landa"     in n: return ("Medicago sativa", "caerulea", "Landa")
    if "caerulea_long"      in n: return ("Medicago sativa", "caerulea", "Long")
    if "xjdy"               in n: return ("Medicago sativa", None, "XJDY")
    if "zm1"                in n: return ("Medicago sativa", None, "ZM1")
    if "zm4"                in n: return ("Medicago sativa", None, "ZM4")
    if "truncatula_a17"     in n: return ("Medicago truncatula", None, "A17")
    if "truncatula_hm078"   in n: return ("Medicago truncatula", None, "HM078")
    return ("Medicago sp.", None, None)


def norm(name):
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
        for item in attrs.strip().split(";"):
            if "=" in item:
                k, v = item.split("=", 1)
                adict[k.strip()] = v.strip()
    else:
        for m in re.finditer(r'(\w+)\s+"([^"]*)"', attrs):
            adict[m.group(1)] = m.group(2)
    return adict


def main():
    try:
        conn = psycopg2.connect(**DB_CONFIG)
        conn.autocommit = False
        print("Connected to PostgreSQL (port 5433)\n")
    except Exception as e:
        print("Cannot connect: {}".format(e))
        sys.exit(1)

    cursor = conn.cursor()

    print("Clearing previous import...")
    cursor.execute("TRUNCATE gene_presence_absence, cluster_membership, "
                   "pan_gene_clusters, genes, contigs, accessions "
                   "RESTART IDENTITY CASCADE")
    conn.commit()
    print("Cleared.\n")

    fasta_files = sorted(
        glob.glob(os.path.join(FASTA_DIR, "*.fasta")) +
        glob.glob(os.path.join(FASTA_DIR, "*.fa")) +
        glob.glob(os.path.join(FASTA_DIR, "*.fna")) +
        glob.glob(os.path.join(FASTA_DIR, "*.fas"))
    )

    if not fasta_files:
        print("No FASTA files in {}".format(FASTA_DIR))
        sys.exit(1)

    print("Found {} FASTA files\n{}".format(len(fasta_files), "=" * 70))

    for fasta_path in fasta_files:
        base = os.path.splitext(os.path.basename(fasta_path))[0]
        species, subspecies, cultivar = classify(base)
        print("\n>>> {}  ({} {} {})".format(base, species,
                                            subspecies or '', cultivar or '').rstrip())

        gff_path = None
        for ext in (".gff3", ".gff", ".gtf"):
            cand = os.path.join(GFF_DIR, base + ext)
            if os.path.exists(cand):
                gff_path = cand
                break

        if not gff_path:
            print("   WARNING: no matching GFF - FASTA only")
        else:
            print("   GFF: {}".format(os.path.basename(gff_path)))

        try:
            sequences = list(SeqIO.parse(fasta_path, "fasta"))
            if not sequences:
                print("   Empty FASTA, skipping")
                continue

            total_len = sum(len(s.seq) for s in sequences)
            avg_gc = sum(gc_fraction(s.seq) * 100 for s in sequences) / len(sequences)
            lengths = sorted((len(s.seq) for s in sequences), reverse=True)
            cum, n50 = 0, 0
            for L in lengths:
                cum += L
                if cum >= total_len / 2:
                    n50 = L
                    break

            cursor.execute("""
                INSERT INTO accessions
                    (accession_name, species, subspecies, cultivar, ploidy,
                     genome_size, gc_content, n50, contig_count, description)
                VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
                RETURNING accession_id
            """, (base, species, subspecies, cultivar, "diploid",
                  total_len, round(avg_gc, 2), n50, len(sequences),
                  "Imported from {}".format(os.path.basename(fasta_path))))
            accession_id = cursor.fetchone()[0]

            contig_id_map = {}
            norm_map = {}
            for seq in sequences:
                name = seq.id
                is_chr = ('chr' in name.lower()) or ('chromosome' in name.lower())
                cursor.execute("""
                    INSERT INTO contigs
                        (accession_id, contig_name, contig_length, gc_content,
                         sequence, is_chromosome)
                    VALUES (%s,%s,%s,%s,%s,%s)
                    RETURNING contig_id
                """, (accession_id, name, len(seq.seq),
                      round(gc_fraction(seq.seq) * 100, 2),
                      str(seq.seq)[:1_000_000], is_chr))
                cid = cursor.fetchone()[0]
                contig_id_map[name] = cid
                norm_map[norm(name)] = cid

            print("   OK: {} contigs | {:,} bp | GC {:.1f}%".format(
                len(sequences), total_len, avg_gc))
            conn.commit()

            gene_count = 0
            unmatched = set()
            gene_lines_seen = 0

            if gff_path:
                with open(gff_path, encoding="utf-8", errors="ignore") as fh:
                    for line in fh:
                        if not line.strip() or line.startswith("#"):
                            continue
                        p = line.rstrip("\r\n").split("\t")
                        if len(p) < 9:
                            continue

                        seqid = p[0].strip()
                        ftype = p[2].strip().lower()
                        if ftype != "gene":
                            continue

                        gene_lines_seen += 1

                        # Debug first 3 rows for every accession
                        if gene_lines_seen <= 3:
                            print("      DEBUG row {}: seqid={!r} type={!r} attrs={!r}".format(
                                gene_lines_seen, seqid, ftype, p[8][:100]))

                        contig_id = contig_id_map.get(seqid)
                        if contig_id is None:
                            contig_id = norm_map.get(norm(seqid))
                        if contig_id is None:
                            unmatched.add(seqid)
                            continue

                        attrs = p[8]
                        adict = parse_attrs(attrs)

                        gene_name = (adict.get("ID")
                                     or adict.get("gene_id")
                                     or "{}_g{}".format(base, gene_count + 1))
                        gene_symbol = adict.get("Name") or adict.get("gene_name")
                        biotype = adict.get("biotype") or adict.get("gene_biotype")

                        try:
                            score_val = float(p[5]) if p[5] not in (".", "") else None
                        except Exception:
                            score_val = None
                        try:
                            phase_val = int(p[7]) if p[7] not in (".", "") else None
                        except Exception:
                            phase_val = None

                        strand = p[6] if p[6] in ("+", "-", ".") else "."

                        try:
                            cursor.execute("""
                                INSERT INTO genes
                                    (accession_id, contig_id, gene_name, gene_symbol,
                                     start_pos, end_pos, strand, gene_type, biotype,
                                     score, phase, attributes)
                                VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
                            """, (accession_id, contig_id, gene_name, gene_symbol,
                                  int(p[3]), int(p[4]), strand,
                                  p[2], biotype, score_val, phase_val, attrs))
                            gene_count += 1
                        except Exception as gene_err:
                            if gene_count < 5:
                                print("      INSERT FAILED on {}: {}".format(gene_name, gene_err))
                            conn.rollback()
                            cursor = conn.cursor()

                cursor.execute(
                    "UPDATE accessions SET gene_count=%s WHERE accession_id=%s",
                    (gene_count, accession_id))
                conn.commit()

                print("   GFF gene lines seen: {:,}".format(gene_lines_seen))
                print("   Genes inserted:      {:,}".format(gene_count))
                if unmatched:
                    print("   Unmatched GFF seqids ({}): {}".format(
                        len(unmatched), list(unmatched)[:5]))

        except Exception as e:
            conn.rollback()
            print("   ERROR: {}".format(e))

    print("\n" + "=" * 70 + "\nFINAL SUMMARY\n" + "=" * 70)
    cursor.execute("SELECT COUNT(*) FROM accessions")
    print("Accessions:  {}".format(cursor.fetchone()[0]))
    cursor.execute("SELECT COUNT(*) FROM contigs")
    print("Contigs:     {:,}".format(cursor.fetchone()[0]))
    cursor.execute("SELECT COUNT(*) FROM genes")
    print("Genes:       {:,}".format(cursor.fetchone()[0]))
    cursor.execute("SELECT SUM(genome_size) FROM accessions")
    print("Total bp:    {:,}".format(cursor.fetchone()[0]))

    print("\nPer-accession breakdown:")
    cursor.execute("""
        SELECT accession_name, species, contig_count, gene_count, genome_size
        FROM accessions ORDER BY species, accession_name
    """)
    for r in cursor.fetchall():
        print("  {:<35} {:<25} {:>3} ctgs  {:>7} genes  {:>14,} bp".format(
            r[0], r[1], r[2], r[3], r[4]))

    cursor.close()
    conn.close()
    print("\nImport complete.")


if __name__ == "__main__":
    main()