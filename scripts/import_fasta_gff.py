# scripts/import_fasta_gff.py
# Import Medicago super-pan-genome FASTA + GFF into PostgreSQL

import os, sys, glob
import psycopg2
from Bio import SeqIO
from Bio.SeqUtils import gc_fraction

DB_CONFIG = {
    'host': 'localhost',
    'port': 5433,
    'database': 'alfalfa_pangenome',
    'user': 'postgres',
    'password': 'postgres'   # change if you set a password
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

try:
    conn = psycopg2.connect(**DB_CONFIG)
    conn.autocommit = False
    print("Connected to PostgreSQL (port 5433)\n")
except Exception as e:
    print(f"Cannot connect: {e}"); sys.exit(1)

cursor = conn.cursor()

fasta_files = sorted(
    glob.glob(os.path.join(FASTA_DIR, "*.fasta")) +
    glob.glob(os.path.join(FASTA_DIR, "*.fa")) +
    glob.glob(os.path.join(FASTA_DIR, "*.fna")) +
    glob.glob(os.path.join(FASTA_DIR, "*.fas"))
)

if not fasta_files:
    print(f"No FASTA files in {FASTA_DIR}"); sys.exit(1)

print(f"Found {len(fasta_files)} FASTA files\n" + "="*60)

for fasta_path in fasta_files:
    base = os.path.splitext(os.path.basename(fasta_path))[0]
    species, subspecies, cultivar = classify(base)
    print(f"\nAccession: {base}")
    print(f"   Species: {species} {subspecies or ''} {cultivar or ''}".rstrip())

    gff_path = None
    for ext in (".gff3", ".gff", ".gtf"):
        cand = os.path.join(GFF_DIR, base + ext)
        if os.path.exists(cand):
            gff_path = cand; break
    if gff_path:
        print(f"   GFF: {os.path.basename(gff_path)}")
    else:
        print("   WARNING: no matching GFF")

    try:
        sequences = list(SeqIO.parse(fasta_path, "fasta"))
        if not sequences:
            print("   Empty FASTA, skipping"); continue

        total_len = sum(len(s.seq) for s in sequences)
        avg_gc    = sum(gc_fraction(s.seq)*100 for s in sequences) / len(sequences)
        lengths   = sorted((len(s.seq) for s in sequences), reverse=True)
        cum, n50 = 0, 0
        for L in lengths:
            cum += L
            if cum >= total_len/2: n50 = L; break

        cursor.execute("""
            INSERT INTO accessions
                (accession_name, species, subspecies, cultivar, ploidy,
                 genome_size, gc_content, n50, contig_count, description)
            VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
            ON CONFLICT (accession_name) DO UPDATE SET
                species=EXCLUDED.species, subspecies=EXCLUDED.subspecies,
                cultivar=EXCLUDED.cultivar, genome_size=EXCLUDED.genome_size,
                gc_content=EXCLUDED.gc_content, n50=EXCLUDED.n50,
                contig_count=EXCLUDED.contig_count
            RETURNING accession_id
        """, (base, species, subspecies, cultivar, "diploid",
              total_len, round(avg_gc,2), n50, len(sequences),
              f"Imported from {os.path.basename(fasta_path)}"))
        accession_id = cursor.fetchone()[0]

        contig_id_map = {}
        for seq in sequences:
            name = seq.id
            is_chr = ('chr' in name.lower()) or ('chromosome' in name.lower())
            cursor.execute("""
                INSERT INTO contigs
                    (accession_id, contig_name, contig_length, gc_content, sequence, is_chromosome)
                VALUES (%s,%s,%s,%s,%s,%s)
                ON CONFLICT (accession_id, contig_name) DO UPDATE SET
                    contig_length=EXCLUDED.contig_length
                RETURNING contig_id
            """, (accession_id, name, len(seq.seq),
                  round(gc_fraction(seq.seq)*100,2),
                  str(seq.seq)[:1_000_000], is_chr))
            contig_id_map[name] = cursor.fetchone()[0]

        print(f"   OK: {len(sequences):,} contigs | {total_len:,} bp | GC {avg_gc:.1f}%")

        gene_count = 0
        if gff_path:
            with open(gff_path, encoding="utf-8", errors="ignore") as fh:
                for line in fh:
                    if not line.strip() or line.startswith("#"): continue
                    p = line.rstrip("\n").split("\t")
                    if len(p) < 9: continue
                    seqid, source, ftype, start, end, score, strand, phase, attrs = p
                    if ftype.lower() not in ("gene","mrna","transcript"): continue
                    contig_id = contig_id_map.get(seqid)
                    if contig_id is None: continue

                    adict = {}
                    for item in attrs.strip().split(";"):
                        if "=" in item:
                            k,v = item.split("=",1); adict[k.strip()] = v.strip()

                    gene_name   = adict.get("ID") or adict.get("gene_id") or adict.get("Name") or f"{base}_g{gene_count+1}"
                    gene_symbol = adict.get("Name") or adict.get("gene_name")
                    biotype     = adict.get("biotype") or adict.get("gene_biotype")

                    try: score_val = float(score) if score not in (".","") else None
                    except: score_val = None
                    try: phase_val = int(phase) if phase not in (".","") else None
                    except: phase_val = None

                    cursor.execute("""
                        INSERT INTO genes
                            (accession_id, contig_id, gene_name, gene_symbol,
                             start_pos, end_pos, strand, gene_type, biotype,
                             score, phase, attributes)
                        VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
                    """, (accession_id, contig_id, gene_name, gene_symbol,
                          int(start), int(end),
                          strand if strand in ("+","-",".") else ".",
                          ftype, biotype, score_val, phase_val, attrs))
                    gene_count += 1

            cursor.execute("UPDATE accessions SET gene_count=%s WHERE accession_id=%s",
                           (gene_count, accession_id))
            print(f"   OK: {gene_count:,} genes imported")

        conn.commit()

    except Exception as e:
        conn.rollback()
        print(f"   Error: {e}")

print("\n" + "="*60 + "\nFINAL SUMMARY\n" + "="*60)
cursor.execute("SELECT COUNT(*) FROM accessions");          print(f"Accessions:  {cursor.fetchone()[0]}")
cursor.execute("SELECT COUNT(*) FROM contigs");             print(f"Contigs:     {cursor.fetchone()[0]:,}")
cursor.execute("SELECT COUNT(*) FROM genes");               print(f"Genes:       {cursor.fetchone()[0]:,}")
cursor.execute("SELECT SUM(genome_size) FROM accessions");  print(f"Total bp:    {cursor.fetchone()[0]:,}")
print("\nPer-accession breakdown:")
cursor.execute("""SELECT accession_name, species, contig_count, gene_count, genome_size
                  FROM accessions ORDER BY species, accession_name""")
for r in cursor.fetchall():
    print(f"  {r[0]:<35} {r[1]:<25} {r[2]:>5} ctgs  {r[3]:>7} genes  {r[4]:>14,} bp")

cursor.close(); conn.close()
print("\nImport complete.")