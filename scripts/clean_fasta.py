# scripts/clean_fasta.py
# Remove Windows artifacts (Ctrl+Z, \r) from a FASTA file
# so that DIAMOND can parse it cleanly.

import os

IN_FILE  = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins.faa"
OUT_FILE = r"C:\Users\Erick.Amombo\alfalfa_pangenome\combined_proteins_clean.faa"

print("Reading {} ...".format(IN_FILE))

# Read as binary to catch ALL control characters
with open(IN_FILE, "rb") as fin:
    data = fin.read()

print("  Original size: {:,} bytes".format(len(data)))

# Strip Windows artifacts:
#   \x1a = Ctrl+Z (EOF marker)
#   \x00 = Null byte
#   \x0d = \r  (carriage return, keep \n only)
data = data.replace(b"\x1a", b"")            # Ctrl+Z
data = data.replace(b"\x00", b"")            # Nulls
data = data.replace(b"\r\n", b"\n")          # CRLF -> LF
data = data.replace(b"\r", b"\n")            # lone CR -> LF

print("  Cleaned size:  {:,} bytes".format(len(data)))

with open(OUT_FILE, "wb") as fout:
    fout.write(data)

print("Wrote:", OUT_FILE)
print("Done.")