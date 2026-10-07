@echo off
cd /D C:\Users\Erick.Amombo\alfalfa_pangenome

set DIAMOND=C:\Users\Erick.Amombo\alfalfa_pangenome\bin\diamond\diamond.exe

if exist blast_all.tsv del blast_all.tsv

for %%F in (batches\batch_*.faa) do (
    echo ================================================
    echo Running %%F
    echo ================================================
    "%DIAMOND%" blastp -d protein_db.dmnd -q "%%F" -o "%%~nF.tsv" --fast -e 1e-3 --max-target-seqs 50 --threads 4 --outfmt 6 qseqid sseqid pident length evalue bitscore
    type "%%~nF.tsv" >> blast_all.tsv
    del "%%~nF.tsv"
)

echo All batches done.