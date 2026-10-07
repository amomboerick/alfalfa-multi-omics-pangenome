# scripts/ask_alfalfa.py
# Two-step AI pipeline: NL -> SQL -> PostgreSQL -> NL answer
import os, sys, json, requests, psycopg2

# --- Configuration ---
GROQ_KEY = os.environ.get("GROQ_API_KEY", "")
if not GROQ_KEY:
    print("ERROR: GROQ_API_KEY not set"); sys.exit(1)

DB = dict(host="localhost", port=5433,
          database="alfalfa_pangenome",
          user="postgres", password="postgres")

MODEL = "qwen/qwen3.8-27b"
URL   = "https://api.groq.com/openai/v1/chat/completions"

SCHEMA_PROMPT = """You are an expert PostgreSQL assistant for the alfalfa pan-genome database.
Given a user question, respond with ONLY a JSON object of the form:
  {"sql": "<your SQL query here>"}
Do not include any text outside the JSON. Do not use markdown fences.
If the question cannot be answered with SQL, respond with:
  {"sql": null, "error": "<short reason>"}

Schema (PostgreSQL):
  accessions(accession_id, accession_name, species, gene_count, gc_content, n50, ...)
  genes(gene_id, accession_id, contig_id, gene_name, start_pos, end_pos, strand, ...)
  pan_gene_clusters(cluster_id, cluster_name, cluster_type, gene_count, presence_across_accessions)
  gene_presence_absence(pa_id, cluster_id, accession_id, is_present, copy_number)
  go_annotations(go_ann_id, gene_id, accession_id, go_term)
  kegg_ko(ko_id, gene_id, accession_id, ko_number)
  kegg_pathways(pathway_id, gene_id, accession_id, pathway_code)
  kog_annotations(kog_id, gene_id, accession_id, cog_letter)
  crispr_guides(guide_id, gene_id, accession_id, gene_name, spacer_seq, pam_seq,
                gc_pct, uniqueness, specificity_class, ...)

IMPORTANT: The table for pan-gene clusters is `pan_gene_clusters`, NOT `clusters`.
Core clusters are: cluster_type = 'core'.
Always LIMIT 100 unless the user asks for a count or aggregate.
"""

def ask_groq_for_sql(question):
    """Step 1: NL -> SQL via Groq JSON mode."""
    body = {
        "model": MODEL,
        "messages": [
            {"role": "system", "content": SCHEMA_PROMPT},
            {"role": "user",   "content": question}
        ],
        "temperature": 0.1,
        "max_tokens": 2000,
        "response_format": {"type": "json_object"}
    }
    r = requests.post(URL,
                      headers={"Authorization": f"Bearer {GROQ_KEY}",
                               "Content-Type": "application/json"},
                      json=body, timeout=60)
    print(f"[1] Groq SQL request -> HTTP {r.status_code}")
    r.raise_for_status()
    txt = r.json()["choices"][0]["message"]["content"]
    print(f"[1] Raw model content: {txt[:200]}")
    inner = json.loads(txt)
    if inner.get("error"):
        raise RuntimeError(f"Model error: {inner['error']}")
    return inner["sql"]

def run_sql(sql):
    """Step 2: execute SQL on PostgreSQL."""
    print(f"[2] Running SQL: {sql}")
    with psycopg2.connect(**DB) as conn:
        with conn.cursor() as cur:
            cur.execute(sql)
            cols = [d[0] for d in cur.description]
            rows = cur.fetchall()
    print(f"[2] Got {len(rows)} row(s), {len(cols)} col(s)")
    return cols, rows

def explain_result(question, cols, rows):
    """Step 3: turn the SQL result into a natural-language answer."""
    preview = f"columns={cols}, rows={rows[:5]}{'...' if len(rows) > 5 else ''}"
    body = {
        "model": MODEL,
        "messages": [
            {"role": "system", "content": "Answer the user's question in one short sentence based on the SQL result. Be concise."},
            {"role": "user",   "content": f"Question: {question}\nResult: {preview}"}
        ],
        "temperature": 0.1,
        "max_tokens": 200
    }
    r = requests.post(URL,
                      headers={"Authorization": f"Bearer {GROQ_KEY}",
                               "Content-Type": "application/json"},
                      json=body, timeout=60)
    print(f"[3] Groq explain request -> HTTP {r.status_code}")
    r.raise_for_status()
    return r.json()["choices"][0]["message"]["content"].strip()

if __name__ == "__main__":
    q = " ".join(sys.argv[1:]) or "How many core clusters are there?"
    print(f"\n=== Question: {q} ===\n")

    sql = ask_groq_for_sql(q)
    print(f"[1] Parsed SQL: {sql}\n")

    cols, rows = run_sql(sql)
    print(f"[2] First rows: {rows[:3]}\n")

    answer = explain_result(q, cols, rows)
    print(f"[3] Answer: {answer}\n")