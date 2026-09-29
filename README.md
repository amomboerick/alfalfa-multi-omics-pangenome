# Alfalfa Multi-Omics Pan-Genome Database

A comprehensive multi-omics resource for exploring the structural and functional diversity of the *Medicago* genus (alfalfa).

![Status](https://img.shields.io/badge/status-active-success)
![R](https://img.shields.io/badge/R-4.1.3-blue)
![PostgreSQL](https://img.shields.io/badge/PostgreSQL-15-blue)
![License](https://img.shields.io/badge/license-MIT-green)

## Overview

This database integrates **12 high-quality *Medicago* genome assemblies** into a comprehensive pan-genome resource, with deep functional annotations and an AI-powered natural language interface.

### Key Statistics

| Metric | Value |
|---|---|
| **Accessions** | 12 (6 species) |
| **Genes** | 823,838 |
| **Pan-gene clusters** | 217,122 |
| **Core gene families** | 561 |
| **GO annotations** | 5,832,931 |
| **KEGG KO** | 449,222 |
| **KEGG pathways** | 2,025,290 |
| **KOG categories** | 855,743 |
| **Total annotation records** | 9,163,186 |

### Species Covered

- *Medicago arabica*
- *Medicago lupulina*
- *Medicago polymorpha*
- *Medicago ruthenica* (Landa, Zhiwusuo)
- *Medicago sativa* (XJDY, ZM1, ZM4_hap4, ssp. caerulea × 2)
- *Medicago truncatula* (A17, HM078, R108)

## Features

### 15 Interactive Tabs

1. **Home** – Overview with live statistics and quick actions
2. **Accessions** – Detailed genome assembly metrics
3. **Pan-Gene Clusters** – Browse 217k clusters with type filtering
4. **Top Gene Families** – 20 largest pan-gene families
5. **Presence/Absence** – Per-accession gene view
6. **Search** – Gene search with cluster context
7. **Download** – CSV export for all tables
8. **GO Browser** – Search genes by Gene Ontology terms
9. **KEGG Pathways** – KO numbers and pathway catalog
10. **KOG Classes** – COG functional category distribution
11. **Annotation Search** – Unified search across all annotation types
12. **AI Assistant** – Natural language → SQL chatbot
13. **Statistics** – Interactive charts and distributions
14. **Assembly Quality** – Genome QC metrics
15. **Species Comparison** – Shared cluster matrix
16. **Heatmap** – Presence/absence heatmap

### AI Assistant

Powered by **Groq** (Llama 3.3 70B). Ask questions like:

- *"How many core clusters are there?"*
- *"Which GO terms are most common in M_arabica?"*
- *"Show me the 10 largest pan-gene families"*
- *"List genes in M_sativa_ZM4 that have GO:0006952"*

## Installation

### Prerequisites

- R 4.1+
- PostgreSQL 15
- Python 3.10+
- Anaconda/Miniconda

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/<your-username>/alfalfa-multi-omics-pangenome.git
   cd alfalfa-multi-omics-pangenome