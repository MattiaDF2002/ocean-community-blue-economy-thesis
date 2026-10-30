# Sustainable Blue Economy Startup Acceleration & Selection: A Quantitative Analysis of Ocean Community (2024–2026)

## Project Overview

This repository contains the data processing pipeline, statistical analysis scripts, and presentation materials for the Master's Thesis in **Management and Sustainability** at **Ca' Foscari University of Venice**:

> **Title:** *I meccanismi di selezione e l’evoluzione post-programma nelle startup della sustainable blue economy: Un’analisi quantitativa del caso Ocean Community*  
> **Candidate:** Mattia Dalla Fontana (Matr. 908793)  
> **Supervisor (Relatrice):** Ch.ma Prof.ssa Daria Slanzi  
> **Co-Supervisor / Reader (Controrelatrice):** Ch.ma Prof.ssa Marta Pittavino  
> **Host Organization:** Ocean Community (Lisbon, Portugal)

---

## Research Questions

1. **RQ1 (Ex-Ante Selection Criteria & Heuristics):**  
   *Which structural characteristics and heuristics influence the admission probability of sustainable blue economy startups into impact accelerator programs?*
- Analyzed on a Master Dataset of **173 applications** across the 2024, 2025, and 2026 cohorts (F6S platform).
- Evaluates the **Founder-First** philosophy vs. traditional Venture Capital *pick-the-winners* heuristics.

2. **RQ2 (Ex-Post Evolution & Needs-Based Taxonomy):**  
   *How do commercial traction, technological maturity (TRL/CRL/BRL), and survival rates evolve post-acceleration, and what distinct startup profiles emerge?*
- Analyzed via primary survey data from **15 tracked finalists (13 active startups)** co-designed with Ocean Community.
- Applies Agglomerative Hierarchical Cluster Analysis to uncover startup archetypes.

---

## Key Empirical Findings

- **B2B Model Dominance:** The B2B business model represents **96.7% of all admitted startups with known models** (p = 0.0077), proving essential for rapid financial validation in high-CapEx marine sectors.
- **Absence of "Fatal Flaws" (Founder-First Philosophy):** Structural variables like funding stage (p = 0.167), team size (p = 0.887), company age (p = 0.287), TRL (p = 0.406), and capital raised (p = 0.095) are **not** significantly associated with rejection.
- **High Post-Acceleration Survival & Bridge Building:**
  - **86.7%** survival rate among tracked finalists.
  - **100%** commercial (CRL) and business model (BRL) advancement.
  - **92.3%** admitted to subsequent global accelerators (Bridge Builder role).
  - **77.8%** identified the **Peer Community** of fellow founders as the most valuable asset.
- **Taxonomy of 3 Archetypal Clusters (Ward.D2 Algorithm):**
  - **Cluster 1 (30.8%):** Early-Stage High Potential (TRL 4, solo-founders, technical focus).
  - **Cluster 2 (53.8%):** Commercial Scale-Up (B2B revenues, field-tested prototypes, financial barrier).
  - **Cluster 3 (15.4%):** Capital-Rich Deep-Tech with High External Barriers (€1.8M–€4.7M raised, yet severe regulatory and capital hurdles).

---

## Methodology & Statistical Stack

- **Data Auditing & Pre-processing:** R pipeline, currency conversion to EUR via historical Banca d'Italia exchange rates, handling of missing values without synthetic imputation.
- **Bivariate Inferential Analysis:**
  - **Welch's Two-Sample t-test** (for unbalanced sample sizes n1=37 vs. n2=136 and unequal variances).
  - **Pearson's Chi-Square Test with Monte Carlo Simulation** (10^5 to 10^6 replicates) for sparse contingency tables.
- **Unsupervised Multivariate Analysis:**
  - **Agglomerative Hierarchical Clustering** on 11 Likert active variables (6 barriers + 5 services).
  - Z-score standardization (`scale()`), **Euclidean Distance**, and **Ward.D2** minimum variance linkage.
  - Dendrogram cut at K = 3 clusters based on fusion height jump (5.403 to 7.523).

---

## 🛠️ Repository Structure

```text
├── README.md                   # Main project documentation
├── .gitignore                  # Git ignore rules for R binaries
├── data/                       # 🔒 [NOT UPLOADED DUE TO NDA] - Raw & processed datasets
├── docs/
│   ├── Presentazione_Tesi_Dalla_Fontana.pdf
└── scripts/
    ├── Analisi_Survey.R
    ├── CLUSTER_ANALYSIS.R
    ├── EDA_2024.R
    ├── EDA_2025.R
    ├── EDA_2026.R
    └── EDA_Master.R
```


---

## Prerequisites & Usage
To reproduce the analysis, ensure you have R (>= 4.0.0) installed along with the following packages:dplyr, readr, stringr, ggplot2, factoextra
Execution Order:
Run scripts/01_data_preprocessing.R to clean and standardize the raw data.
Run scripts/02_eda.R for descriptive and inferential statistics and visualizations.
Run scripts/03_survey_cluster_analysis_rq2.R to analysis survey data and generate the hierarchical clustering model.

Data Confidentiality Note: Due to Non-Disclosure Agreements regarding startup financial data and proprietary scoring from Ocean Community, the data/raw/ directory is excluded from this public repository. The scripts can be reviewed for methodological soundness, but will require the original datasets to run locally.

---

## Citation & Acknowledgments
Special thanks to Ocean Community (Lisbon) and founder Susanne Wedin-Schildt for providing access to primary application data and collaborating on the survey instrument during my research internship.

---

## Author & Contact
Mattia Dalla Fontana
MSc in Management and Sustainability

LinkedIn: www.linkedin.com/in/mattia-dalla-fontana
