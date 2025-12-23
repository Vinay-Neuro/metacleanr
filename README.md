# metacleanr

**metacleanr** — A reproducible pre-analysis validation and harmonization toolkit for meta-analysis datasets (R).

Quick: it flags common extraction errors (missing values, inconsistent study IDs, non-numeric strings in numeric columns, events>n, decimals in counts, outliers, subgroup mismatch), suggests harmonizations (e.g., drug name clustering), and optionally applies safe fixes (e.g., median→mean conversions with documented methods).


devtools::install_github("Vinay-Neuro/metacleanr")
