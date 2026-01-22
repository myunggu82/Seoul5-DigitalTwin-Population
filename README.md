

# Seoul5 Digital Twin Population (Seoul5-ABSP): Activity-Based Synthetic Population for Southwest Seoul
Synthetic population dataset and code for Seoul 5-districts digital twin (2024)

This repository contains the R code and synthetic population datasets
for constructing a digital twin population of approximately 3.7 million individuals
for five districts in Seoul, Republic of Korea
(Geumcheon, Guro, Gwanak, Yeongdeungpo, Dongjak).

## Contents
- data/: sample synthetic datasets (illustrative subsets only)
- code/: fully reproducible R pipeline (scripts + modular functions)
- docs/: data dictionary

## Reproducibility

This repository provides a fully reproducible pipeline for generating the Seoul5 digital twin population.

Due to data size and redistribution constraints, the processed input datasets are not included in this repository.
Users should place the required processed input files under `data/processed/` on their local machine
(see `data/README_data.md` for detailed input specifications).

##  Citation
A DOI will be available after the Zenodo release.

##  License
Code: MIT License
Data: CC-BY-4.0

The full synthetic population and validation outputs can be reproduced by running:

```bash
Rscript scripts/99_run_pipeline.R

The repository also includes small illustrative sample outputs under data/sample/,
which allow reviewers to inspect the structure and format of the generated datasets
without access to the full input data.

