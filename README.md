[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17427823.svg)](https://doi.org/10.5281/zenodo.17427823)

# About This Repo

This repo documentation contains and describes the analytical workflow used to generate results for:

> **Astle, W., & Haus, S. (2025).**, *Driving with Influence: Exploring Crash Factors of Automated Systems In Different Roadway Contexts* 

The most relevant pieces are organized accordingly:

```
paper_1_driving_with_influence/
├── _targets.R
├── R/
│   │   ├── clean_data.R
│   │   ├── clean_data_helpers.R
│   │   ├── get_data.R
│   │   ├── make_rules.R
│   │   ├── get_rules_per_threshold.R
│   │   ├── get_calculations.R
│   │   ├── get_calculatons_helpers.R
│   │   └── plot.R
├── data/
│   ├── raw/
│   │   ├── ADS.csv
│   │   └── ADAS.csv
│   └── processed/
├── quarto/
│   ├── overview.html
│   └── overview.qmd
└── renv.lock
```

`_targets.R` contains the `{target}` pipeline workflow used to generate the results.

`\R` contains the the R code the the `_targets.R` file uses to process the data.

`\data` stores the raw data for processing.

`quarto` holds documentation on how to assess the various parts of the `{targets}` pipeline

`renv.lock` serves as the snapshot of the R environment to be able to reproduce the environment exactly
