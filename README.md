
# Background

This is the code repository for the research paper:

Fitzgerald O, Dyer S, Zegers-Hochschild F, Keller, E, Adamson GD, Chambers GM. (forthcoming). Gender inequality and utilisation of assisted reproductive technology: an international cross-sectional and longitudinal analysis.

![Cross sectional comparison of the GII and ART utilisation](results/graphs/gii_art_xsectional.png)

# Usage of the data

The data in the repository may be reused. To cite the original data source (UN, WB, ICMART), see the manuscript. If using the final linked dataset please cite the current work.

# Reproducing the analysis

The following steps will reproduce the results present in the paper. Bear in mind 
that as part of the analysis consists of Markov Chain Monte Carlo base imputation running the full analysis at once can take several hours.

## Step by step

If you are using RStudio or another GUI reproducing the analysis step by step 
may be the preferred method. To do so, run these files in the order described below.

First, set up the environment (install required packages). We use the package `renv` for reproducibility of the package environment.

```
install.packages("renv")
renv::restore()
```

Set up the analysis dataset:
```
scripts/install_packages.R
scripts/create_data/art_data.R
scripts/create_data/art_data_plus_covariates.R
```

Exploratory data analysis:
```
nbs/eda.Rmd
```

Perform the imputation analysis and fit models to the results:
```
scripts/impute/impute_missing_data.R
nbs/models-imputed-data.Rmd
```

Fit models to the complete case data:
```
nbs/models-complete-cases.Rmd
```

Graph the fitted models (note the models are saved in `models/`:
```
scripts/model_results.R
```
