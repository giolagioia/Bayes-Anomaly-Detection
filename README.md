# Bayesian Fraud Detection

This project analyzes anomalous bank transactions using a Bayesian workflow.
Since the dataset does not contain true fraud labels, anomaly labels are first
constructed with Isolation Forest and then modeled with Bayesian methods.

## Project Structure

```text
.
├── R/
│   ├── main.R
│   ├── preprocessing.R
│   ├── anomalyIF.R
│   ├── bayes.R
│   ├── bayesEvol.R
│   ├── mcmc.R
│   └── comparison.R
├── plots/
├── README.md
└── Bayesian_anomaly_detection.Rproj
```

The input file `bank.xlsx` is expected in the project root. It can be downloaded from https://www.kaggle.com/datasets/apoorvwatsky/bank-transaction-data before running the analysis.

## Requirements

Install the required R packages before running the analysis:

```r
install.packages("readxl")
install.packages("ggplot2")
install.packages("pROC")
install.packages("brms")
install.packages("rstan")
install.packages("isotree")
```

`brms` uses Stan for MCMC sampling, so the first run may take longer because the
Stan model has to be compiled.

## Run

Run the full pipeline from the project root:

```sh
Rscript R/main.R
```

The script executes the following steps:

1. Preprocesses the bank transaction data.
2. Constructs anomaly labels using Isolation Forest.
3. Estimates the global anomaly rate with Beta-Binomial models.
4. Tracks sequential Bayesian learning over time.
5. Fits Bayesian and frequentist logistic regression models.
6. Compares predictive performance on the test set.

## Outputs

The console output is organized into metric sections:

- `ISOLATION FOREST LABELS`
- `BETA-BINOMIAL MODEL`
- `SEQUENTIAL BAYESIAN LEARNING`
- `TRAIN/TEST SPLIT`
- `MCMC DIAGNOSTICS`
- `PREDICTIVE PERFORMANCE ON TEST SET`
- `BAYESIAN PREDICTIVE PROBABILITIES`

The pipeline saves the main figures in `plots/`:

- `01_isolation_forest_scores.png`
- `02_beta_posterior_comparison.png`
- `03_sequential_bayesian_learning.png`
- `04_roc_comparison.png`
- `05_bayesian_predictive_uncertainty.png`

## Reproducibility

The train/test split and Bayesian logistic regression use fixed seeds. The
Bayesian model is fitted with four chains, 1200 iterations per chain, and 600
warm-up iterations.
