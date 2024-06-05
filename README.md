# JRCD_Floyd

This repository contains the code to replicate all analyses included the the article:

> Fine, Adam, Thiago R. Oliveira, Jon Jackson, Rick Trinkner, Ben Bradford, and Chris Posch. Did the murder of George Floyd damage public perceptions of police and law in the United States? Journal of Reserach in Crime and Delinquency (Accepted for publication).

Please cite this repository as:

> Oliveira, Thiago. (2024). *Replication materials for "Did the murder of George Floyd damage public perceptions of police and law in the United States"*. Accessed on XXXX.

Unfortunately, raw data necessary to replicate the analyses cannot be made publicly available now. Those interested in gaining access to the data should contact Adam Fine or Rick Trinkner. What follows is the code used to produce all analyses reported in the article.

### Structure

-   `data` is an empty directory which is populated when `1 - cleaning data.R`, `2 - measurement models.R`, and `4 - regression models.R` run
    -   `raw` is an empty directory with the raw survey data, which is protected 
-   `tables` is populated when `4 - regression models.R` run
-   `plots` is populated when `3b - time series plots` and `5 - regression plots.R` run.
-   `1 - cleaning data.R` loads the raw survey data and produces a clean data set used across all analyses
-   `2 - measurement models.R` estimates all measurement models and produces the respondent-change long dataset used across all analyses
-   `3a - descriptive stats.R` produces descriptive statistics
-   `3b - time series plots.R` produces plots included in Figures 1a, 1b, and 1c in the article
-   `4a - regression models.R` estimates all regression models reported in the article
-   `5 - regression plots.R` produces plots included in Figures 2a, 2b, and 2c in the article
-   `6 - supplemental and exploratory analysis.R` estimates the supplemental and exploratory analysis included in the artcle
-   `JRCD_Floyd.Rproj` is an R project

