# Formative Report: NCD-RisC Data Analysis

**Author:** Nepoliyan Ria, Hamsini Manjunath, Sancha Baretto /n
**Module:** Graduate Foundations of Statistics and Data Science /n
**Submission Date:** 10 November 2025

## Project Overview

This project investigates global trends in diabetes and obesity prevalence using data from the NCD-RisC. The analysis follows two cycles of the CRISP-DM framework and uses the `ProjectTemplate` structure for reproducibility. The final report is an R Markdown document featuring interactive `plotly` visualizations.

---

## Required R Packages

To run this analysis, you will need R and RStudio, along with the following R packages.

* `ProjectTemplate`: For structuring the project and loading data.
* `dplyr`: For all data manipulation.
* `ggplot2`: For static plots.
* `plotly`: For creating all interactive visualizations and maps.
* `rprojroot`: Used by `knitr` in the Rmd file to find the project's root directory.
* `knitr`: For knitting the R Markdown document.

You can install all required packages by running the following command in your R console:

```r
install.packages(c("ProjectTemplate", "dplyr", "ggplot2", "plotly", "rprojroot", "knitr"))
