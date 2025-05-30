# Replication Files for "From Protest to Power: How the Women's March Worked" 

This repository contains replication files for all the analysis in my article "From Protest to Power: How the Women's March Worked", forthcoming in Social Forces.

The analysis requires installation of R and RStudio. The original analysis was run using R 4.5.0 and RStudio "Mariposa Orchid" Release (f0b76cc0, 2025-05-04)

## Install Dependencies

Please begin by installing the following R packages, available from
CRAN, used in the analysis.

- bookdown_0.41
- ivreg_0.6-5
- ivtools_2.3.0
- marginaleffects_0.26.0
- rnaturalearth_v1.0.1
- sf_1.0-21
- texreg_1.39.4
- tidyverse_2.0.0
- tigris_2.2.1

## Main Text Analysis

Running the main-text-tables-and-figures.R script will produce all figures and tables in the manuscript, as well as specific in-text references to the data.

## Appendix Analysis
The supplemental appendix is stored in the working-appendix.rmd file. To replicate the full appendix, including all tables and figures, the file should be output to pdf using the "knit" function. Note that the  specific format of the appendix is pdf_document2, a format from the  "bookdown" package, and thus requires installation of the bookdown  package, even though bookdown is not called in the markdown file itself.

