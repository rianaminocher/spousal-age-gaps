spousal-age-gaps
============

[![License: CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## About this repository

This repository contains data and code to reproduce results from:

> Spousal age gaps and sexual conflict: A replication of Lawson et al. 2020

This code was written by Riana Minocher and Cody T. Ross under Creative Commons License [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). See LICENSE.md for details.

The code was written in R 4.0.3. Statistical models are fit using the Stan MCMC engine via the `rstan` package (2.21.2), which requires a C++ compiler. Installation instructions are available at https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started. The `rethinking` package (2.12) is required to process fitted model outputs - installation instructions at http://xcelab.net/rm/software/.

## Reproducing our analysis

To fit the models and produce the results reported in text:

1. In R, set the working directory to this folder (i.e. "spousal-age-gaps") containing the `readme.md`.

2. In the R console, execute the line `source("run_all.r")`.

This will create the figures and tables reported in text, and store them in the folder "output/".

## Navigating this repository

1. "data.Rdata"

This is an R data object, that contains the marriage networks, nomination networks, and individual-level variables, used in analyses. This data was produced using R scripts that processed the raw data collected at each site. These R scripts are not available in this repository, as they contain identifying information about individuals at particular sites. 

2. "R"

The "fit_" scripts fit Stan models, and the "plot_" scripts process the output of Stan models. The "summarize_data.R" script produces the summary statistics reported in the main text and supplement. The scripts should be executed in the order they are executed in the "run_all.r" script. 

3. "stan"

Each .stan file corresponds to a single model. These are fit with the R scripts "fit_".
