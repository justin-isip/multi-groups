# Matched comparisons highlight divergent impacts of land-use change on the dominant orders of insects

This repository contains all the code and some data used in the second chapter of my thesis which is currently being prepared for submission.

Author(s): [Justin E Isip](mailto:j.isip@nhm.ac.uk) and [Andy Purvis](mailto:andy.purvis.@nhm.ac.uk).

## Data

* `/data` contains the cleaned data used in the analyses,

Raw data are not included in this repo due to size limitations. All raw and cleaned data are available from the [NHM Data Portal](). 

If you use the cleaned data please cite as follows: 
> Justin E Isip.. (). Dataset: Lepidosaur bite-force data. Natural History Museum Data Portal (data.nhm.ac.uk).

-------
## Analyses

The analysis code is divided into `.Rmd` files that run the analyses for each section of the paper/supplementary materials, and more detailed scripts for the figures found in the paper.

-------
## Other folders

* `/figures` contains the figures
* `/outputs` contains the statistical results for tables

-------
## Session Info
For reproducibility purposes, here is the output of `devtools::session_info()` used to perform the analyses in the publication.

## Checkpoint for reproducibility
To rerun all the code with packages as they existed on CRAN at time of our analyses we recommend using the `checkpoint` package, and running this code prior to the analysis:

```{r}
checkpoint("2021-14-05")
```
