# Matched comparisons highlight divergent impacts of land-use change on the dominant orders of insects

This repository contains all the code and some data used in the [paper](ADD LINK WHEN PUBLISHED).

Author(s): [Justin E Isip](mailto:j.isip@nhm.ac.uk) and [Andy Purvis](mailto:andy.purvis.@nhm.ac.uk).

To cite the paper:

Matched comparisons highlight divergent impacts of land-use change on the dominant orders of insects. 2026. Justin E Isip and Andy Purvis. ADD DETAILS WHEN PUBLISHED.

To cite this repo:

Justin Isip. Code for the paper v1. GitHub: nhcooper123/multi-groups. Zenodo. DOI: ADD ON PUBLICATION

ADD ZENODO BADGE WHEN PUBLISHED

## Data

* `/data` contains the cleaned data used in the analyses,

Raw data are not included in this repo due to size limitations. All raw and cleaned data are available from the [NHM Data Portal](). 

If you use the cleaned data please cite as follows: 
> Justin E Isip.. (). Dataset: Lepidosaur bite-force data. Natural History Museum Data Portal (data.nhm.ac.uk).

-------
## Analyses

The analysis code is divided into `.Rmd` files in the following sections. 

Data preparation

Analyses



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
checkpoint("")
```
