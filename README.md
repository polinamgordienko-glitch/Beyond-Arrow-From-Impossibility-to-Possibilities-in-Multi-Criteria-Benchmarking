HELM–MMLU (v1.0.0)

This repository contains the code to reproduce our experiments on HELM MMLU (v1.0.0).  
The code reads HELM run folders, builds rankings for each MMLU subject, and then tests for:

- **Single-peakedness**
- **Group separability**
- **Distance-restrictedness**

We do **not** include HELM data files in this repo. Instead, we explain how to download them from the official HELM public bucket.


1) What you need

 Software
- **R** 

 R packages
- `jsonlite`
- `dplyr`
- `purrr`
- `tibble`
- `digest`

