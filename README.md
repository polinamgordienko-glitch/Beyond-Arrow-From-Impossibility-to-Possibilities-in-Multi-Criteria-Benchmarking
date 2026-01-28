# Beyond Arrow: From Impossibility to Possibilities in Multi-Criteria Benchmarking

## Introduction

This repository contains the code to reproduce our experiments on HELM MMLU (v1.0.0).  
The code reads HELM run folders, builds rankings for each MMLU subject, and then tests for:

- **Single-peakedness**
- **Group separability**
- **Distance-restrictedness**

The code was tested with

- R version 4.4.2
- R version 4.5.2

on

- macOS Sequoia 15.7.3
- Windows 11

### Data: HELM MMLU raw results (v1.0.0)

Option A: Download the HELM MMLU raw results under this link: https://zenodo.org/records/18402602?preview=1&token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjI1MDc3YWZjLWIzYWEtNDUzYy05NzBkLTY4OTA5NmEwMjcwOSIsImRhdGEiOnt9LCJyYW5kb20iOiI4NmI4MjY1MWJkMWNiZTFmNzM5NDFiYmUyYTc2YTI0MiJ9.SfRSP6FsVUywpMI0iBcdhiRran2YRlGyeso2JPgoPxhl7KXCWtCiMcziKANBDr6V2zUqQUBR7WeeMhEdjGodAQ 

Option B: Download the HELM MMLU raw results from the public crfm-helm-public bucket in the Google Cloud Storage (GCS) by completing the following two steps:

	1.	Install the Google Cloud CLI (gcloud) by following Google’s official instructions: 
https://docs.cloud.google.com/sdk/docs/install-sdk

	2.	Download HELM MMLU raw results by following the official HELM instructions:
https://crfm-helm.readthedocs.io/en/latest/downloading_raw_results/

After downloading via Option A or B the code expects the following structure:

`helm_mmlu/
  runs/
    v1.0.0/
      <RUN_ID_1>/
        run_spec.json
        stats.json
      <RUN_ID_2>/
        run_spec.json
        stats.json
      ...`

### Data: PMLB & OpenML results

For our experiments on PMLB and OpenML we use the final results from Jansen et al. (2024). These can be downloaded under the following link: https://github.com/hannahblo/Statistical-Multicriteria-Benchmarking-via-the-GSD-Front. Download hannahblo/Statistical-Multicriteria-Benchmarking-via-the-GSD-Front and ensure the following files are available: pmlb_results/final_results.RDS (PMLB data) and openml_permutation_results/dat_openml_filter.rds (OpenML data). 


### Code
