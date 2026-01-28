# Beyond Arrow: From Impossibility to Possibilities in Multi-Criteria Benchmarking

## Introduction

This repository contains the code to reproduce our experiments on HELM MMLU (v1.0.0).  
The code reads HELM run folders, builds rankings for each MMLU subject, and then tests for:

- **Single-peakedness**
- **Group separability**
- **Distance-restrictedness**


### Data: HELM MMLU raw results (v1.0.0)

Please download HELM raw data from the public crfm-helm-public bucket in the Google Cloud Storage (GCS) by completing the following two steps:

	1.	Install the Google Cloud CLI (gcloud) by following Google’s official instructions: 
https://docs.cloud.google.com/sdk/docs/install-sdk

	2.	Download HELM MMLU raw results by following the official HELM instructions:
https://crfm-helm.readthedocs.io/en/latest/downloading_raw_results/

After downloading the code expects the following structure:

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
