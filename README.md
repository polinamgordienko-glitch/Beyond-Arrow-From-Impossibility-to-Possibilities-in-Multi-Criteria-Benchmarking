Beyond Arrow: From Impossibility to Possibilities in Multi-Criteria Benchmarking

HELM–MMLU (v1.0.0)

This repository contains the code to reproduce our experiments on HELM MMLU (v1.0.0).  
The code reads HELM run folders, builds rankings for each MMLU subject, and then tests for:

- **Single-peakedness**
- **Group separability**
- **Distance-restrictedness**


Data: HELM MMLU raw results (v1.0.0)

We do not include HELM data in this repository. Instead, please download it from the public crfm-helm-public bucket in the Google Cloud Storage (GCS).
	1.	Install the Google Cloud CLI (gcloud) by following Google’s official instructions:
https://docs.cloud.google.com/sdk/docs/install-sdk
	2.	Download HELM raw results by following the official HELM instructions:
https://crfm-helm.readthedocs.io/en/latest/downloading_raw_results/
