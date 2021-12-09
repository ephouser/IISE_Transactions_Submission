# IISE_Transactions_Submission
Data and Code for the IISE Transactions Journal Submission

This README file will detail the order in which code should be executed. This file will be updated regularly. 
(Last Update 12/8/21)


Pre-Phase Preparation
- Before our methodology can be executed, raw data logs output by the Electron Beam Melting machine must be cleaned. These raw data logs had to be uploaded as compressed zip files due to the size of each .txt file. Each file represents the logs from the production of a single build. The code to be used for the Pre-Phase Preparation is the "FinalCode_BuildingCovaraiteDataset.R" file. In this file, the data was cleaned and key process variable were identified. Through exploration of these process variables, a covariate dataset is then generated (Modeling_Dataset_2.csv) to prepare for the methodology. (NOTE: This code will be further cleaned soon)


Phase 1
- The Phase 1 code was replicated into four R files. Each file has a uniquely defined Beta value (Phase1_Beta1.R, Phase2_Beta2.R, Phase3_Beta3.R, ND Phase4_Beta4.R). The code was separated into four files due to lengthy run times. The Phase 1 code utilizes the Modeling_Dataset_2.csv dataset created from Pre-Phase Preparation. Each Phase 1 file produces two key pieces of output: a dataset of features selected by the LASSO-I methodology and by the SOFS-I methodology. The resulting .RData files are as follows:
  - P1B1_LASSOI_Feats.RData
  - P1B1_SOFSI_Feats.RData
  - P1B2_LASSOI_Feats.RData
  - P1B2_SOFSI_Feats.RData
  - P1B3_LASSOI_Feats.RData
  - P1B3_SOFSI_Feats.RData
  - P1B4_LASSOI_Feats.RData
  - P1B4_SOFSI_Feats.RData
  
  
Phase 2
- The Phase 2 code (Phase2_SubmissionCode.R) utilizes the 8 .RData files produces from Phase 1. When executed, this code produces the data and plots used to create the tables and figures for our research paper. 
