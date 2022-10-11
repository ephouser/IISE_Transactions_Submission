# IISE_Transactions_Submission
Data and Code for the IISE Transactions Journal Submission

This README file will detail the order in which code should be executed. This file will be updated regularly. 
(Last Update 10/10/22)


Pre-Phase Preparation (NOTE: This code will be further cleaned and updated soon)
- Before our methodology can be executed, raw data logs output by the Electron Beam Melting machine must be cleaned. These raw data logs had to be uploaded as compressed zip files due to the size of each .txt file. Each file represents the logs from the production of a single build. The code to be used for the Pre-Phase Preparation is the "FinalCode_BuildingCovaraiteDataset.R" file. In this file, the data was cleaned and key process variable were identified. Through exploration of these process variables, a covariate dataset is then generated (Modeling_Dataset_2.csv) to prepare for the methodology. 


Phase 1 and Phase 2 (Experiment)
- The overall experiment code was integrated and updated into "Experiment, figure and plot.ipynb" file. Related figures and tables can be replicated through ordered scripts. The overall running time is estimated to within one-hour, but might be different depending on the hardware performance.
