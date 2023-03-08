# IISE_Transactions_Submission
Data and Code for the IISE Transactions Journal Submission

This README file will detail the order in which code should be executed. This file will be updated regularly. The exploratory code is currently being cleaned. However, it was not the main focus of the research.
(Last Update 1/30/22)


Pre-Phase Preparation (Exploratory Phase)
- NOTE: This code is used to explore the data and generate the modeling dataset. The code for this phase is still being cleaned and prepped, but running this code is not necessary to begin the modeling. The output of the Pre-Phase Prepration is the file "Modeling_Dataset_2.csv" which has been provided.
- Before our methodology can be executed, raw data logs output by the Electron Beam Melting machine must be cleaned. These raw data logs had to be uploaded as compressed zip files due to the size of each .txt file. Each file represents the logs from the production of a single build. The code to be used for the Pre-Phase Preparation is the "FinalCode_BuildingCovaraiteDataset.R" file. In this file, the data was cleaned and key process variable were identified. Through exploration of these process variables, a covariate dataset is then generated (Modeling_Dataset_2.csv) to prepare for the methodology. 


Phase 1 and Phase 2 (Experiment)
- The overall experiment code was integrated and updated into "SOEN_Experimentation.ipynb" file. Related figures and tables can be replicated through ordered scripts. The overall running time is estimated to within one-hour, but might be different depending on the hardware performance.

Required Python Packages:
In order to run the code, the following python packages should be installed on your computer.
- sklearn
- seaborn
- pygad
- tqdm.notebook
- lightgbm 
- imblearn
