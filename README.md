# SmartChem_v3

## Summary

SmartChem processing code v3.

## Highly Qualified Personnel

- Lana Vuleta, GIWS, lana.vuleta@usask.ca

## Instructions

 1) Download R and RStudio. Follow the instructions at https://rstudio-education.github.io/hopr/starting.html.
 2) From the SmartChem Github page, go to <> Code -> Download ZIP. Unzip the folder.
 3) COPY the SmartChem files to process into the "data/input" folder. This folder will be emptied upon completion of the script. If this is your first time using the tool, the input folder contains an example file for you to test.
 4) Open RStudio.
 5) Go to File -> Open Project, and go to the downloaded SmartChem folder. Select SmartChem_v3.Rproj.
 6) Go to File -> Open File, and select sc_run_me.R.
 7) Select all code in the script (for example, by using Ctrl+a).
 8) Run the code, either by clicking "Run" (button in the top right corner) or by pressing Ctrl+Enter.
 9) Check the data/output file for results. Outputs are named as: runDate_runTime_numberOfProcessedFiles.

## Repo content information

### data/required

Contains the file used to identify and store info about the different tests run by SmartChem, including their associated MDLs.

### data/input

Place the raw SC files that you wish to process into this folder. Files will be erased from the folder upon completion of the script.

### data/output/plots

Folder created upon script completion. Contains plots of the data.

### data/output/processed_data

Folder created upon script completion. Contains processed csv files.

### scripts

Scripts for processing raw data into standard format.

