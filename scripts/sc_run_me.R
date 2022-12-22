#############################
## SmartChem processing code

## Authors: LV, CJW, JWD

## 2022/10/24
#############################

# How to: 
# 1) COPY the files you'd like to process in the "data/input" folder.
#    This folder will be emptied by the script.
# 2) Click "Run" (button in the top right corner).
# 3) Check the data/output file for results.

source("scripts/sc_functions.R")
source("scripts/sc_helper_functions.R")

suppressWarnings(suppressMessages(load_packages()))

process_sc()
