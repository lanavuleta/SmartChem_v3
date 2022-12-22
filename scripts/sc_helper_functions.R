load_packages <- function() {
  
  # Determine which packages are missing
  package_list <- c("dplyr", "tidyr", "readxl", "ggplot2", "lubridate", 
                    "stringr", "gridExtra", "purrr", "here")
  packages_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(packages_new)) install.packages(packages_new)
  
  # Load each desired package
  #suppressWarnings(suppressMessages(library(dplyr)))
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(ggplot2)
  #suppressMessages(library(lubridate))
  library(lubridate)
  library(stringr)
  #suppressMessages(library(gridExtra))
  library(gridExtra)
  library(purrr)
  library(here)
  
}

gc_sd <- function(conc_col, conc_mean_col) {
  
  if (length(conc_col) != 2) {
    
    warning(paste("Beware: This specific run idenfitied by its Dil Ratio,",
                  "Manual Dil, and Position does not have 2 duplicates. 😲"))
    sd <- NA
  
  } else {
  
    sd <- sqrt((( (conc_mean_col - conc_col[1])^2 + 
                  (conc_mean_col - conc_col[2])^2  ))/2)
    
  }
}

flags_dup <- function(conc_col, dup_col, flags_col, mdl) {
  
  flags <- c()
  
  flags_og <- flags_col[dup_col]
  # Setting empty flags to NULL makes concatenating the flags vector into a 
  # string simpler
  flags_og <- if(flags_og == "") NA_character_ else {flags_og}
  
  conc <- conc_col[dup_col]
  
  if (is.na(conc)) {
    flags <- c(flags, "Conc does not exist")
  } else {
    # Flag runs with negative concentrations
    if (conc < 0) {
      flags <- c(flags, sprintf("< 0 (og value: %.3f)", conc))
    }
    
    # Flag runs for which concentration is below the MDL
    if (conc < mdl) {
      flags <- c(flags, "< MDL")
    }
  }
  
  flags <- c(flags, flags_og)
  flags <- flags[!is.na(flags)]
  flags <- paste(flags, collapse = ",")
  
  return(flags)

}

flags_sc <- function(pd_col, flags_col, conc_col, dup1_col, dup2_col, mdl) {
  flags <- c()
  
  flags_og <- flags_col[dup1_col] # Dup 1 chosen as representative (Dup 2 is same)
  # Setting empty flags to NULL makes concatenating the flags vector into a 
  # string simpler
  flags_og <- if(flags_og == "") NA_character_ else {flags_og}
  
  conc_pd <- pd_col
  
  if (is.na(conc_pd)) {
    flags <- c(flags, "One or more conc does not exist")
  } else {
    conc_d1 <- conc_col[dup1_col]
    conc_d2 <- conc_col[dup2_col]
    
    if (abs(conc_pd) > 10 & conc_d1 >= mdl & conc_d2 >= mdl) {
      flags <- c(flags, "Perc difference > 10 - Both conc above MDL")
    }
    
    if (abs(conc_pd) > 10 & (conc_d1 < mdl  | conc_d2 < mdl )) {
      flags <- c(flags, "Perc difference > 10 - One or both conc below MDL")
    }
  }
  
  flags <- c(flags, flags_og)
  flags <- flags[!is.na(flags)]
  flags <- paste(flags, collapse = ", ")
  
  return(flags)
  
}

check_means <- function(mean_col, flag1_col, flag2_col) {

  # If either concentration is negative, Colin has asked that the mean value
  # be set to -111, in combination with the use of a flag (in Flags Dup 1/2)
  if (str_detect(flag1_col, "< 0") | str_detect(flag2_col, "< 0")) {
    mean_col <- -111
  } else {
    mean_col <- mean_col
  }
  
  return(mean_col)
  
}
