load_packages <- function() {
  
  # Determine which packages are missing
  package_list <- c("dplyr", "tidyr", "readxl", "ggplot2", "lubridate", 
                    "stringr", "gridExtra", "purrr", "here")
  packages_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(packages_new)) install.packages(packages_new)
  
  # Load each desired package
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(ggplot2)
  library(lubridate)
  library(stringr)
  library(gridExtra)
  library(purrr)
  library(here)
  
}

check_tests <- function(tests) {
  counts <- tests %>% 
    group_by(test) %>% 
    summarise(n = n())
  
  if (any(counts$n != 1)) {
    stop(paste("Issues with tests.xlsx. Each test should only occur once in",
               "the file. Remove repeats."),
         call. = FALSE)
  }
}

find_match_row <- function(filename, tests) {
  
  match_row <- str_which(filename, tests$test)
  
  if (is_empty(match_row)) {
    message(paste(sprintf("%s: There is an issue with this file. File is skipped.\n",
                          filename),
                  "\tThis file might not be a SC output file.\n",
                  "\tAlternatively, the test being run might not be listed in",
                  "data/required/tests.xlsx. Check tests.xlsx. Note that SC",
                  "file detection is case-sensitive.\n")) 
    return(NA)
  } else {
    return(match_row)
  }
  
}

get_mdl <- function(file_info, match_row, tests) {
  
  # mdl = Method detection limit
  mdl <- tests$mdl_lab[match_row]
  
  if (is.na(mdl)) {
    
    # There is no lab-calculated MDL. Use script-calculated MDL instead
    mdl <- file_info[["path"]] %>%
      read_excel(sheet = 2) %T>%
      # There are some files with "***" as concentration. as.numeric() to set 
      # those to NA
      {options(warn = -1)} %>% 
      mutate(Concentration = as.numeric(Concentration)) %T>%
      {options(warn = 0)} %>%
      filter(SampleID == "STND", !is.na(`Concentration`)) %>%
      with(sd(Concentration)*qt(c(.95), df = (length(Concentration) - 1)))
    
  } 
  return(mdl)
}

get_mdl_date <- function(file_info, match_row, tests) {
  mdl_date <- tests$mdl_lab_date[match_row]
}

check_mdl_date <- function(mdl_date) {
  if (!is.na(mdl_date)) {
    if (abs(time_length(difftime(mdl_date, Sys.Date()), "years")) > 1) {
      return(TRUE)
    }
  }
}

check_for_dups <- function(data) {
  # Note that when a sample's runs look like "Sample", "Sample_Dup", this
  # script refers to the "Sample" run as dup1, the "Sample_Dup" run as dup2
  
  num_dup1_runs <- sum(!str_count(data$SampleID, "_Dup"))
  
  # If there's a different number of dup1 vs dup2 rows, some runs were not 
  # completed and do not have a dup2. Since the script compares 2 dups, a 
  # dummy dup must be created
  if (sum(str_count(data$SampleID, "_Dup")) != num_dup1_runs) {
    
    num_rows <- nrow(data)
    
    # Sometimes, a sample is run multiple times with the same ID, dilutions,
    # and position. In this case, no other column is safe to group by. Instead,
    # we loop through rows to spot when a direct consecutive dup2 is missing
    for (i in 1:num_dup1_runs) {
      
      # No "_Dup" in the next row indicates that dup2 was not run
      if (2*i-1 == num_rows | !str_detect(data$SampleID[2*i], "_Dup")) {
        
        dup_row <- data[i, ] %>%
          mutate(SampleID = paste0(SampleID, "_Dup"),
                 Concentration = NA,
                 Abs           = NA,
                 Flags         = NA,
                 Recovery_PRD  = NA)
        
        data <- insert_row(data, dup_row, 2*i)
        
        num_rows <- num_rows + 1
      }
    }
  }
  
  # If we have multiple samples, we indicate that there exist other repeats
  # with the same dilution and position with the RepeatNum column
  if (num_dup1_runs != 1) {
    data$RepeatNum <- sort(rep(1:num_dup1_runs, 2))
  }
  
  return(data)
  
}

# Function sourced from https://www.geeksforgeeks.org/add-new-row-at-specific-index-position-to-dataframe-in-r/
insert_row <- function(data, new_row, new_index) {
  data_new <- rbind(data[1:new_index-1, ],            
                    new_row,                
                    data[- (1:new_index-1), ])
  return(data_new)
}

flags_dup <- function(conc, flags_og, mdl) {
  
  flags <- c()
  
  flags_og <- if (is.na(flags_og)) flags_og else if (flags_og == "") NA_character_ else flags_og
  
  if (is.na(conc)) {
    # [] should only be NA if a dup2 was missing and a dummy dup was inserted by
    # the script
    flags <- c(flags, "Run was not completed")
  } else {
    if (conc < 0) {
      flags <- c(flags, sprintf("< 0 (actual value: %.3f)", conc))
    }
    
    if (conc < mdl) {
      flags <- c(flags, "< MDL")
    }
  }
  
  flags <- c(flags, flags_og)
  flags <- flags[!is.na(flags)]
  flags <- paste(flags, collapse = ",")
  
  return(flags)
  
}

gc_sd <- function(conc_col, conc_mean) {
  sd <- sqrt((( (conc_mean - conc_col[1])^2 + 
                (conc_mean - conc_col[2])^2  ))/2)
}

check_means <- function(mean, flag1, flag2) {
  
  # If either concentration is negative, Colin has asked that the mean value
  # be set to -111, in combination with the use of a flag (in Flags Dup 1/2)
  if (str_detect(flag1, "< 0") | str_detect(flag2, "< 0")) {
    mean <- -111
  } else {
    mean <- mean
  }
  
  return(mean)
  
}

flags_sc <- function(conc_pd, flags_og, conc_dup1, conc_dup2, mdl, repeat_num) {
  flags <- c()
  
  # Setting empty flags to NA makes concatenating the flags vector into a 
  # string simpler
  flags_og <- if(flags_og == "") NA_character_ else {flags_og}
  
  if (!is.na(conc_pd)) {
    if (abs(conc_pd) > 10 & conc_dup1 >= mdl & conc_dup2 >= mdl) {
      flags <- c(flags, "Perc difference > 10 & both conc above MDL")
    }
    
    if (abs(conc_pd) > 10 & (conc_dup1 < mdl  | conc_dup2 < mdl )) {
      flags <- c(flags, "Perc difference > 10 & one or both conc below MDL")
    }
    
    if (abs(conc_pd) <= 10 & (conc_dup1 < mdl  & conc_dup2 < mdl )) {
      flags <- c(flags, "Both conc below MDL")
    }
  }
  
  flags <- c(flags, flags_og)
  flags <- flags[!is.na(flags)]
  flags <- paste(flags, collapse = ", ")
  
  if (!is.na(repeat_num)) {
    flags <- paste(flags, "Multi-run sample", sep = ", ")
  }
  
  return(flags)
  
}

