process_sc <- function() {
  
  # Select input files ---------------------------------------------------------
  filenames <- list.files("data/input/", full.names = F) %>%
    gsub(pattern = "Xls", replacement = "xls")
  
  if (length(filenames) == 0) {
    stop("The data/input folder is empty. Try again.")
  }
  
  # Process all data -----------------------------------------------------------
  print("Preparing the data...")
  file_info <- map(filenames, prepare_file_info)
  
  print("Performing quality control check...")
  data_qc <- map(file_info, process_results)
  
  print("Preparing the plots...")
  plots <- map2(file_info, data_qc, create_plots)
  
  # Prepare data for writing, write data ---------------------------------------
  print("Writing the data to file (check the output folder!)...")
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M_")
  
  # Save all of the plots in one pdf file
  pdf_destination <- file.path(here(), "data/output/plots",
                               paste0(timestamp, length(filenames), ".pdf"))
  
  pdf(file = pdf_destination)
  
  for (i in 1:length(plots)) {
    plot(plots[[i]])
  }
  
  dev.off() 
  
  # Combine all of the QCd data for easier viewing
  data_qc <- bind_rows(data_qc)
  
  # To make more meaningful, readable flags
  flag_replacement <- c("LL"  = "Linearly Low",
                        "LH"  = "Linearly High",
                        "EPL" = "End-Point Limit",
                        "INV" = "Inverse Linearly Low",
                        "><"  = "Out of Calibration Curve",
                        "SS"  = "Short on Sample",
                        "SR"  = "Short on Reagent",
                        "H"   = "High",
                        ","   = ", ")
  
  data_qc[c("Flags Dup 1", "Flags Dup 2")] <- 
    apply(data_qc[c("Flags Dup 1", "Flags Dup 2")],
          2,
          function(x) str_replace_all(x, flag_replacement))
  
  write.csv(data_qc, file.path(here(), "data/output/processed_data/",
                               paste0(timestamp, length(filenames), ".csv")),
            row.names = FALSE)
  
  # So that the "data/input" folder is empty for the next runs of the script
  print("Removing files in the input folder...")
  options(warn = -1)
  file.remove(list.files("data/input/", full.names = T))
  options(warn = 0)
  
  print("Complete.")
  
}


prepare_file_info <- function(filename) {
  
  path <- file.path("data/input", filename)
  
  parameters_by_test <- read_xlsx("data/required/parameters_by_test.xlsx")
  
  sc_method <- parameters_by_test$test[str_which(filename, 
                                                 parameters_by_test$test)]
  
  file_info <- c(path, sc_method)
  names(file_info) <- c("path", "sc_method")
  
  return(file_info)
  
}

process_results <- function(file_info) {
  
  # Read in worksheets ---------------------------------------------------------

  result <- file_info[["path"]] %>%
    read_excel(sheet = paste0("_", file_info[["sc_method"]], "_Result")) %T>%
    # Settings these cols to numeric sends a warning of "NAs introduced by
    # coersion". suppressWarnings() cannot be used in pipes, so options() is
    # used instead.
    {options(warn = -1)} %>% 
    mutate_at(c("Concentration", "Abs", "Dil Ratio", "Position"), as.numeric) %T>%
    {options(warn = 0)} %>%
    mutate(Test = file_info[["sc_method"]],
           # To later be able to append extra Flags (cannot append to NA!)
           Flags = replace_na(Flags, ""),
           # To be able to paste RunDate and RunTime to get a DateTime
           RunTime = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           # TO have simpler, fewer columns
           DateTime = as.POSIXct(paste(RunDate, RunTime)))
  
  # Create output folders ------------------------------------------------------
  
  # In case if these directories are not yet created, create them
  dir.create(path = "data/output", showWarnings = FALSE)
  dir.create(path = "data/output/plots", showWarnings = FALSE)
  dir.create(path = "data/output/processed_data", showWarnings = FALSE)
  
  # Result processing ----------------------------------------------------------

  # mdl = Method detection limit
  mdl <- result %>% 
    filter(SampleID == "STND", !is.na(`Concentration`)) %>%
    with(sd(Concentration)*qt(c(.95), df = (length(Concentration) - 1)))

  # Must reorganize data to have one row for each sample, where samples are
  # grouped by their Position.
  result_qc <- result %>% 
    filter(SampleType == 0) %>%
    group_by(Position) %>%
    # Only the last run of each sample is desired (i.e. if the SC dilutes the 
    # sample and reruns it, only keep the result of the last dilution).
    # Occasionally, when the SC user knows that samples will need to be diluted,
    # samples are run with a Dil Ratio > 1 from the start, hence why we compare
    # the max and min Dil Ratios. If max and min Dil Ratio = 5, no rerun occurred,
    # even though Dil Ratio != 1
    mutate(Reruns = case_when(max(`Dil Ratio`) != min(`Dil Ratio`) & 
                                `Dil Ratio` == max(`Dil Ratio`)       ~ "Rerun",
                              TRUE ~ "")) %>%
    filter(`Dil Ratio` == max(`Dil Ratio`)) %>%
    # In summarise(), we often must select either the 1st or 2nd duplicate rows.
    # This makes the selection cleaner
    mutate(Dup1 = !str_detect(SampleID, "Dup"),
           Dup2 =  str_detect(SampleID, "Dup")) %>%
    summarise(Test          = Test[Dup1],
              DateTime      = DateTime[Dup1], # Dup 1 val chosen as representative
              `Abs Dup 1`   = Abs[Dup1],
              `Abs Dup 2`   = Abs[Dup2],
              `Dil Ratio`   = `Dil Ratio`[Dup1], # Dup 1 dil chosen as representative
              `Manual Dil`  = `Manual Dil`[Dup1], # Dup 1 dil chosen as representative
              
              `Flags Dup 1` = flags_dup(Concentration, Dup1, Flags, mdl),
              `Flags Dup 2` = flags_dup(Concentration, Dup2, Flags, mdl),
              
              # Following values yield important information about the runs
              `Concentration (mean)`        = round(mean(Concentration), 4),
              `Concentration (diff)`        = diff(Concentration),
              `Concentration (perc diff)`   = round(100*`Concentration (diff)`/(sum(Concentration)/2), 2),
              `Concentration (sd)`          = round(gc_sd(Concentration, `Concentration (mean)`), 4),
              `Concentration (relative sd)` = round(`Concentration (sd)`/`Concentration (mean)`*100, 4),
              `Concentration (mean)`        = check_means(`Concentration (mean)`, `Flags Dup 1`, `Flags Dup 2`),
              
              `Flags SC` = flags_sc(`Concentration (perc diff)`, Reruns, Concentration, Dup1, Dup2, mdl),
              
              # Dup2 sample ID contains "Dup" - Dup1 sample ID is therefore desired
              SampleID = SampleID[Dup1]) %>%
    
    select(SampleID, Test, `Concentration (mean)`:`Concentration (relative sd)`,
           `Abs Dup 1`:`Flags Dup 2`, `Flags SC`, Position, DateTime) %>%
    mutate(MDL = mdl,
           `Source File` = tail(unlist(str_split(file_info[["path"]], "/")), 1)) %>%
    arrange(Position)

  return(result_qc)
 
} 

create_plots <- function(file_info, result_qc) {
  
  path <- file_info[["path"]]
  sc_method <- file_info[["sc_method"]]
  
  controls <- path %>%
    read_excel(sheet = paste0("_", sc_method, "_Controls")) %>%
    mutate_at(c("Concentration", "Nominal", "Abs", "Recovery_PRD"), as.numeric) %>%
    mutate(RunTime = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           DateTime = as.POSIXct(paste(RunDate, RunTime)))
  
  calibrants <- path %>%
    read_excel(sheet = paste0("_", sc_method, "_Calibrants")) %>%
    mutate_at(c("Concentration", "Abs", "Dil Ratio", "SampleType"), as.numeric) %>%
    mutate(RunTime = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           DateTime = as.POSIXct(paste(RunDate, RunTime)))
  
  rbl <- path %>%
    read_excel(sheet = paste0("_", sc_method, "_Rbl")) %>%
    mutate_at(c("Concentration", "Abs"), as.numeric) %>%
    mutate(RunTime = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           DateTime = as.POSIXct(paste(RunDate, RunTime)))
  
  # Calibration curve ----------------------------------------------------------
  
  # Mean RBL is used as the intercept of the calibration curve
  rbl_mean <- mean(rbl$Abs)
  
  # Calibration is only done for the first plan of the day. Later plans will not
  # have calibration data. If calibrants were ran in current run, create 
  # calibration curve.
  if (nrow(calibrants) > 0) {
    
    calibration_lm <- lm(calibrants$Abs ~ calibrants$Concentration)
    
    plot_calibration_curve <- calibrants %>% 
      ggplot(aes(DateTime, Abs)) +
      geom_point(na.rm = TRUE) +
      geom_smooth(formula = y ~ x, method = "lm", na.rm = TRUE) + 
      ggtitle(label = paste0("Calibrant Report: R^2 = ", 
                             format(summary(calibration_lm)$r.squared, 
                                    digits = 3))) +
      geom_hline(yintercept = rbl_mean, linetype = "longdash")
    
  }
  
  # Duplicate summaries --------------------------------------------------------

  plot_perc_diff <- result_qc %>% 
    ggplot(aes(DateTime, `Concentration (perc diff)`)) + 
    geom_point(na.rm = TRUE) + 
    ggtitle(paste("Percent Difference of Duplicates")) + 
    geom_hline(yintercept = 10, linetype = "dashed") + 
    geom_hline(yintercept = -10, linetype = "dashed")  + 
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Percent Difference") +
    xlab("Run Time")
  
  plot_diff <- result_qc %>% 
    ggplot(aes(DateTime, `Concentration (diff)`)) + 
    geom_point(na.rm = TRUE) +
    ggtitle(paste("Difference of Duplicates")) + 
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Difference") +
    xlab("Run Time")
  
  # Quality controls -----------------------------------------------------------
  # Plot controls over duration of run
  plot_controls <- controls %>% 
    ggplot(aes(DateTime, Concentration, color = SampleID, fill = SampleID)) +
    geom_point(na.rm = TRUE) +
    suppressWarnings(geom_smooth(formula = y ~ x, method = "lm", na.rm = TRUE)) +
    ggtitle(label = paste("Controls")) + 
    theme(axis.text.x = element_text(angle = 45)) +
    xlab("Runtime") +
    ylab("Concentration") +
    geom_hline(yintercept = unique(controls$Nominal, linetype = 2))
  
  
  # Calculate coil efficiency over duration of run (NOx only) and plot results
  if (sc_method == "BNO3") {  
    
    coil_effic <- controls %>% 
      filter(grepl("CCN", SampleID)) %>% 
      mutate(cd_coil_effic = (Abs/Nominal)/(lag(Abs)/lag(Nominal))) %>% 
      filter(SampleID == "CCN3") 
    
    plot_coil_effic <- coil_effic %>%   
      ggplot(aes(x = RunTime, y = cd_coil_effic)) + 
      geom_point(na.rm = TRUE) + 
      geom_line(na.rm = TRUE) +
      scale_y_continuous(limits = c(.75, 1)) +
      ggtitle(label = "Cd Coil Efficiency")
    
  }
  
  # Creating final, combined plot ----------------------------------------------
  
  # plot_controls in particular can throw warnings depending on if there are too
  # few points for a proper linear model. These warnings are not desired
  plot_diffs <- suppressWarnings(grid.arrange(plot_diff, 
                                              plot_perc_diff, 
                                              plot_controls, 
                                              ncol = 1))
  
  if (exists("plot_calibration_curve")) {
    if (exists("plot_coil_effic")) {
      plot_extras <- grid.arrange(plot_calibration_curve, plot_coil_effic, 
                                  ncol = 1)
    } else {
      plot_extras <- plot_calibration_curve
    }
  } else {
    if (exists("plot_coil_effic")) {
      plot_extras <- plot_coil_effic
    } else {
      plot_extras <- NA
    }
  }
  
  # Filenames begin with 8 digits indicating the run date
  run_date <- as.Date(substr(tail(unlist(str_split(path, "/")), 1), 1, 8), 
                      format = "%Y%m%d")
  
  title <- paste(run_date,
                 sc_method,
                 # Get only the run name - do not include the date, method, 
                 # or file extension
                 str_remove(str_remove(tail(unlist(str_split(path, "/")), 1), 
                                       "\\d+\\s-\\s[A-Za-z0-9]+\\s-\\s"), 
                            "\\.xls"),
                 sep = " ")
  
  if (all(is.na(plot_extras))) {
    plot_all <- grid.arrange(plot_diffs, top = title)
  } else {
    plot_all <- grid.arrange(plot_diffs, plot_extras, ncol = 2, top = title)
  }
  
  return(plot_all)
  
}

