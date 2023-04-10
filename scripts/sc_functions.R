process_sc <- function() {
  
  # summarise() shows unnecessary message
  options(dplyr.summarise.inform = FALSE)
  
  # Select input files ---------------------------------------------------------
  filenames <- list.files("data/input/", full.names = F) %>%
    gsub(pattern = "Xls", replacement = "xls")
  
  if (length(filenames) == 0) {
    stop("The data/input folder is empty. Try again.",
         call. = FALSE)
  }
  
  # Process all data -----------------------------------------------------------
  print("Preparing the data...")
  # compact() used to remove info assigned NULL because the input file was
  # skipped
  file_info <- compact(map(filenames, prepare_file_info))
  
  if (length(file_info) == 0) {
    stop("None of the input files are valid. Cancelling.",
         call. = FALSE)
  }
  
  print("Performing quality control check...")
  data_qc <- map(file_info, process_results)
  
  print("Preparing the plots...")
  plots <- map2(file_info, data_qc, create_plots)
  
  # Prepare data for writing and write data ------------------------------------
  print("Writing the data to file (check the output folder!)...")
  
  create_output_folders()
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M_")
  
  # Save all of the plots in one pdf file
  pdf_destination <- file.path(here(), "data/output/plots",
                               paste0(timestamp, length(file_info), ".pdf"))
  
  pdf(file = pdf_destination)
  
  for (i in 1:length(plots)) {
    plot(plots[[i]])
  }
  
  dev.off() 
  
  data_qc <- bind_rows(data_qc)
  
  # To make more meaningful, readable flags
  flag_replacement <- c("LL"  = "Linearity Low",
                        "LH"  = "Linearity High",
                        "EPL" = "End-Point Limit",
                        "INV" = "Inversion",
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
                               paste0(timestamp, length(file_info), ".csv")),
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
  
  parameters_by_test <- read_xlsx("data/required/tests.xlsx")
  
  sc_method <- parameters_by_test$test[str_which(filename, 
                                                 parameters_by_test$test)]
  
  if (is_empty(sc_method)) {
    message(paste(sprintf("%s: There is an issue with this file. File is skipped.\n",
                          filename),
            "\tThis file might not be a SC output file.\n",
            "\tAlternatively, the test being run might not be listed in",
            "data/required/tests.xlsx. Check tests.xlsx\n")) 
    return(NULL)
  } else {
    file_info <- c(path, sc_method)
    names(file_info) <- c("path", "sc_method")
    return(file_info)
  }
}

process_results <- function(file_info) {
  
  # Read in worksheets ---------------------------------------------------------

  result <- file_info[["path"]] %>%
    read_excel(sheet = 1) %>%
    filter(SampleType == 0) %T>%
    # Setting these cols to numeric sends a warning of "NAs introduced by
    # coersion". suppressWarnings() cannot be used in pipes, so options() is
    # used instead.
    {options(warn = -1)} %>% 
    mutate_at(c("Concentration", "Abs", "Dil Ratio", "Position"), as.numeric) %T>%
    {options(warn = 0)} %>%
    mutate(Test     = file_info[["sc_method"]],
           Flags    = as.character(Flags),
           # To later be able to append extra Flags (cannot append to NA)
           Flags    = replace_na(Flags, ""),
           # To be able to paste RunDate and RunTime to get a DateTime
           RunTime  = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           # TO have simpler, fewer columns
           DateTime = as.POSIXct(paste(RunDate, RunTime))) 
  
  # Result processing ----------------------------------------------------------

  # mdl = Method detection limit
  mdl <- file_info[["path"]] %>%
    read_excel(sheet = 2) %T>%
    # There are some files with "***" as concentration. as.numeric() to set 
    # those to NA
    {options(warn = -1)} %>% 
    mutate(Concentration = as.numeric(Concentration)) %T>%
    {options(warn = 0)} %>%
    filter(SampleID == "STND", !is.na(`Concentration`)) %>%
    with(sd(Concentration)*qt(c(.95), df = (length(Concentration) - 1)))
  
  # Want to see the max of the calibration curve for easier interpretation of 
  # "High" flags
  method_runs <- file_info[["path"]] %>%
    read_excel(sheet = 5) %>%
    select(starts_with("StdRef")) %>% 
    pivot_longer(cols = starts_with("StdRef"), names_to = "run", values_to = "ref")

  # Must reorganize data to have one row for each sample
  result_qc <- result %>% 
    # There are some SampleIDs with hidden tabs - when displayed in Excel, name 
    # appears normal, but when name is copied elsewhere, a tab shows up
    mutate(SampleID  = str_remove(SampleID, "\t"),
           # Some samples are run numerous times with the same dilutions and 
           # positions. RepeatNum stores which repeat the run is, if applicable
           RepeatNum = NA) %>%
    # Must account for rogue spacing
    group_by(str_remove(SampleID, "\\s*_Dup$"), Position) %>%
    # Only the last run of each sample is desired (i.e. if the SC dilutes the 
    # sample and reruns it, only keep the result of the last dilution).
    # Occasionally, when the SC user knows that samples will need to be diluted,
    # samples are run with a Dil Ratio > 1 from the start, hence why we compare
    # the max and min Dil Ratios. If max and min Dil Ratio = 5, no rerun occurred,
    # even though Dil Ratio != 1
    mutate(Reruns = case_when(max(`Dil Ratio`) != min(`Dil Ratio`) & 
                              `Dil Ratio` == max(`Dil Ratio`)       ~ "Rerun dilution",
                              TRUE ~ "")) %>%
    filter(`Dil Ratio` == max(`Dil Ratio`)) %>%
    group_split() %>%
    # Sometimes a sample is run without a Dup (for ex, if short on regant). 
    # Script works on the premise of 2 duplicates, so we create dummy dups
    map_df(check_for_dups) %>%
    mutate(SampleID = str_remove(SampleID, "\\s*_Dup$")) %>%
    group_by(SampleID, Position, RepeatNum) %>%
    summarise(Test          = Test[1],
              DateTime      = DateTime[1], # Dup 1 val chosen as representative
              `Abs Dup 1`   = Abs[1],
              `Abs Dup 2`   = Abs[2],
              # Dup 1 dil chosen as representative (val is the same between Dup 1 and Dup 2)
              `Dil Ratio`   = `Dil Ratio`[1],
              `Manual Dil`  = `Manual Dil`[1],
              
              `Flags Dup 1` = flags_dup(Concentration[1], Flags[1], mdl),
              `Flags Dup 2` = flags_dup(Concentration[2], Flags[2], mdl),
              `High Calibration Standard` = max(method_runs$ref),
              
              # Following values yield important information about the runs
              `Concentration (mean)`        = round(mean(Concentration, na.rm = TRUE), 4),
              `Concentration (diff)`        = diff(Concentration),
              `Concentration (perc diff)`   = round(100*`Concentration (diff)`/(sum(Concentration)/2), 2),
              `Concentration (sd)`          = round(gc_sd(Concentration, `Concentration (mean)`), 4),
              `Concentration (relative sd)` = round(`Concentration (sd)`/`Concentration (mean)`*100, 4),
              `Concentration (mean)`        = check_means(`Concentration (mean)`, `Flags Dup 1`, `Flags Dup 2`),
              
              `Flags` = flags_sc(`Concentration (perc diff)`,
                                 # Dup 1 val chosen as representative
                                 Reruns[1],
                                 Concentration[1], Concentration[2], 
                                 mdl,
                                 RepeatNum[1])) %>%
    
    select(SampleID, Test, `Concentration (mean)`:`Concentration (relative sd)`,
           `Abs Dup 1`:`High Calibration Standard`, `Flags`, Position, DateTime) %>%
    mutate(MDL = mdl,
           `Source File` = tail(unlist(str_split(file_info[["path"]], "/")), 1)) %>%
    # So that order is the same as in original file
    arrange(DateTime)

  return(result_qc)
 
} 

create_plots <- function(file_info, result_qc) {
  
  path <- file_info[["path"]]
  sc_method <- file_info[["sc_method"]]
  
  # Hide messages that warn us when non-numeric values are converted to NA when
  # as.numeric is applied
  options(warn = -1)
  
  controls <- path %>%
    read_excel(sheet = 2) %>%
    mutate_at(c("Concentration", "Nominal", "Abs", "Recovery_PRD"), as.numeric) %>%
    mutate(RunTime = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           DateTime = as.POSIXct(paste(RunDate, RunTime)))
  
  calibrants <- path %>%
    read_excel(sheet = 3) %>%
    mutate_at(c("Concentration", "Abs", "Dil Ratio", "SampleType"), as.numeric) %>%
    mutate(RunTime = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           DateTime = as.POSIXct(paste(RunDate, RunTime)))
  
  rbl <- path %>%
    read_excel(sheet = 4) %>%
    mutate_at(c("Concentration", "Abs"), as.numeric) %>%
    mutate(RunTime = format(as.POSIXct(RunTime, format = "%I:%M:%S %p"), "%H:%M:%S"),
           DateTime = as.POSIXct(paste(RunDate, RunTime)))
  
  options(warn = 0)
  
  # Calibration curve ----------------------------------------------------------
  
  # Calibration is only done for the first plan of the day. Later plans will not
  # have calibration data. If calibrants were ran in current run, create 
  # calibration curve.
  if (nrow(calibrants) > 0) {
  
    # Mean RBL is used as the intercept of the calibration curve
    rbl_mean <- mean(rbl$Abs)
    
    # Hide messages printed by geom_smooth
    options(warn = -1)
    
    lm     <- lm(calibrants$Concentration ~ calibrants$Abs)
    lm_eqn <- paste0(round(lm$coefficients[[2]],4), 
                     " * Abs + (", 
                     round(lm$coefficients[[1]],2), 
                     ")")
    
    plot_calibration_curve <- calibrants %>% 
      ggplot(aes(Concentration, Abs)) +
      geom_point(na.rm = TRUE) +
      geom_smooth(method = "lm", na.rm = TRUE) +
      geom_text(aes(min(Concentration), max(Abs), label = lm_eqn, 
                     vjust = "inward", hjust = "inward")) +
      ggtitle(label = paste0("Calibrant Report: R^2 = ", 
                             round(summary(lm)$r.squared, 4))) +
      geom_hline(yintercept = rbl_mean, linetype = "longdash") +
      geom_text(aes(max(Concentration), rbl_mean, label = "Mean RBL", 
                    vjust = "inward", hjust = "inward"))
    
    options(warn = 0)
    
  }
  
  # Duplicate summaries --------------------------------------------------------

  plot_perc_diff <- result_qc %>% 
    ggplot(aes(DateTime, `Concentration (perc diff)`)) + 
    geom_point(na.rm = TRUE) + 
    ggtitle(paste("Percent Difference of Duplicates")) +
    geom_hline(yintercept = 10,  linetype = "dashed") + 
    geom_hline(yintercept = -10, linetype = "dashed") + 
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Percent Difference") +
    xlab("Run Time") +
    scale_x_datetime(date_labels = "%H:%M")
  
  plot_diff <- result_qc %>% 
    ggplot(aes(DateTime, `Concentration (diff)`)) + 
    geom_point(na.rm = TRUE) +
    ggtitle(paste("Difference between Duplicates")) + 
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Difference") +
    xlab("Run Time") +
    scale_x_datetime(date_labels = "%H:%M")
  
  # Quality controls -----------------------------------------------------------
  # Plot controls over duration of run
  
  # Because these nominals are showed in the control plot below
  nominal_min <- min(controls$Nominal)
  nominal_max <- max(controls$Nominal)
  
  # Because we display the mdl (Method detection limit) on the control plot below
  mdl <- controls %T>%
    # There are some files with "***" as concentration. as.numeric() to set 
    # those to NA
    {options(warn = -1)} %>% 
    mutate(Concentration = as.numeric(Concentration)) %T>%
    {options(warn = 0)} %>%
    filter(SampleID == "STND", !is.na(`Concentration`)) %>%
    with(sd(Concentration)*qt(c(.95), df = (length(Concentration) - 1)))

  # Hide messages printed by geom_smooth
  options(warn = -1)
  
  plot_controls <- controls %>% 
    ggplot(aes(DateTime, Concentration, color = SampleID, fill = SampleID)) +
    geom_point(na.rm = TRUE) +
    geom_smooth(formula = y ~ x, method = "lm", na.rm = TRUE, show.legend = TRUE) +
    ggtitle(label = paste("Controls")) + 
    theme(axis.text.x = element_text(angle = 45)) +
    xlab("Runtime") +
    ylab("Concentration") +
    geom_hline(yintercept = nominal_min, linetype = "dashed", colour = "dimgrey") +
    geom_text(aes(max(DateTime), nominal_min, label = "Min Nominal", 
                   vjust = "inward", hjust = "inward"),       colour = "dimgrey") +
    geom_hline(yintercept = nominal_max, linetype = "dashed", colour = "dimgrey") +
    geom_text(aes(max(DateTime), nominal_max, label = "Max Nominal", 
                   vjust = "inward", hjust = "inward"),       colour = "dimgrey") +
    geom_hline(yintercept = mdl,                              colour = "blue3") +
    geom_text(aes(min(DateTime), mdl, label = "MDL", 
                   vjust = "inward", hjust = "inward"),       colour = "blue3") 
  
  options(warn = 0)

  # Creating final, combined plot ----------------------------------------------
  
  # plot_controls in particular can throw warnings depending on if there are too
  # few points for a proper linear model. These warnings are not desired
  plot_diffs <- suppressWarnings(arrangeGrob(plot_diff, 
                                             plot_perc_diff, 
                                             plot_controls, 
                                             ncol = 1))
  
  if (exists("plot_calibration_curve")) {
      plot_extras <- plot_calibration_curve
  } else {
    plot_extras <- NA
  }
  
  # Filenames begin with 8 digits indicating the run date
  run_date <- as.Date(substr(tail(unlist(str_split(path, "/")), 1), 1, 8), 
                      format = "%Y%m%d")
  
  title <- str_remove(tail(unlist(str_split(path, "/")), 1), "\\.xls")
  
  # Hide messages that warn us when linear model is applied on a small number
  # of points
  options(warn = -1)
  
  if (all(is.na(plot_extras))) {
    plot_all <- suppressMessages(arrangeGrob(plot_diffs, top = title))
  } else {
    plot_all <- suppressMessages(arrangeGrob(plot_diffs, plot_extras, ncol = 2, top = title))
  }
  
  options(warn = 0)
  
  return(plot_all)
  
}

create_output_folders <- function() {
  # In case if these directories are not yet created, they must be created
  dir.create(path = "data/output", showWarnings = FALSE)
  dir.create(path = "data/output/plots", showWarnings = FALSE)
  dir.create(path = "data/output/processed_data", showWarnings = FALSE)
}
