# Find relevant trials.
rm(list = ls())

`%>%` <- dplyr::`%>%`

# Parameters ------------------------------------------------------------------
verbose <- TRUE
export_path <-
  file.path("D:/Dropbox (Personal)/astrolabe/grants",
            "mount_sinai_sttr_grant_20170905", "clinical_trials")


# Main ------------------------------------------------------------------------
clinical_trials_df_filename <- 
  file.path(export_path, "clinical_trials_df.RDS")
clinical_trials_df <- readRDS(clinical_trials_df_filename)

# Count total number of conditions.
condition_count <- dplyr::count(clinical_trials_df, Condition)
write.csv(condition_count %>%
            dplyr::filter(n >= 50) %>%
            dplyr::arrange(desc(n)),
          "D:/temp/condition_count.csv", row.names = FALSE)

clinical_trials_df %>%
  dplyr::filter("2016-01-01" <= StartDate,
                StartDate <= "2016-12-31",
                StudyType == "Interventional",
                Phase != "N/A")

clinical_trials_df %>%
  dplyr::filter("2016-01-01" <= StartDate, StartDate <= "2016-12-31") %>%
  dplyr::summarize(sum(Enrollment, na.rm = TRUE))

clinical_trials_df %>%
  dplyr::filter("2016-01-01" <= StartDate, StartDate <= "2016-12-31",
                Phase %in% c("Early Phase 1", "Phase 1",
                             "Phase 1/Phase 2", "Phase 2")) %>%
  dplyr::summarize(sum(Enrollment, na.rm = TRUE))

