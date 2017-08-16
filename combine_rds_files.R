# Combine all of the RDS files into a single data frame.
rm(list = ls())

`%>%` <- dplyr::`%>%`

# Parameters ------------------------------------------------------------------
verbose <- TRUE
export_path <-
  file.path("D:/Dropbox (Personal)/astrolabe/grants",
            "mount_sinai_sttr_grant_20170905", "clinical_trials")


# Functions -------------------------------------------------------------------
#' Convert the date string into a date.
#' 
#' Identify the date format used and convert into a date.
#' @param date_str A date in string format, or a vector of dates in string
#' format.
#' @return A date, or a vector of dates.
convertDate <- function(date_str) {
  if (length(date_str) > 1) {
    # Using do.call to make sure dates are not converted to numeric.
    do.call(c, lapply(date_str, convertDate))
  } else {
    if (is.na(date_str)) {
      NA
    } else {
      # Remove comma and tokenize.
      tokens <- strsplit(gsub(",", "", date_str), split = " ")[[1]]
      
      # First token is always month.
      month <- match(tokens[1], month.name)
      
      if (length(tokens) == 2) {
        # Format is Month Year.
        day <- 1
        year <- as.numeric(tokens[2])
      } else {
        # Format is Month Day Year.
        day <- as.numeric(tokens[2])
        year <- as.numeric(tokens[3])
      }
    }
    
    as.Date(ISOdate(year, month, day))
  }
}

# Main ------------------------------------------------------------------------
id_paths <- dir(export_path)

clinical_trials_df <- lapply(id_paths, function(filename) {
  readRDS(file.path(export_path, filename))
}) %>% dplyr::bind_rows()

# Switch from long to wide.
clinical_trials_df <-
  reshape2::dcast(clinical_trials_df, TrialId ~ Property, value.var = "Value")
# Convert dates from string to date.
clinical_trials_df$StartDate <- convertDate(clinical_trials_df$StartDate)
clinical_trials_df$CompletionDate <-
  convertDate(clinical_trials_df$CompletionDate)
# Convert enrollment to integer.
clinical_trials_df$Enrollment <- as.numeric(clinical_trials_df$Enrollment)
# Remove file name.
clinical_trials_df[["TrialId"]] <- NULL

clinical_trials_df <- tibble::as_tibble(clinical_trials_df)

clinical_trials_df_filename <- 
  file.path(export_path, "clinical_trials_df.RDS")
saveRDS(clinical_trials_df, clinical_trials_df_filename)
