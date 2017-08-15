rm(list = ls())

library(parallel)
`%>%` <- dplyr::`%>%`

# Parameters ------------------------------------------------------------------
verbose <- TRUE
import_years <- as.character(2012:2017)
data_path <- "D:/Downloads/AllPublicXML"
export_path <-
  file.path("D:/Dropbox (Personal)/astrolabe/grants",
            "mount_sinai_sttr_grant_20170905", "clinical_trials")
no_cores <- detectCores() - 1


# Functions -------------------------------------------------------------------
#' Process a property from a ClinicalTrials.gov XML file.
#' 
#' Given a property name and an XML endpoint, converts the endpoint into a data
#' frame. Empty data points become NAs, single-values become one-row data
#' frames, and multi-values have one row for the text field and one for each
#' attribute.
#' 
#' @param property The name of the property.
#' @param l The XML endpoint (NULL, a character, or a list with text and
#' $.attrs fields).
#' @return A data frame version of the XML endpoint.
xmlProcessProperty <- function(property, l) {
  if (is.null(l)) {
    # Property missing, return empty.
    data.frame(Property = property, Value = NA)
  } else if (is.character(l)) {
    # Property has a single value.
    data.frame(Property = property, Value = l)
  } else if (is.list(l)) {
    # Property has multiple values.
    df <- data.frame(Property = property, Value = l$text)
    
    for (attr in names(l$.attrs)) {
      df <- rbind(
        df,
        data.frame(
          Property = paste0(property, "_", attr),
          Value = l$.attrs[[attr]]
        )
      )
    }
    
    df
  } else {
    stop(paste0("unknown XML endpoint format for property \"", property, "\""))
  }
}


# Main ------------------------------------------------------------------------
id_paths <- dir(data_path)
id_paths <- setdiff(id_paths, "Contents.txt")

for (id_path in id_paths) {
  if (verbose) message(id_path)
  
  trial_ids <- dir(file.path(data_path, id_path))
  
  # Examine last file, skip this directory if it's not within last few years.
  xml <- XML::xmlTreeParse(file.path(data_path, id_path, tail(trial_ids, 1)))
  xml_list <- XML::xmlToList(xml)
  start_date <- xml_list$start_date
  if (is.null(start_date)) next
  if (is.list(start_date)) start_date <- start_date$text
  if (verbose) message(start_date)
  date_in_import_years <- 
    grep(paste0("(", paste(import_years, collapse = "|"), ")"), start_date)
  if (length(date_in_import_years) == 0) next
  
  # Skip directories that were already imported.
  trial_df_file_path <- file.path(export_path, paste0(id_path, ".RDS"))
  if (file.exists(trial_df_file_path)) next
  
  # Initiate cluster.
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("data_path", "id_path", "%>%", "xmlProcessProperty"))
  
  # Import all trials in this directory.
  trial_df <- parLapply(cl, trial_ids, function(trial_id) {
    trial_path <- file.path(data_path, id_path, trial_id)
    xml <- XML::xmlTreeParse(trial_path)
    xml_list <- XML::xmlToList(xml)
    
    # Scan properties and convert to DF.
    xml_endpoints <- tibble::tibble(
      Property = c(
        "NctId",
        "BriefTitle",
        "OfficialTitle",
        "StartDate",
        "CompletionDate",
        "Phase",
        "StudyType",
        "Enrollment",
        "Condition"
      ),
      XmlEndpoint = list(
        xml_list$id_info$nct_id,
        xml_list$brief_title,
        xml_list$official_title,
        xml_list$start_date,
        xml_list$completion_date,
        xml_list$phase,
        xml_list$study_type,
        xml_list$enrollment,
        xml_list$condition
      )
    )
    
    # Process each property in turn.
    xml_endpoints %>%
      apply(1, function(row)
        xmlProcessProperty(row$Property, row$XmlEndpoint)) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(TrialId = trial_id)
  })
  
  # Shut down cluster and export data frame.
  stopCluster(cl)

  trial_df <- trial_df %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::select(TrialId, Property, Value)
  
  saveRDS(trial_df, trial_df_file_path)
}

