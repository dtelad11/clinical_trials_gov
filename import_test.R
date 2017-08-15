rm(list = ls())

`%>%` <- dplyr::`%>%`

verbose <- TRUE
data_path <- "D:/Downloads/AllPublicXML"

id_path <- "NCT0315xxxx"

trial_ids <- dir(file.path(data_path, id_path))

trial_df <- lapply(trial_ids, function(trial_id) {
  if (verbose) message(trial_id)
  
  trial_path <- file.path(data_path, id_path, trial_id)
  xml <- XML::xmlTreeParse(trial_path)
  xml_list <- XML::xmlToList(xml)
  
  # ADD FUNCTION THAT PROCESSES
  # NULL -> NA (plus all dependent variables)
  # LIST -> PARSE
  # xml_list$enrollment$.attrs["type"]
  
  official_title <- xml_list$official_title
  if (is.null(official_title)) official_title <- NA
  
  start_date <- xml_list$start_date
  start_date_type <- NA
  if (is.list(start_date)) {
    start_date_type <- start_date$.attrs[[1]]
    start_date <- start_date$text
  }
  
  enrollment <- xml_list$enrollment

  xml_df <- data.frame(
    NctId = xml_list$id_info$nct_id,
    BriefTitle = xml_list$brief_title,
    OfficialTitle = official_title,
    StartDate = start_date,
    StartDateType = start_date_type,
    Phase = xml_list$phase,
    StudyType = xml_list$study_type,
    Enrollment = xml_list$enrollment$text,
    EnrollmentType = xml_list$enrollment$.attrs[[1]],
    Condition = xml_list$condition
  )
  
  if (nrow(xml_df) > 1) stop("xml_df has more than one row")
  
  xml_df
}) %>% dplyr::bind_rows() %>%
  tibble::as_tibble()


# biospec_descr -- blood

