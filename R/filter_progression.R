



#' Returns information on real-world progression, filtering & aggregating Flatiron's progression table
#'
#' @param .progression_table a data frame with column structure identical to Flatiron's progression table plus an additional column
#' containing the baseline date of the analysis.
#' @param .start_date character. The name of a date column in .progression_table that represents
#' the baseline date
#' @param .require_radiographic logical. Should only radiographically confirmed progression
#' events be considered?
#' @param .exclude_pseudoprogression logical. Should progression events with mention of
#' pseudoprogression be excluded?
#' @param .discard_n_days integer. Number of days after baseline where progression events
#' will be discarded. Choose 0 to not discard any.
#' @param .label character. Label to keep apart different progression variables in the same dataset.
#' @param .prog_filter_expression character. A string containing R code (including unquoted
#' progression table column names) that evaluates to TRUE for all progression events that should be included.
#' Allows for fine-grained selection of events and use with all datamarts. Will be combined with `require_radiographic`
#' and `exclude_pseudoprogression` using logical AND.
#'
#' @description Filters and aggregates Flatiron's progression table and returns a dataframe with three columns: the patient id, the date of first progression occurring at least `discard_n_days` after start_date, and the
#' date up to which progression information was abstracted. A subset of progression events to be considered can be specified.
#'
#' @return Returns a data frame with the following three columns.
#' 
#' \strong{New Columns:}
#' 
#' \describe{
#' 
#'   \item{patientid}{Patient ID}
#'
#'   \item{progressiondate<label>}{The date of the first progression event occurring at least `discard_n_days` after start_date and passing the
#'   filtering criteria specified using the \code{require_radiographic}, \code{exclude_pseudoprogression} and \code{prog_filter_expression} arguments.
#'   \code{NA} if no progression event satisfies all criteria.}
#'
#'   \item{lastclinicnotedate<label>}{The date up to which real-world progression was abstracted in the database. \code{NA} if lastclinicnotedate
#'   is missing or before start_date}
#'
#' }
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' 
#' #Note: the FlatironKitchen package is used in these
#' #examples for simplicity. This is not a requirement.
#' 
#' library(FlatironKitchen)
#' library(dplyr)
#' 
#' #Initialize FlatironKitchen object
#' fk <- fi_start(datamart = "AdvancedNSCLC",
#'                title = "rwPFS in aNSCLC")  %>%
#'   
#'   #Use start of line as start_date
#'   fi_add_lineoftherapy_flatiron(
#'     lines = c(1, 2),
#'     index_date = "advanceddiagnosisdate",
#'     left = 0,
#'     right = 90,
#'     calc_duration = FALSE
#'   ) %>%
#'   
#'   #Restrict to Carboplatin & Paclitaxel in 1st line
#'   fi_cohort_include(
#'     lot1linename == "Carboplatin,Paclitaxel",
#'     description = "Carboplatin & Paclitaxel in 1st line",
#'     keep_na = FALSE
#'   ) 
#' 
#' #Pull the analysis dataset from 
#' #the FlatironKitchen object
#' df <- fk %>%
#'   fi_pull_data()
#' 
#' #Download the progression table belonging
#' #to the current datamart
#' progression <- fk %>%
#'   fi_read_table(table_name="ENHANCED_ADVNSCLCPROGRESSION") %>%
#'   collect()
#'   
#' #Add progression information to analysis dataset
#' df <- df %>%
#'   #select the start_date column..
#'   select(patientid, lot1startdate) %>%
#'   
#'   #..and add it to the raw progression table
#'   left_join(progression, by = "patientid") %>%
#'   
#'   #Aggregate progression information (one-row per patient)
#'   filter_progression(
#'     .start_date = "lot1startdate",
#'     .require_radiographic = FALSE,
#'     .exclude_pseudoprogression = TRUE,
#'     .discard_n_days = 0,
#'     .label = "_no_pseudo_no_mixed",
#'     .prog_filter_expression = "ismixedresponse == 'No'"
#'   ) %>%
#'   
#'   #Combine progression columns with analysis dataset
#'   right_join(df, by = "patientid")
#' }
#'
filter_progression <- function(.progression_table,
                               .start_date = NULL,
                               .require_radiographic = NULL,
                               .exclude_pseudoprogression = NULL,
                               .discard_n_days = NULL,
                               .label = "",
                               .prog_filter_expression = NULL
                               ) {

  # Validity checks ------------------------------------------


  # Get start_date (and ensure is an existing data variable in x$data)
  if (is.null(.start_date)) {
    stop("Please specify start_date - the column which corresponds to your index date")
  }

  if (.start_date %in% names(.progression_table) == FALSE) {
    stop(.start_date, "is not a column in your .progression_table (and must be)")
  } else if (class(.progression_table[[.start_date]]) != "Date") {
    stop(.start_date, "is not of class date (and must be)")
  }



  if (is.null(.require_radiographic) | class(.require_radiographic) != "logical") {
    stop("Please specify whether radiographic confirmation of progression is required (TRUE/FALSE)")
  }


  if (is.null(.exclude_pseudoprogression) | class(.exclude_pseudoprogression) != "logical") {
    stop("Please specify whether to exclude progression events where pseudoprogression is mentioned (TRUE/FALSE)")
  }


  if (is.null(.discard_n_days) || class(.discard_n_days) != "numeric" || .discard_n_days < 0) {
    stop("Please specify the number of days after baseline where progression events are ignored (0 for none)")
  }


  if (class(.label) != "character") {
    stop("The label for output columns should be a string!")
  }


  if (!is.null(.prog_filter_expression) & class(.prog_filter_expression) != "character") {
    stop("The excl_flag_expression argument should be either null or a string containing a logical expression using column names of the progression table")
  }


  # Preparation ----------------------------------------

  start_date <- rlang::sym(.start_date)

  progression_filter <- parse(text = paste(
    dplyr::if_else(
      .require_radiographic,
      "isradiographicevidence == 'Yes'",
      "TRUE"
    ),
    dplyr::if_else(
      .exclude_pseudoprogression,
      "ispseudoprogressionmentioned == 'No'",
      "TRUE"
    ),
    dplyr::if_else(
      is.null(.prog_filter_expression),
      "TRUE",
      .prog_filter_expression
    )
    ,
    sep = " & "
  ))



  # Process ---------------------------------------------

  data_progression_orpp <- .progression_table %>%

    # Must have progression abstracted beyond index date
    dplyr::filter(lastclinicnotedate > !!start_date) %>%

    dplyr::rowwise() %>%

    #Filter progression events (can't use dplyr::filter - we'd lose lastclinicnotedate!)
    dplyr::mutate(

      #keep progression events occurring after start_date
      progressiondate = dplyr::if_else(
        progressiondate > !!start_date,
        progressiondate,
        NA_character_ %>% as.Date(),
        missing = NA_character_ %>% as.Date()
      ),

      #keep progression events satisfying progression_filter criteria
      progressiondate = dplyr::if_else(
        eval(progression_filter),
        progressiondate,
        NA_character_ %>% as.Date(),
        missing = NA_character_ %>% as.Date()
      ),

      # # Calculate lag
      progressiondate_startdate_lag = as.numeric(progressiondate - !!start_date),

      #Discard events within discard_n_days days after baseline
      progressiondate = dplyr::if_else(
        progressiondate_startdate_lag <= .discard_n_days,
        NA_character_ %>% as.Date(),
        progressiondate,
        missing = NA_character_ %>% as.Date()
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(patientid) %>%
    dplyr::summarise(
      #start_date = min(!!start_date), #Uncomment for debugging
      progressiondate = suppressWarnings(min(progressiondate, na.rm = TRUE)), #some patients may have NA only
      lastclinicnotedate = min(lastclinicnotedate, na.rm = TRUE)
    ) %>%
    
    #min() above returned -Inf when no non-NA progressiondate values were present. Set -Inf to NA.
    dplyr::mutate(progressiondate  = dplyr::if_else(
      !is.finite(progressiondate),
      as.Date(NA_character_),
      progressiondate
      )) %>%
    
    # Add all patients back in (including those with missing progression information)
    dplyr::right_join(.progression_table %>%
                        dplyr::distinct(patientid), by = "patientid")


  #Check if there are patients with missing progression information
  n_missing <- sum(is.na(data_progression_orpp$lastclinicnotedate))
  if(n_missing > 0){
    warning(
      paste(
        n_missing,
        "patient(s) had no progression information abstracted beyond startdate (lastclinicnotedate set to missing)"
      )
    )
  }


  #Apply label to the column names
  data_progression_orpp <- data_progression_orpp %>%
    dplyr::transmute(
      patientid,
      !!rlang::sym(paste0("progressiondate", .label)) := progressiondate,
      !!rlang::sym(paste0("lastclinicnotedate", .label)) := lastclinicnotedate
    )


  return(data_progression_orpp)
}





