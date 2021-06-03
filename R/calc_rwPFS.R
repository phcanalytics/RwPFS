
#' Calculate the real-world Progression-free Survival (rwPFS) endpoint
#'
#' @param df a data frame
#' @param .start_date character. The name of a date column in df that represents
#'     the baseline date
#' @param .visit_gap_start_date character. The name of a date column in df that represents
#' the date of the last visit before a large (usually >90d) visit gap occurring after start_date (if any).
#' Progression follow-up will be censored at this date (latest). Dates should be NA if there's no visit gap.
#' @param .last_progression_abstraction_date character. The name of a date column in df that represents
#' the date up which real-world progression was abstracted in the database.
#' Progression follow-up will be censored at this date (latest).
#' @param .progression_date character. The name of a date column in df that represents
#' the date of first progression after start date (if any). Dates should be NA if there's no progression.
#' @param .last_activity_date character. The name of a date column in df that represents
#' the date of last structural activity in the database. Progression follow-up will be censored at this date (latest).
#' @param .death_date character. The name of a date column in df that represents the date of death.
#' Dates should be NA if there's no recorded death.
#' @param .death_window_days integer. How many days after end of progression follow-up should death events be included?
#' This is necessary because patients often drop out of the database shortly before death, and (only) death events correlated with
#' the end of follow-up should be captured. Common values are 30-60 days, but this may
#' depend on various factors. See vignette for how to decide on the length of this time window.
#' @param .max_follow_up_days integer. Maximum number of days after which patients will be censored.
#' @param .label character. Label to keep track of multiple rwPFS endpoints in one dataset.
#'
#' @description Calculates the real-world PFS (rwPFS) endpoint given information on follow-up period, progression-, and death dates.
#'
#'
#' @return Returns a data frame with the following new data columns added:
#'
#' \strong{New Columns:}
#'
#' \describe{
#'
#'   \item{rwpfs<label>_eof_date}{The end date of progression follow-up. This is the earliest non-missing date of \code{visit_gap_start_date},
#'   \code{last_clinic_note_date}, and \code{last_activity_date}}
#'
#'   \item{rwpfs<label>_event_type}{The type of rwPFS event: "Death", "Progression", "Censored", or "Missing" (if rwPFS could not be calculated)}
#'
#'   \item{rwpfs<label>_date}{The rwPFS event- or censoring date.}
#'
#'   \item{rwpfs<label>_days}{The time from \code{start_date} to \code{rwpfs<label>_date} in days.}
#'
#'   \item{rwpfs<label>_event}{Whether a rwPFS event was recorded (1) or the observation was censored (0) }
#'
#'   \item{rwpfs<label>_months}{The time from \code{start_date} to \code{rwpfs<label>_date} in months.}
#'
#' }
#'
#'If rwPFS could not be calculated, the \code{rwpfs<label>_event_type} will be set to "Missing", and all other columns will contain missing values.
#'This can be due to one of the following reasons: missing \code{last_clinic_note_date}, missing \code{last_activity_date}, \code{last_clinic_note_date} equal to \code{start_date},
#'or \code{death_date} being earlier than \code{start_date} (this can happen because death dates are rounded to mid-month in Flatiron).
#'
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
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
#'   )  %>%
#'   
#'   #Add information on any long visit gaps
#'   fi_calc_visitgap(
#'     index_date = "lot1startdate",
#'     gapdays = 90,
#'     what = "After",
#'     force_database = FALSE
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
#' 
#' 
#' #Add rwPFS
#' df <- df %>%
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 30,
#'     .max_follow_up_days = Inf,
#'     .label = "_no_pseudo_no_mixed"
#'   )
#' 
#' 
#'}
#'
#'
calc_rwPFS <- function(df,
                       .start_date = NULL,
                       .visit_gap_start_date = NULL,
                       .last_progression_abstraction_date = NULL,
                       .progression_date = NULL,
                       .last_activity_date = NULL,
                       .death_date = NULL,
                       .death_window_days = NULL,
                       .max_follow_up_days = Inf,
                       .label = ""
) {

  #Check if column names are resent in data frame
  .cols = c(
    .start_date,
    .visit_gap_start_date,
    .last_progression_abstraction_date,
    .progression_date,
    .last_activity_date,
    .death_date
  )

  missing_columns <-.cols[!(.cols %in% colnames(df))]

  if(length(missing_columns) != 0){
    stop(paste("The following required columns are missing: ", paste(missing_columns, collapse = ", ")))
  }


  if (is.null(.death_window_days) | class(.death_window_days) != "numeric" | .death_window_days < 0) {
    stop("Please specify the time window after end of progression follow-up (in days) for inclusion of death events.")
  }

  #TODO test date class
  #TODO make sure e.g. start_date is before all other dates.. etc (but beware of FI inconsistencies, e.g. rounding of death to mid-month)

  #create symbols for input column names
  .start_date = rlang::sym(.start_date)
  .visit_gap_start_date = rlang::sym(.visit_gap_start_date)
  .last_progression_abstraction_date = rlang::sym(.last_progression_abstraction_date)
  .progression_date = rlang::sym(.progression_date)
  .last_activity_date = rlang::sym(.last_activity_date)
  .death_date = rlang::sym(.death_date)


  #create output column names
  rwPFS_eof_date <- rlang::sym(paste0("rwPFS", .label, "_eof_date"))
  rwPFS_event_type <- rlang::sym(paste0("rwPFS", .label, "_event_type"))
  rwPFS_date <- rlang::sym(paste0("rwPFS", .label, "_date"))
  rwPFS_days <- rlang::sym(paste0("rwPFS", .label, "_days"))
  rwPFS_months <- rlang::sym(paste0("rwPFS", .label, "_months"))
  rwPFS_event <- rlang::sym(paste0("rwPFS", .label, "_event"))



  #------------------------
  # rwPFS
  #------------------------
  df %>%
    dplyr::mutate(

      !!rwPFS_eof_date := pmin(
        !!.last_progression_abstraction_date,
        !!.visit_gap_start_date,
        !!.last_activity_date,
        na.rm = TRUE
      ),

      !!rwPFS_event_type := dplyr::case_when(
        #The order of these cases matters

        #Missing
        is.na(!!.last_progression_abstraction_date) |
          is.na(!!.last_activity_date) |
          is.na(!!.start_date) |
          !!.last_progression_abstraction_date == !!.start_date |
          !!.death_date <= !!.start_date ~ "Missing",


        #Progression
        !!.progression_date == pmin(!!rwPFS_eof_date,
                                      !!.death_date,
                                      !!.progression_date,
                                      na.rm = TRUE) ~  "Progression",

        #Death
        !!.death_date - lubridate::days(.death_window_days) == pmin(
          !!rwPFS_eof_date,
          !!.death_date - lubridate::days(.death_window_days),
          #capturing deaths up to x days  after end of prog follow-up
          !!.progression_date,
          na.rm = TRUE
        ) ~  "Death",

        #Censoring
        !!rwPFS_eof_date == pmin(
          !!rwPFS_eof_date,
          !!.death_date - lubridate::days(.death_window_days),
          !!.progression_date,
          na.rm = TRUE
        )  ~  "Censored",

        #Default case
        TRUE  ~ NA_character_
      ),

      !!rwPFS_date := dplyr::case_when(
        !!rwPFS_event_type == "Missing" ~ NA_character_ %>% lubridate::as_date(),
        !!rwPFS_event_type == "Progression" ~ !!.progression_date,
        !!rwPFS_event_type == "Death" ~ !!.death_date,
        !!rwPFS_event_type == "Censored" ~ !!rwPFS_eof_date,
        TRUE ~ NA_character_ %>% lubridate::as_date()
      ),

      !!rwPFS_days := (!!rwPFS_date - !!.start_date) %>% as.numeric,


      !!rwPFS_event := dplyr::case_when(
        !!rwPFS_event_type == "Missing" ~ NA_real_,
        !!rwPFS_event_type == "Progression" ~ 1.0,
        !!rwPFS_event_type == "Death" ~ 1.0,
        !!rwPFS_event_type == "Censored" ~ 0.0,
        TRUE ~ NA_real_
      ),

      #Restrict maximum follow_up to .max_follow_up_days = NA
      !!rwPFS_event := dplyr::if_else(!!rwPFS_days <= .max_follow_up_days,
                               !!rwPFS_event,
                               0),
      !!rwPFS_days := dplyr::if_else(
        !!rwPFS_days <= .max_follow_up_days,
        !!rwPFS_days,
        .max_follow_up_days
      ),

      !!rwPFS_months := !!rwPFS_days/30.4375
    )
}



