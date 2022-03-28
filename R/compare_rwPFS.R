#' Compare different real-world PFS (rwPFS) definitions
#'
#' @param .df a data frame
#' @param .labels character vector. A vector of strings representing the labels that identify the
#' rwPFS endpoints/definitions to be compared (in order of desired appearance in the output table).
#' @param .reference character. Label identifying the reference rwPFS definition that others are compared against.
#' @param .incremental_deaths_column logical. Should the output table contain an additional column comparing the incremental
#' number of death events captured from one definition to the next? Intended for comparing different \code{death_window_days}
#' parameter values in \code{calc_rwPFS}.
#'
#'
#' @description Compares a range of different rwPFS definitions with respect to event composition, KM median time to event,
#' Hazard ratio relative to the reference, and incremental no. of death events from one rwPFS definition to the next (optional).
#'
#' @return Returns a summary table
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' 
#' library(dplyr)
#' library(RwPFS) 
#' library(survival)
#' 
#' 
#' #Generate rwPFS endpoints across a range of different definitions
#' 
#' #Starting point is the simprog simulated dataset included in the RwPFS package
#' df <- simprog %>% 
#'   
#'   #Start with the "_all_events" rwPFS endpoint
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 30,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_all_events"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   # Add "discard_le_14d" rwPFS
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_discard_le_14d",
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 30,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_discard_le_14d"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%  
#'   
#'   # Add "radiographic_only" rwPFS
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_radiographic_only",
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 30,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_radiographic_only"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%  
#'   
#'   # Add "no_pseudoprogression" rwPFS
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_no_pseudoprogression",
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 30,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_no_pseudoprogression"                  #Label for this rwPFS endpoint (for comparisons)
#'   ) 
#' 
#' 
#' #add an addition rwPFS endpoint where visit gaps are ignored (not used for early censoring)
#' 
#' #On a side note, supplying a custom date column as .visit_gap_start_date can e.g. be used to trick calc_rwPFS into censoring observations early (e.g. at a change of the line of therapy). 
#' #Similarly, progression events can be imputed (e.g. at a line of therapy change) by creating a custom .progression_date column.
#' 
#' df <- df %>%
#'   mutate(
#'     #adding an all_na column 
#'     all_na = NA_character_ %>% as.Date()
#'   ) %>%
#'   
#'   # Add "ignore_visit_gaps" rwPFS
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "all_na",                 #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 30,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_ignore_visit_gaps"                     #Label for this rwPFS endpoint (for comparisons)
#'   )   
#' 
#' #Compare the different rwPFS endpoints
#' df %>%
#'   compare_rwPFS(
#'     .labels = c(                         #Specify the rwPFS labels to be tabulated (in the desired order)
#'       "_all_events",
#'       "_discard_le_14d",
#'       "_radiographic_only",
#'       "_no_pseudoprogression",
#'       "_ignore_visit_gaps"
#'     ),
#'     .reference = "_all_events",
#'     .incremental_deaths_column = FALSE
#'   ) 
#' 
#' 
#' 
#' 
#' #Generate rwPFS endpoints with different durations of the time window for capturing death events:
#' 
#' df <- simprog %>% 
#'   
#'   #Start with the "_all_events" rwPFS endpoint
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 0,                           #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_0d_window"                             #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 10,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_10d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 20,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_20d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 30,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_30d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 40,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_40d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 50,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_50d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 60,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_60d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 70,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_70d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 80,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_80d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "baseline_date",                    #baseline date for measuring time-to-event
#'     .visit_gap_start_date = "visit_gap_start_date",   #the start date of any gap in visits >90 days  
#'     .last_progression_abstraction_date = "last_progression_abstraction_date", 
#'     .progression_date = "progression_date_all_events",#the date of progression (none: NA)
#'     .last_activity_date = "last_activity_date",       #the date of last activity in the database
#'     .death_date = "death_date",                       #the date of death
#'     .death_window_days = 90,                          #include death events <30d after progression EOF
#'     .max_follow_up_days = Inf,                        #censor patients after a maximum time. 
#'     .label = "_90d_window"                            #Label for this rwPFS endpoint (for comparisons)
#'   )
#' 
#' 
#' #Compare them, this time including an "incremental deaths" column
#' 
#' df %>%
#'   compare_rwPFS(
#'     .labels = paste0("_", 0:9*10, "d_window"),#Specify the rwPFS labels to be tabulated (in the desired order)
#'     .reference = "_30d_window",
#'     .incremental_deaths_column = TRUE
#'   )
#'
#'}
#'
compare_rwPFS <- function(
  .df,
  .labels = NULL,
  .reference = NULL,
  .incremental_deaths_column = NULL
){



  #Check labels are present
  if (is.null(.labels) | class(.labels) != "character" | length(.labels) < 2) {
    stop("Please provide a character vector with the labels of at least two rwPFS definitions to compare (label forms part of rwPFS column names, e.g. rwPFS<label>_event)")
  }


  #Make sure required columns are present
  rwpfs_colnames <- paste0("rwPFS",
                           paste0(.labels,
                                  rep(c("_months", "_event", "_event_type"), #c("_eof_date", "_event_type", "_date", "_days", "_months", "_event"),
                                      each= length(.labels)
                                  )
                           )
  )

  if (all(rwpfs_colnames %in% names(.df)) == FALSE) {
    stop(
      paste(
        "The following required columns are missing in your data:",
        paste0(rwpfs_colnames[!(rwpfs_colnames %in% names(.df))], collapse = "," )
      )

    )
  }


  #Check a reference rwPFS definition is give
  if (is.null(.reference) | class(.reference) != "character" ) {
    stop("Please provide the name of the reference label (should be one of those supplied via the 'labels' variable)")
  } else if (!(tolower(.reference) %in% .labels)) {
    stop("The reference label should be one of those supplied via the 'labels' variable")
  }
  .reference <- tolower(.reference)

  if (is.null(.incremental_deaths_column) | class(.incremental_deaths_column) != "logical") {
    stop("Please specify whether the summary table should include a column containing the incremental no. of death events from one rwPFS defnition to the next (TRUE/FALSE)")
  }

  #Patients with missing values cannot be used in the comparison
  missing_rwPFS <- .df %>%
    dplyr::filter_at(vars(ends_with("_event_type")), any_vars(. == "Missing")) %>%
    nrow()
  
  if (missing_rwPFS > 0) {
    warning(paste(missing_rwPFS, "Patients with missing rwPFS were found and removed"))
    .df <- .df %>%
      dplyr::filter_at(vars(ends_with("_event_type")), all_vars(. != "Missing"))
  }


  #Pivot to long format
  spec <- .df %>%
    dplyr::select(patientid, contains(.labels)) %>%
    tidyr::build_longer_spec(
      cols = contains("rwPFS") ,
      names_to = c("rwPFS_definition"),
      names_pattern = "rwPFS(.*)_.*[eof_date|event_type|date|days|months|event]$"
    ) %>%
    dplyr::mutate(
      rwPFS_definition = stringr::str_replace_all(rwPFS_definition, "_event$|_eof$", ""),
      .value = dplyr::case_when(
        stringr::str_detect(.name, "eof_date") ~ "rwPFS_eof_date",
        stringr::str_detect(.name, "event_type") ~ "rwPFS_event_type",
        stringr::str_detect(.name, "date") ~ "rwPFS_date",
        stringr::str_detect(.name, "days") ~ "rwPFS_days",
        stringr::str_detect(.name, "months") ~ "rwPFS_months",
        stringr::str_detect(.name, "event") ~ "rwPFS_event"
      ),
      rwPFS_definition = readr::parse_factor(
        as.character(rwPFS_definition),
        levels = .labels,
        ordered = TRUE
      )
    )

  d.prog_defs_long <- .df %>%
    dplyr::select(patientid, contains(.labels)) %>%
    tidyr::pivot_longer_spec(spec)



  #Compute KM median & HR relative to consensus variant

  reference_df <- d.prog_defs_long %>%
    dplyr::filter(rwPFS_definition == .reference)


  f <- function(df, reference_df){

    d.combined <- dplyr::bind_rows(df, reference_df) %>%
      dplyr::mutate(
        rwPFS_definition = as.character(rwPFS_definition) %>%
          factor() %>%
          stats::relevel(ref = .reference)
      )

    #KM median
    med <- survival::survfit(survival::Surv(rwPFS_months, rwPFS_event) ~ rwPFS_definition, data=df) %>%
      survminer::surv_median() %>%
      dplyr::mutate(
        KM_median = paste0(round(median, 2), " (", round(lower, 2), "-", round(upper, 2), ")")
      ) %>%
      dplyr::pull()

    #Hazard ratio
    if(all(df$rwPFS_definition == .reference)){
      hr <- "1 (reference)"
    } else {

      hr <- survival::coxph(
        survival::Surv(rwPFS_months, rwPFS_event) ~ rwPFS_definition,
        data = d.combined
      ) %>%
        broom::tidy(
          exponentiate = T,
          conf.int = T
        ) %>%
        dplyr::mutate(
          hazard_ratio = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")")
        ) %>%
        dplyr::pull()


    }

    df %>%
      dplyr::mutate(
        KM_median = med,
        hazard_ratio = hr
      )
  }


  d.prog_defs_long <- d.prog_defs_long %>%
    dplyr::mutate(grouping_var = rwPFS_definition) %>%
    dplyr::group_by(grouping_var) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(.x = data , .f= f, reference_df)
    ) %>%
    tidyr::unnest(cols = c(data))


  tableData <- d.prog_defs_long %>%
    dplyr::group_by(rwPFS_definition) %>%
    dplyr::summarise(
      Censoring = sum(rwPFS_event_type == "Censored"),
      Progression = sum(rwPFS_event_type == "Progression"),
      Death = sum(rwPFS_event_type == "Death"),
      Percent_death = 100*mean(rwPFS_event_type[rwPFS_event_type != "Censored"] == "Death") %>% round(digits = 3),
      KM_median = dplyr::first(KM_median),
      Hazard_ratio = dplyr::first(hazard_ratio)
    ) %>%
    dplyr::arrange(rwPFS_definition)



  if(.incremental_deaths_column){
    tableData <- tableData %>%
      dplyr::arrange(rwPFS_definition) %>%
      dplyr::mutate(
        Incremental_deaths = Death - dplyr::lag(Death)
      ) %>%
      dplyr::select(
        rwPFS_definition,
        Censoring,
        Progression,
        Death,
        Incremental_deaths,
        Percent_death,
        everything()
      )
  }

  #polish table appearance
  tableData <- tableData %>%
    dplyr::mutate(
      rwPFS_definition = as.character(rwPFS_definition),
      rwPFS_definition = stringr::str_replace(rwPFS_definition, pattern = "^_", replacement = ""),
      rwPFS_definition = stringr::str_replace_all(rwPFS_definition, pattern = "_", replacement = " "),
      rwPFS_definition = stringr::str_to_sentence(rwPFS_definition)
    ) %>%
    dplyr::rename_with(stringr::str_replace, pattern = "_", replacement = " ")


  return(tableData)
}




