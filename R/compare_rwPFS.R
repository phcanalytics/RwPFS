#' Compare different real-world PFS (rwPFS) definitions
#'
#' @param .df a data frame
#' @param .labels character vector. A vector of strings representing the labels that identify the
#' rwPFS endpoints/definitions to be compared (in order of desired appearance in the output table).
#' @param .reference character. Label identifying the reference rwPFS definition that others are compared against.
#' @param .incremental_deaths_column logical. Should the output table contain an additional column comparing the incremental
#' number of death events captured from one definition to the next? Intended for comparing different \code{death_window_days}
#' parameter values in \code{fi_calc_rwPFS}.
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
#' #Add several different rwPFS endpoints
#' df <- df %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 0,
#'     .max_follow_up_days = 1200,
#'     .label = "_0d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 10,
#'     .max_follow_up_days = 1200,
#'     .label = "_10d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 20,
#'     .max_follow_up_days = 1200,
#'     .label = "_20d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 30,
#'     .max_follow_up_days = 1200,
#'     .label = "_30d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 40,
#'     .max_follow_up_days = 1200,
#'     .label = "_40d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 50,
#'     .max_follow_up_days = 1200,
#'     .label = "_50d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 60,
#'     .max_follow_up_days = 1200,
#'     .label = "_60d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 70,
#'     .max_follow_up_days = 1200,
#'     .label = "_70d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 80,
#'     .max_follow_up_days = 1200,
#'     .label = "_80d_window"
#'   ) %>%
#'   
#'   calc_rwPFS(
#'     .start_date = "lot1startdate",
#'     .visit_gap_start_date = "lastvisitbeforegap",
#'     .last_progression_abstraction_date = "lastclinicnotedate_no_pseudo_no_mixed",
#'     .progression_date = "progressiondate_no_pseudo_no_mixed",
#'     .last_activity_date = "lastactivitydate",
#'     .death_date = "dateofdeath",
#'     .death_window_days = 90,
#'     .max_follow_up_days = 1200,
#'     .label = "_90d_window"
#'   ) %>%
#'   
#'   #Remove patients with missing rwPFS
#'   filter(
#'     rwPFS_0d_window_event_type != "Missing"
#'   )
#' 
#' #Compare different rwPFS definitions
#' df %>%
#'   compare_rwPFS(
#'     .labels = paste0("_", 0:9*10, "d_window"),
#'     .reference = "_30d_window",
#'     .incremental_deaths_column = TRUE
#'   ) %>%
#'   knitr::kable()
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




