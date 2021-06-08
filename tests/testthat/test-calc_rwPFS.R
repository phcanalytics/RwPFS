


test_that("calc_rwPFS validations", {
  
  mock_dates <- c(
    #NA - any date could be missing.
    as.Date(NA_character_), 
    #six dates one week apart
    lubridate::as_date("2018-01-01") + lubridate::weeks(1:6)
    )
  
  #All possible date permutations (order of dates, incl. NA & -Inf)
  mock_dataset <- expand.grid(
    start_date = mock_dates,
    visit_gap_start_date = mock_dates,
    last_progression_abstraction_date = mock_dates,
    progression_date = mock_dates,
    last_activity_date = mock_dates,
    death_date = mock_dates
  )
  
  
  mock_result <- calc_rwPFS(
    mock_dataset,
    .start_date = "start_date",
    .visit_gap_start_date = "visit_gap_start_date",
    .last_progression_abstraction_date = "last_progression_abstraction_date",
    .progression_date = "progression_date",
    .last_activity_date = "last_activity_date",
    .death_date = "death_date",
    .death_window_days = 30,
    .label = "_test"
    
  )
  
  
  
  #If all dates for determining follow-up are missing, eof_date and all rwPFS results should be missing
  testthat::expect_true(
    mock_result %>%
      dplyr::filter(is.na(visit_gap_start_date) &
             is.na(last_progression_abstraction_date) &
               is.na(last_activity_date)
             ) %>%
      {
        all(is.na(.$rwPFS_test_eof_date)) &
          all(.$rwPFS_test_event_type == "Missing") &
          all(is.na(.$rwPFS_test_date)) & 
        all(is.na(.$rwPFS_test_days)) & 
        all(is.na(.$rwPFS_test_event)) & 
        all(is.na(.$rwPFS_test_months))
      },
    label = "If all dates for determining follow-up are missing, eof_date and all rwPFS results should be missing"
  )

  #If start_date is missing, all rwPFS result columns except eof_date should be missing
  testthat::expect_true(
    mock_result %>%
      dplyr::filter(is.na(start_date)) %>%
      {
          all(.$rwPFS_test_event_type == "Missing") &
          all(is.na(.$rwPFS_test_date)) & 
          all(is.na(.$rwPFS_test_days)) & 
          all(is.na(.$rwPFS_test_event)) & 
          all(is.na(.$rwPFS_test_months))
      },
    label = "If start_date is missing, all rwPFS result columns except eof_date should be missing"
  )  

  
  #if there's at least one non-missing of last_activity_date, last_progression_abstraction_date, visit_gap_start_date
  #then the earliest of those should be end of follow-up for progression
  testthat::expect_true(
    mock_result %>%
      dplyr::filter(!(is.na(visit_gap_start_date) &
               is.na(last_progression_abstraction_date) &
               is.na(last_activity_date))
      ) %>%
      {
        all(
          pmin(.$last_activity_date, 
               .$last_progression_abstraction_date, 
               .$visit_gap_start_date, 
               na.rm = TRUE
               ) == .$rwPFS_test_eof_date
        )
      },
    label = "If there's at least one non-missing of last_activity_date, last_progression_abstraction_date, visit_gap_start_date then the earliest of those should be end of follow-up for progression"
  )  
  
  
  
  #if the minimum of last_activity_date, last_progression_abstraction_date, visit_gap_start_date
  #is before start date, then all result columns must be missing
  #FIXME fix this tes
  testthat::expect_true(
    mock_result %>%
      dplyr::filter(!(is.na(start_date) & #we need non-missing start_date
                    !(is.na(visit_gap_start_date) & #..and at least one non-missing of these 
                        is.na(last_progression_abstraction_date) &
                        is.na(last_activity_date)) &
                    rwPFS_test_eof_date < start_date
                      )
      ) %>%
      {
        all(.$rwPFS_test_event_type == "Missing") &
          all(is.na(.$rwPFS_test_date)) & 
          all(is.na(.$rwPFS_test_days)) & 
          all(is.na(.$rwPFS_test_event)) & 
          all(is.na(.$rwPFS_test_months))
      }
  )    
  t
  
  #TODO eof_date must be larger than start_date .. if the minimum of the above three is before start date, then rwPFS cannot be calculated
  #and is set to missing (event type as well as output dates) 
  
  #TODO If progression_date is <= eof_date, then rwPFS_date is equal to progression date, and event_type is "Progression" and event == 1
  #days/months is progression_date minus start_date
  
  #TODO else if there's no progression <= eof_date, but there's a non-missing death date within <30d after eof_date, then rwPFS date
  #must be equal to death date, event_type must be "Death", and event == 1, days/months is death_date minus start_date
  
  #If there's no progression <= eof_date and non-missing death date within <30d after eof_date, then rwPFS_event_type is "censored"
  #rwPFS_event == 0 , and days/months is eof_date - minus start_date
  
  #see vignette for context https://github.roche.com/bretscm2/rwPFS4Rshowcase
  
})
