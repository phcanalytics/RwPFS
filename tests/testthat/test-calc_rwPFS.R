


test_that("calc_rwPFS validations", {


  
  mock_dates <- c(
    #we want to test what calc_rwPFS does with -Inf because filter_progression returns it when no progression event is present (TODO:change)
    as.Date(-Inf), 
    #NA - whichh *should* be returned by filter progression in absence of progression events
    as.Date(NA_character_), 
    #six dates one month apart
    lubridate::as_date("2018-01-01") + lubridate::months(1:6)
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
  
  
  # mock_result <- calc_rwPFS(
  #   
  # )
  
  #see vignette for context https://github.roche.com/bretscm2/rwPFS4Rshowcase
  
  #TODO: eof_date (in output of calc_rwPFS) must be the smallest of last_activity_date, last_clinic_note_date, visit_gap_start_date
  #but must not use -Inf as smallest - if -Inf is present it should be ignored (treated as missing) and the smallest non-missing
  #date should be the eof_date
  
  #TODO eof_date must be larger than start_date .. if the minimum of the above three is before start date, then rwPFS cannot be calculated
  #and is set to missing (event type as well as output dates) 
  
  #TODO If progression_date is <= eof_date, then rwPFS_date is equal to progression date, and event_type is "Progression" and event == 1
  #days/months is progression_date minus start_date
  
  #TODO else if there's no progression <= eof_date, but there's a non-missing death date within <30d after eof_date, then rwPFS date
  #must be equal to death date, event_type must be "Death", and event == 1, days/months is death_date minus start_date
  
  #If there's no progression <= eof_date and non-missing death date within <30d after eof_date, then rwPFS_event_type is "censored"
  #rwPFS_event == 0 , and days/months is eof_date - minus start_date
  
})
