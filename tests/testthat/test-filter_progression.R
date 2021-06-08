


test_that("filter_progression validations", {
  
  
  mock_dates <- lubridate::as_date("2012-01-01") + 
    lubridate::days(1:5) 
  
  mock_prog_table <- tidyr::expand_grid(
    patientid = 1:5,
    startdate = lubridate::as_date("2012-01-02"),
    progressiondate = mock_dates,
    lastclinicnotedate = max(mock_dates),
    isradiographicevidence =  c("No"),
    ispseudoprogressionmentioned = c("Yes"),
    isdivineintervention = NA
  ) 
  
  
  
 #least restricted
 tmp <-  filter_progression(mock_prog_table,
                     .start_date = "startdate",
                     .require_radiographic = FALSE,
                     .exclude_pseudoprogression = FALSE,
                     .discard_n_days = 0,
                     .prog_filter_expression = NULL
  )
  testthat::expect_equal(
    nrow(tmp),
    5
  )
  testthat::expect_equal(
    length(unique(tmp$progressiondate)),
    1
  )
  testthat::expect_equal(
    unique(tmp$progressiondate),
    lubridate::as_date("2012-01-03")
  )
  
  
  
  #discard 2  days (progression earliest 3 days after startdate)
  tmp <-  filter_progression(mock_prog_table,
                             .start_date = "startdate",
                             .require_radiographic = FALSE,
                             .exclude_pseudoprogression = FALSE,
                             .discard_n_days = 2,
                             .prog_filter_expression = NULL
  )  
  testthat::expect_equal(
    nrow(tmp),
    5
  )
  testthat::expect_equal(
    length(unique(tmp$progressiondate)),
    1
  )
  testthat::expect_equal(
    unique(tmp$progressiondate),
    lubridate::as_date("2012-01-05")
  )  
  
  
  
  
  #require radiographic (none present in mock data)
  tmp <-  filter_progression(mock_prog_table,
                             .start_date = "startdate",
                             .require_radiographic = TRUE,
                             .exclude_pseudoprogression = FALSE,
                             .discard_n_days = 0,
                             .prog_filter_expression = NULL
  )  
  testthat::expect_equal(
    nrow(tmp),
    5
  )
  testthat::expect_equal(
    length(unique(tmp$progressiondate)),
    1
  )
  testthat::expect_equal(
    unique(tmp$progressiondate),
    Inf %>% as.Date #this is because of min(progressiondate, na.rm = TRUE)) - #TODO check this has no impact in calc_rwPFS
  )  
  

  
  #exclude pseudoprogression (none present in mock data)
  tmp <-  filter_progression(mock_prog_table,
                             .start_date = "startdate",
                             .require_radiographic = FALSE,
                             .exclude_pseudoprogression = TRUE,
                             .discard_n_days = 0,
                             .prog_filter_expression = NULL
  )  
  testthat::expect_equal(
    nrow(tmp),
    5
  )
  testthat::expect_equal(
    length(unique(tmp$progressiondate)),
    1
  )
  testthat::expect_equal(
    unique(tmp$progressiondate),
    Inf %>% as.Date #this is because of min(progressiondate, na.rm = TRUE)) - #TODO check this has no impact in calc_rwPFS
  )    
  
 
  
  #exclude pseudoprogression via .prog_filter_expression (none present in mock data)
  tmp <-  filter_progression(mock_prog_table,
                             .start_date = "startdate",
                             .require_radiographic = FALSE,
                             .exclude_pseudoprogression = FALSE,
                             .discard_n_days = 0,
                             .prog_filter_expression = "ispseudoprogressionmentioned=='No'"
  )  
  testthat::expect_equal(
    nrow(tmp),
    5
  )
  testthat::expect_equal(
    length(unique(tmp$progressiondate)),
    1
  )
  testthat::expect_equal(
    unique(tmp$progressiondate),
    Inf %>% as.Date #this is because of min(progressiondate, na.rm = TRUE)) - #TODO check this has no impact in calc_rwPFS
  )    
  
  
  
  
})




