
test_that("calc_rwPFS validations", {
  
  mock_data <- tibble::tribble(
    ~patientid,   ~last_activity_date, ~death_date, ~start_date, ~visit_gap_start_date, ~last_progression_abstraction_date, ~progression_date, 
    "patient1+",  as.Date("2016-02-04"), NA, as.Date("2015-07-16"), as.Date("2015-03-16"), NA, as.Date("2015-04-12"),
    "patient2+",  as.Date("2015-08-14"), as.Date("2015-09-15"), as.Date("2014-05-19"), NA, as.Date("2017-03-08"), as.Date("2017-03-03"),
    "patient3+",  as.Date("2020-02-12"), as.Date("2020-02-12"), as.Date("2020-01-29"), as.Date("2020-02-05"), as.Date("2020-02-12"), as.Date("2020-02-12"),
    "patient4+",  as.Date("2021-04-27"), NA, NA, NA, NA, NA,
    "patient5+",  as.Date("2020-01-13"), as.Date("2020-01-15"), as.Date("2018-07-16"), NA, NA, NA,
    "patient6+",  as.Date("2016-06-15"), as.Date("2016-12-15"), NA, NA, NA, NA,
    "patient7+",  as.Date("2021-04-27"), NA, NA, NA, NA, NA,
    "patient8+",  as.Date("2019-02-07"), as.Date("2019-02-15"), NA, NA, NA, NA,
    "patient9+",  as.Date("2011-05-20"), as.Date("2015-05-15"), NA, NA, NA, NA,
    "patient10+", as.Date("2018-01-22"), NA, as.Date("2018-01-08"), as.Date("2018-01-22"), as.Date("2018-01-22"), as.Date("2018-01-15"),
    "patient11+", as.Date("2020-02-12"), as.Date("2020-02-12"), as.Date("2020-01-29"), as.Date("2020-02-12"), as.Date("2020-02-12"), as.Date("2020-02-05"),
    "patient12+", as.Date("2018-02-12"), as.Date("2018-01-15"), as.Date("2018-01-08"), NA, as.Date("2018-01-15"), as.Date("2018-01-22"),
    "patient13+", as.Date("2020-12-30"), NA, NA, NA, NA, NA,
    "patient14+", as.Date("2019-01-15"), as.Date("2019-01-15"), as.Date("2019-01-08"), as.Date("2019-01-15"), as.Date("2019-01-15"), as.Date("2019-01-22"),
    "patient15+", as.Date("2015-08-14"), as.Date("2015-09-10"), as.Date("2014-05-19"), NA, as.Date("2017-03-08"), as.Date("2017-03-03"),
  )
  
  
  mock_result <- calc_rwPFS(
    mock_data,
    .start_date = "start_date",
    .visit_gap_start_date = "visit_gap_start_date",
    .last_progression_abstraction_date = "last_progression_abstraction_date",
    .progression_date = "progression_date",
    .last_activity_date = "last_activity_date",
    .death_date = "death_date",
    .death_window_days = 30,
    .label = "_testing"
    
  )
  
  #If progression_date is <= eof_date, then rwPFS_date is equal to progression date, and event_type is "Progression" and event == 1
  #days/months is progression_date minus start_date
  
 mock_result_subset <- mock_result %>%
    dplyr::filter((progression_date <= rwPFS_testing_eof_date))
  
  expected <- c("patient10+", "patient11+")
  testthat::expect_equal(expected, mock_result_subset$patientid)
  
  testthat::expect_true(
    mock_result %>%
      dplyr::filter((progression_date <= rwPFS_testing_eof_date)) %>%
      {
        all(.$rwPFS_testing_date == .$progression_date) &
          all(.$rwPFS_testing_event_type == "Progression") &
          all(.$rwPFS_testing_event == 1) &
          all(.$rwPFS_testing_days == .$progression_date - .$start_date)
      },
    label = "'If progression_date is <= eof_date, then rwPFS_date is equal to progression date, and event_type is 'Progression' and event == 1 days/months is progression_date minus start_date'"
  )
  
  #else if there's no progression <= eof_date, but there's a non-missing death date within <30d after eof_date, then rwPFS date
  #must be equal to death date, event_type must be "Death", and event == 1, days/months is death_date minus start_date
  
  mock_result_subset<- mock_result %>%
    dplyr::filter((progression_date > rwPFS_testing_eof_date) &
                    (death_date - rwPFS_testing_eof_date) < 30 &
                    (rwPFS_testing_event_type == "Death")
    )
  
  expected <- c("patient3+", "patient12+", "patient14+", "patient15+")
  testthat::expect_equal(expected, mock_result_subset$patientid)
  
  testthat::expect_true(
    mock_result %>%
      dplyr::filter((progression_date > rwPFS_testing_eof_date) &
                      !(is.na(death_date)) &
                      ((death_date - rwPFS_testing_eof_date) < 30)
                    ) %>%
      {
        all(.$rwPFS_testing_date == .$death_date) &
          all(.$rwPFS_testing_event_type == "Death") &
          all(.$rwPFS_testing_event == 1) &
          all(.$rwPFS_testing_days == .$death_date - .$start_date)
      },
    label = "'If there's no progression <= eof_date, but there's a non-missing death date within <30d after eof_date, then rwPFS date
  must be equal to death date, event_type must be 'Death', and event == 1, days/months is death_date minus start_date'"
  )
  
  
  #If there's no progression <= eof_date and non-missing death date within <30d after eof_date, then rwPFS_event_type is "censored"
  #rwPFS_event == 0 , and days/months is eof_date - minus start_date
  
  mock_result_subset<- mock_result %>%
    dplyr::filter(progression_date > rwPFS_testing_eof_date &
                  death_date - rwPFS_testing_eof_date >= 30 &
                  rwPFS_testing_event_type == "Censored"
    )
  
  expected <- c("patient2+")
  testthat::expect_equal(expected, mock_result_subset$patientid)
  
  testthat::expect_true(
    mock_result %>%
      dplyr::filter((progression_date > rwPFS_testing_eof_date) &
                      !(is.na(death_date)) &
                      ((death_date - rwPFS_testing_eof_date) > 30)
      ) %>%
      {
          all(.$rwPFS_testing_event_type == "Censored") &
          all(.$rwPFS_testing_event == 0) &
          all(.$rwPFS_testing_days == .$rwPFS_testing_eof_date - .$start_date)
      },
    label = "'If there's no progression <= eof_date and non-missing death date within <30d after eof_date, then rwPFS_event_type is 'censored'
    rwPFS_event == 0 , and days/months is eof_date - minus start_date'"
  )
  
})
