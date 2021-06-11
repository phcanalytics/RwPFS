context("filter_progression")

test_that("filter_progression - Validations", {
  
  testthat::expect_error(
    filter_progression(NULL,
                       .start_date = NULL,
                       .require_radiographic = FALSE,
                       .exclude_pseudoprogression = FALSE,
                       .discard_n_days = 0,
                       .prog_filter_expression = NULL
    ),
    "Please specify start_date - the column which corresponds to your index date"
  )
  
  # TODO add other validations
})


test_that("filter_progression - condition lastclinicnotedate > start_date", {
  
  mock_prog_table <- tibble::tribble(
    ~patientid, ~startdate,          ~progressiondate,     ~lastclinicnotedate, ~isradiographicevidence, ~ispseudoprogressionmentioned,
    "patient1a", as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "Yes", "Yes",
    "patient1b", as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "No" , "Yes",
    "patient1c", as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "Yes", "No",
    "patient2a", as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "Yes", "Yes",
    "patient2b", as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"),  NA  , "Yes",
    "patient2c", as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "Yes", "No",
    "patient3",  as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "Yes", "No",
    "patient4",  as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "Yes", "No",
    "patient5",  as.Date("2017-04-25"), as.Date("2017-03-25"), as.Date("2018-05-25"), "Yes", "No",
    "patient6",  as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2017-03-25"), "Yes", "No",
    "patient7",  NA, as.Date("2017-05-25"), as.Date("2017-03-25"), "Yes", "No",
    "patient8",  as.Date("2017-04-25"), as.Date("2017-05-25"), NA, "Yes", "No",
  )
  
  testthat::expect_warning(
    filter_progression(mock_prog_table,
                       .start_date = "startdate",
                       .require_radiographic = FALSE,
                       .exclude_pseudoprogression = FALSE,
                       .discard_n_days = 0,
                       .prog_filter_expression = NULL
    )
  )
  
  df_actual <- filter_progression(mock_prog_table,
                                  .start_date = "startdate",
                                  .require_radiographic = FALSE,
                                  .exclude_pseudoprogression = FALSE,
                                  .discard_n_days = 0,
                                  .prog_filter_expression = NULL
  )
  
  # consider only patients with lastclinicnotedate > start_date are valid
  # patient6 - lastclinicnotedate < start_date
  # patient7 & patient78 - with NA
  df_nas <- df_actual %>%
    dplyr::filter(patientid %in% c("patient6", "patient7", "patient8")) %>%
    dplyr::select(progressiondate, lastclinicnotedate)
  
  testthat::expect_true(all(is.na(df_nas)))
  
  
  # all patients must be returned
  testthat::expect_equal(mock_prog_table$patientid, df_actual$patientid)
  
})

test_that("filter_progression - progressiondate transformation", {
  
  # mock data- start_date between progressiondate
  mock_prog_table <- tibble::tribble(
    ~patientid, ~startdate,              ~progressiondate,     ~lastclinicnotedate, ~isradiographicevidence, ~ispseudoprogressionmentioned,
    "patient1", as.Date("2017-04-25")    , as.Date("2017-04-25"), as.Date("2018-05-25"), "Yes", "Yes",
    "patient2", as.Date("2017-04-25") + 1, as.Date("2017-04-25"), as.Date("2018-05-25"), "Yes", "Yes",
    "patient3", as.Date("2017-04-25") - 1, as.Date("2017-04-25"), as.Date("2018-05-25"), "Yes", "Yes",
  )
  
  df_actual <- filter_progression(mock_prog_table,
                                  .start_date = "startdate",
                                  .require_radiographic = FALSE,
                                  .exclude_pseudoprogression = FALSE,
                                  .discard_n_days = 0,
                                  .prog_filter_expression = NULL
  )
  
  # all patients must be returned
  testthat::expect_equal(mock_prog_table$patientid, df_actual$patientid)
  
  # condition if start_date < progressiondate then NA
  df_nas <- df_actual %>%
    dplyr::filter(patientid %in% c("patient1", "patient2")) %>%
    dplyr::select(progressiondate)
  
  #Valid patient
  testthat::expect_is(df_actual$progressiondate[df_actual$patientid == "patient3"], "Date")
  
  testthat::expect_true(all(is.na(df_nas)))
  
  
  
  # test when .require_radiographic = TRUE
  mock_prog_table <- tibble::tribble(
    ~patientid, ~startdate,          ~progressiondate,     ~lastclinicnotedate, ~isradiographicevidence, ~ispseudoprogressionmentioned,
    "patient1", as.Date("2017-04-25"), as.Date("2017-05-01"), as.Date("2018-05-25"), "Yes", "Yes", # valid
    "patient2", as.Date("2017-04-25"), as.Date("2017-06-15"), as.Date("2018-05-25"), "No", "Yes",  # invalid
    "patient3", as.Date("2017-04-25"), as.Date("2017-07-30"), as.Date("2018-05-25"),  "" , "Yes",  # invalid
    "patient4", as.Date("2017-04-25"), as.Date("2017-07-30"), as.Date("2018-05-25"),  NA , "Yes",  # invalid
  )
  
  df_actual <- filter_progression(mock_prog_table,
                                  .start_date = "startdate",
                                  .require_radiographic = TRUE,
                                  .exclude_pseudoprogression = FALSE,
                                  .discard_n_days = 0,
                                  .prog_filter_expression = NULL
  )
  
  # all patients must be returned
  testthat::expect_equal(mock_prog_table$patientid, df_actual$patientid)
  
  # condition require_radiographic != Yes then NA
  df_nas <- df_actual %>%
    dplyr::filter(patientid %in% c("patient2", "patient3", "patient4")) %>%
    dplyr::select(progressiondate)
  
  testthat::expect_true(all(is.na(df_nas)))
  
  # Valid patient
  testthat::expect_identical(df_actual$progressiondate[df_actual$patientid == "patient1"], as.Date("2017-05-01"))
  
  
  #test when ispseudoprogressionmentioned = TRUE
  mock_prog_table <- tibble::tribble(
    ~patientid, ~startdate,          ~progressiondate,     ~lastclinicnotedate, ~isradiographicevidence, ~ispseudoprogressionmentioned,
    "patient5", as.Date("2017-04-25"), as.Date("2017-05-01"), as.Date("2018-05-25"), "Yes", "No",  # valid
    "patient6", as.Date("2017-04-25"), as.Date("2017-06-15"), as.Date("2018-05-25"), "Yes", "Yes", # invalid
    "patient7", as.Date("2017-04-25"), as.Date("2017-07-30"), as.Date("2018-05-25"), "Yes", "",    # invalid
    "patient8", as.Date("2017-04-25"), as.Date("2017-07-30"), as.Date("2018-05-25"), "Yes", NA,    # invalid
  )
  
  df_actual <- filter_progression(mock_prog_table,
                                  .start_date = "startdate",
                                  .require_radiographic = FALSE,
                                  .exclude_pseudoprogression = TRUE,
                                  .discard_n_days = 0,
                                  .prog_filter_expression = NULL
  )
  
  # all patients must be returned
  testthat::expect_equal(mock_prog_table$patientid, df_actual$patientid)
  
  # condition ispseudoprogressionmentioned != No then NA
  df_nas <- df_actual %>%
    dplyr::filter(patientid %in% c("patient6", "patient7", "patient8")) %>%
    dplyr::select(progressiondate)
  
  testthat::expect_true(all(is.na(df_nas)))
  
  # Valid patient
  testthat::expect_identical(df_actual$progressiondate[df_actual$patientid == "patient5"], as.Date("2017-05-01"))
  
  #test when require_radiographic & ispseudoprogressionmentioned = TRUE
  mock_prog_table <- tibble::tribble(
    ~patientid, ~startdate,          ~progressiondate,     ~lastclinicnotedate, ~isradiographicevidence, ~ispseudoprogressionmentioned,
    "patient1", as.Date("2017-04-25"), as.Date("2017-05-01"), as.Date("2018-05-25"), "Yes", "Yes",  # valid
    "patient2", as.Date("2017-04-25"), as.Date("2017-06-15"), as.Date("2018-05-25"), "No",  "Yes", # invalid
    "patient3", as.Date("2017-04-25"), as.Date("2017-07-30"), as.Date("2018-05-25"), "Yes", "No",    # invalid
    "patient4", as.Date("2017-04-25"), as.Date("2017-07-30"), as.Date("2018-05-25"), "No",  "No",    # invalid
  )
  
  df_actual <- filter_progression(mock_prog_table,
                                  .start_date = "startdate",
                                  .require_radiographic = TRUE,
                                  .exclude_pseudoprogression = TRUE,
                                  .discard_n_days = 0,
                                  .prog_filter_expression = NULL
  )
  
  # all patients must be returned
  testthat::expect_equal(mock_prog_table$patientid, df_actual$patientid)
  
  # condition require_radiographic != Yes then NA
  df_nas <- df_actual %>%
    dplyr::filter(patientid %in% c("patient6", "patient7", "patient8")) %>%
    dplyr::select(progressiondate)
  
  testthat::expect_true(all(is.na(df_nas)))
  
  # Valid patient
  testthat::expect_identical(df_actual$progressiondate[df_actual$patientid == "patient3"], as.Date("2017-07-30"))
  
  #test condition startdate_lag <= .discard_n_days then NA
  mock_prog_table <- tibble::tribble(
    ~patientid, ~startdate,          ~progressiondate,     ~lastclinicnotedate, ~isradiographicevidence, ~ispseudoprogressionmentioned,
    "patient1", as.Date("2017-04-25"), as.Date("2017-05-24"), as.Date("2018-05-25"), "Yes", "No", # 29 lag
    "patient2", as.Date("2017-04-25"), as.Date("2017-05-25"), as.Date("2018-05-25"), "Yes", "No", # 30 lag
    "patient3", as.Date("2017-04-25"), as.Date("2017-06-25"), as.Date("2018-05-24"), "Yes", "No", # 61 lag
    "patient3", as.Date("2017-04-25"), as.Date("2017-06-26"), as.Date("2018-05-24"), "Yes", "No", # 62 lag
  )
  
  df_actual <- filter_progression(mock_prog_table,
                                  .start_date = "startdate",
                                  .require_radiographic = TRUE,
                                  .exclude_pseudoprogression = TRUE,
                                  .discard_n_days = 30,
                                  .prog_filter_expression = NULL
  )
  
  # all patients must be returned
  testthat::expect_equal(unique(mock_prog_table$patientid), df_actual$patientid)
  
  # condition discard_n_days
  df_nas <- df_actual %>%
    dplyr::filter(patientid %in% c("patient1", "patient2")) %>%
    dplyr::select(progressiondate)
  
  testthat::expect_true(all(is.na(df_nas)))
  
  # Valid patient and min
  testthat::expect_identical(df_actual$progressiondate[df_actual$patientid == "patient3"], as.Date("2017-06-25"))
  
})




