context("calc_rwPFS")

test_that("calc_rwPFS validations", {
  
  require(dplyr)
  
  # 10 patients for Missing/Progression/Death/Censored
  mock_prog_table <- tibble::tribble(
    ~startdate, ~visit_gap_start_date, ~last_progression_abstraction_date, ~progression_date, ~last_activity_date, ~death_date,
    "2020-12-03", NA, "2021-03-10", NA, "2021-04-23", NA,
    "2021-01-25", NA, "2021-04-30", NA, "2021-04-26", NA,
    "2012-09-26", "2013-04-25", "2015-03-27", "2014-06-14", "2015-03-27", "2015-04-15",
    "2016-12-27", "2017-12-15", "2019-08-13", NA, "2019-08-13", "2019-08-15",
    "2013-05-13", "2013-09-18", "2013-12-18", NA, "2013-12-18", "2014-01-15",
    "2015-01-23", "2015-07-08", "2016-04-25", "2015-11-05", "2016-04-25", "2016-04-15",
    "2020-09-24", NA, "2020-12-14", NA, "2021-03-08", NA,
    "2018-01-09", "2018-05-15", "2020-06-23", "2019-02-26", "2020-06-18", "2020-07-15",
    "2019-10-14", NA, "2020-12-23", NA, "2021-04-23", NA,
    "2017-02-06", NA, "2018-11-06", NA, "2018-11-07", "2019-03-15",
    "2016-02-17", NA, "2016-03-16", NA, "2016-03-16", "2016-03-15",
    "2016-05-09", NA, "2016-12-02", NA, "2016-12-19", "2016-12-15",
    "2019-08-26", NA, "2019-10-24", NA, "2019-10-11", "2019-10-15",
    "2012-12-05", NA, "2013-01-29", "2013-01-28", "2013-01-23", "2013-02-15",
    "2018-09-10", NA, "2018-10-05", NA, "2018-10-01", "2018-10-15",
    "2014-04-07", NA, "2014-06-23", NA, "2014-06-23", "2014-08-15",
    "2019-10-31", NA, "2019-11-29", NA, "2019-11-29", "2019-12-15",
    "2015-09-24", NA, "2015-11-18", NA, "2015-11-18", "2016-02-15",
    "2013-12-30", NA, "2014-07-01", NA, "2014-07-17", "2014-07-15",
    "2018-10-09", NA, "2019-03-20", NA, "2019-03-20", "2019-06-15",
    "2019-04-09", NA, "2021-01-04", "2020-02-10", "2020-12-08", "2021-01-15",
    "2012-06-21", "2013-08-19", "2014-03-11", "2013-02-18", "2014-03-11", "2014-03-15",
    "2016-04-28", NA, "2017-07-06", "2016-08-22", "2017-07-06", "2017-08-15",
    "2017-01-04", NA, "2020-12-14", "2018-08-06", "2021-04-19", NA,
    "2019-09-13", "2019-12-02", "2020-12-04", "2019-12-02", "2021-04-30", NA,
    "2015-10-28", NA, "2016-01-29", "2016-01-05", "2016-01-06", "2016-04-15",
    "2019-09-25", NA, "2020-06-18", "2019-12-05", "2020-06-18", "2020-07-15",
    "2014-01-16", NA, "2017-02-02", "2014-09-29", "2017-02-02", "2017-02-15",
    "2016-09-20", "2017-05-17", "2018-06-14", "2017-05-15", "2018-06-14", "2018-07-15",
    "2015-06-03", NA, "2015-11-03", "2015-10-25", "2015-11-16", "2015-12-15",
    "2011-08-17", NA, "2011-08-21", NA, "2011-08-18", "2011-08-15",
    "2015-03-19", NA, "2015-03-26", NA, "2015-03-26", "2015-03-15",
    "2014-09-15", NA, "2014-09-26", NA, "2014-09-22", "2014-09-15",
    "2014-04-15", NA, "2014-04-25", NA, "2014-04-25", "2014-04-15",
    "2020-07-23", NA, "2020-07-27", NA, "2020-07-24", "2020-07-15",
    "2015-10-15", NA, "2015-10-25", NA, "2015-10-22", "2015-10-15",
    "2018-01-15", NA, "2018-01-22", NA, "2018-01-29", "2018-01-15",
    "2017-05-20", NA, "2017-05-24", NA, "2018-05-08", "2017-05-15",
    "2017-04-17", NA, "2017-04-21", NA, "2017-04-21", "2017-04-15",
    "2014-10-20", NA, "2014-10-29", NA, "2014-10-20", "2014-10-15",
  )

  mock_prog_table <- tibble::as_tibble(lapply(mock_prog_table, as.Date))

  mock_prog_table[["patientid"]] <- paste0("patientid", 1:40)

  label_50 <- "_50"
  df_rwPFS_50 <- calc_rwPFS(mock_prog_table,
    .start_date = "startdate",
    .visit_gap_start_date = "visit_gap_start_date",
    .last_progression_abstraction_date = "last_progression_abstraction_date",
    .progression_date = "progression_date",
    .last_activity_date = "last_activity_date",
    .death_date = "death_date",
    .death_window_days = 100,
    .label = label_50
  )

  label_100 <- "_100"
  df_rwPFS_100 <- calc_rwPFS(df_rwPFS_50,
    .start_date = "startdate",
    .visit_gap_start_date = "visit_gap_start_date",
    .last_progression_abstraction_date = "last_progression_abstraction_date",
    .progression_date = "progression_date",
    .last_activity_date = "last_activity_date",
    .death_date = "death_date",
    .death_window_days = 100,
    .label = label_100
  ) %>% # remove Missing records from all *_event_type columns
    dplyr::filter_at(vars(ends_with("_event_type")), all_vars(. != "Missing"))

  df <- compare_rwPFS(df_rwPFS_100,
    .labels = c(label_50, label_100),
    .reference = label_50,
    .incremental_deaths_column = TRUE
  )

  # rwPFS column entries correspond to labels of rwPFS definitions, in order of increasing no. of days
  expected_rwPFS_definition <- c("50", "100")
  testthat::expect_equal(expected_rwPFS_definition, df$`rwPFS definition`)

  # validate Censoring Progression Death for lable 50
  testthat::expect_equal(
    nrow(dplyr::filter(df_rwPFS_50, rwPFS_50_event_type == "Censored")),
    dplyr::filter(df, `rwPFS definition` == "50")$Censoring
  )

  testthat::expect_equal(
    nrow(dplyr::filter(df_rwPFS_50, rwPFS_50_event_type == "Progression")),
    dplyr::filter(df, `rwPFS definition` == "50")$Progression
  )

  testthat::expect_equal(
    nrow(dplyr::filter(df_rwPFS_50, rwPFS_50_event_type == "Death")),
    dplyr::filter(df, `rwPFS definition` == "50")$Death
  )

  # validate Censoring Progression Death for lable 100
  testthat::expect_equal(
    nrow(dplyr::filter(df_rwPFS_100, rwPFS_100_event_type == "Censored")),
    dplyr::filter(df, `rwPFS definition` == "100")$Censoring
  )

  testthat::expect_equal(
    nrow(dplyr::filter(df_rwPFS_100, rwPFS_100_event_type == "Progression")),
    dplyr::filter(df, `rwPFS definition` == "100")$Progression
  )

  testthat::expect_equal(
    nrow(dplyr::filter(df_rwPFS_100, rwPFS_100_event_type == "Death")),
    dplyr::filter(df, `rwPFS definition` == "100")$Death
  )

  # sum of censoring, progression and death events must add up to total number of patients in dataset (always same number)
  # 60 = for rwPFS_definition_50:30 and rwPFS_definition_100:30 (excluding missing)
  testthat::expect_equal(
    sum(df$Censoring, df$Progression, df$Death),
    60
  )

  # incremental deaths must be deaths in the previous row mins deaths in current row
  testthat::expect_equal(
    df$`Incremental deaths`,
    c(NA, diff(df$Death))
  )

  # percent death in every row must be equal to the proportion of death events among progression+death events
  testthat::expect_equal(
    round(df$Death * 100 / (df$Death + df$Progression), 1),
    round(df$`Percent death`, 1)
  )
})

# see vignette for context https://github.roche.com/bretscm2/rwPFS4Rshowcase


# example output (from vignette)
# rwPFS definition	Censoring	Progression	Death	Incremental deaths	Percent death	KM median	Hazard ratio
# 0d window	4045	3310	164	NA	4.7	7.62 (7.39-7.89)	0.8 (0.76-0.83)
# 10d window	3766	3310	443	279	11.8	7.26 (7.03-7.46)	0.86 (0.83-0.9)
# 20d window	3423	3310	786	343	19.2	6.77 (6.54-6.97)	0.94 (0.9-0.98)
# 30d window	3123	3310	1086	300	24.7	6.34 (6.14-6.57)	1 (reference)
# 40d window	2916	3310	1293	207	28.1	6.11 (5.95-6.24)	1.04 (1-1.08)
# 50d window	2770	3310	1439	146	30.3	5.98 (5.75-6.14)	1.07 (1.02-1.11)
# 60d window	2667	3310	1542	103	31.8	5.88 (5.65-6.05)	1.09 (1.04-1.13)
# 70d window	2596	3310	1613	71	32.8	5.82 (5.59-5.98)	1.1 (1.05-1.14)
# 80d window	2531	3310	1678	65	33.6	5.75 (5.55-5.95)	1.11 (1.06-1.15)
# 90d window	2486	3310	1723	45	34.2	5.72 (5.52-5.95)	1.11 (1.07-1.16)


# TODO create a number of different rwPFS endpoints, using calc_rwPFS, with different definitions (different .death_window_days)

# TODO check numbers in table satisfy the following constraints:

# rwPFS column entries correspond to labels of rwPFS definitions, in order of increasing no. of days

# sum of censoring, progression and death events must add up to total number of patients in dataset (always same number)

# incrememntal deaths must be deaths in the previous row mins deaths in current row

# percent death in every row must be equal to the proportion of death events among progression+death events

# KM median?
# hazard ratio ?

# TODO
