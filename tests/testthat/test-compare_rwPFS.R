test_that("multiplication works", {
  expect_equal(2 * 2, 4)

})


#see vignette for context https://github.roche.com/bretscm2/rwPFS4Rshowcase


#example output (from vignette)
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


#TODO create a number of different rwPFS endpoints, using calc_rwPFS, with different definitions (different .death_window_days)

#TODO check numbers in table satisfy the following constraints:

#rwPFS column entries correspond to labels of rwPFS definitions, in order of increasing no. of days

# sum of censoring, progression and death events must add up to total number of patients in dataset (always same number)

# incrememntal deaths must be deaths in the previous row mins deaths in current row

# percent death in every row must be equal to the proportion of death events among progression+death events

#KM median?
#hazard ratio ? 

#TODO 
