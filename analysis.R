# Tiffany Tse
# Workbook 5: analyze NHANES data

# Set up

library(survey)
library(Hmisc)

demo <- sasxport.get("DEMO_I.XPT")
alco <- sasxport.get("ALQ_I.XPT")

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = T)

wt_sum <- sum(nhanes$wtint2yr, na.rm = T)

# wt_sum: total US population

# Part 2: Analysis
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA


# Survey Design
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = T,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

nhances_mean <- svymean(~alq151, nhanes_survey, na.rm = T)

mean_by_gender <- svyby(~alq151, ~riagendr, nhanes_survey, svymean)