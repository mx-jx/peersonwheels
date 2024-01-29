# PREFER project
## Data pull

# Get data

# install.packages("readr")

library(here)
library(readr)
library(readstata13)


ethos <- read.dta13(
  here::here("analysis", "data", "raw_data", "enrollment survey POC merged_final.dta")
)

wave_2 <- read.dta13(
  here::here("analysis", "data", "raw_data", "Wave II complete.dta")
)

wave_complete <- read.dta13(
  here::here("analysis", "data", "raw_data", "W1 W2 complete.dta")
)

enroll <- read.dta13(
  here::here("analysis", "data", "raw_data", "ENR_D.dta")
)

site_state <- read.dta13(
  here::here("analysis", "data", "raw_data", "site state.dta")
)

clean <- read.dta13(
  here::here("analysis", "data", "raw_data", "CM clean.dta")
)


alice <- read.dta13(
  here::here("analysis", "data", "raw_data", "ETHOS Engage_enrolment POC_Wave 1&2.dta")
)


dataset <- read.dta13(
  here::here("analysis", "data", "raw_data", "data 1280 110221.dta")
)

ethos.csv <- read.csv(
  here::here("analysis", "data", "raw_data", "ethos.engage.csv")
)

# ethos<- read_dta("enrollment survey POC merged_final.dta")

View(enroll)
