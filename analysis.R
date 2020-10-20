setwd("~/Desktop/covid project")
library(tidyr)
library(dplyr)

survey <- read.csv("county_counts.csv", header = F)
names(survey) <-
  c(
    "month",
    "state",
    "county",
    "fips",
    "clinton_biden",
    "trump_trump",
    "clinton_trump",
    "trump_biden"
  )

covid_wide <- read.csv("time_series_covid19_confirmed_US.csv")
covid_long <- pivot_longer(covid_wide, starts_with("X"), names_to = "date")
covid_long$date <- as.Date(covid_long$date, format = "X%m.%d.%y")
covid_long <- covid_long[!is.na(covid_long$FIPS),]
covid_long <- covid_long[order(covid_long$FIPS, covid_long$date),]
covid_long[["new_cases"]] <-
  c(0, covid_long$value[-1] - covid_long$value[1:length(covid_long$value) - 1])
covid_long[covid_long$date == "2020-01-22", "new_cases"] <- 0
covid_long[["month"]] <- format(covid_long$date, "%Y-%m")
covid <-
  covid_long %>% group_by(FIPS, month) %>% summarize(cases = sum(new_cases))


dta <-
  merge(survey,
        covid,
        by.x = c("fips", "month"),
        by.y = c("FIPS", "month"))
dta <- dta[!dta$month %in% c("2020-01", "2020-10"),]
