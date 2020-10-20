library(tidyr)
library(dplyr)

survey <- read.csv("respondents.csv", header = F)
names(survey) <-
  c(
    "week",
    "state",
    "county",
    "fips",
    "biden",
    "trump",
    "clinton_biden",
    "trump_trump",
    "clinton_trump",
    "trump_biden",
    "ethnicity",
    "age_range",
    "gender",
    "county_clinton",
    "county_trump",
    "county_pop"
  )

covid_wide <-
  read.csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
covid_long <- pivot_longer(covid_wide, starts_with("X"), names_to = "date")
covid_long$date <- as.Date(covid_long$date, format = "X%m.%d.%y")
covid_long <- covid_long[!is.na(covid_long$FIPS),]
covid_long <- covid_long[order(covid_long$FIPS, covid_long$date),]
covid_long[["new_cases"]] <-
  c(0, covid_long$value[-1] - covid_long$value[1:length(covid_long$value) - 1])
covid_long[covid_long$date == "2020-01-22", "new_cases"] <- 0
covid_long[["week"]] <- format(covid_long$date, "%Y-%U")
covid <-
  covid_long %>% 
  group_by(FIPS, week) %>% 
  summarize(cases = sum(new_cases), .groups = "keep")


dta <-
  merge(survey,
        covid,
        by.x = c("fips", "week"),
        by.y = c("FIPS", "week"))

dta <- dta[!dta$week %in% c("2020-02", strftime(Sys.Date(), format = "%Y-%U")),]
dta[["weekn"]] <- as.numeric(substr(dta$week, 6, 7))

write.csv(dta, file = "respondents_covid.csv")


init <- lm(clinton_trump ~ cases + weekn, data = dta)
summary(init)


full <- lm(clinton_trump ~ cases + weekn + gender + age_range + ethnicity, data = dta)
summary(full)
