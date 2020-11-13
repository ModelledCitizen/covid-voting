require(tidyr)
require(dplyr)
require(RCurl)
require(jsonlite)
require(stringr)


# COVID -------------------------------------------------------------------

parse_jhu <- function(cwo) {
  cwn <- cbind(
    FIPS = cwo$FIPS, 
    cwo[, grep("X(0-9)*", names(cwo))[-1]] - cwo[, grep("X(0-9)*", names(cwo))[-1] - 1]
  )
  clo <- pivot_longer(cwo, starts_with("X"), names_to = "date", values_to = "total_cases")
  cln <- pivot_longer(cwn, starts_with("X"), names_to = "date", values_to = "new_cases")
  cl <- merge(clo, cln)
  cl$date <- as.Date(cl$date, format = "X%m.%d.%y")
  cl <- cl[!is.na(cl$FIPS), ]
  cl$new_cases[cl$new_cases < 0] <- 0
  cl
}

read.csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
) %>%
  parse_jhu() -> cl


# PREZ --------------------------------------------------------------------

parse_nyt <- function(jsn) {
  dta <- fromJSON(jsn)[["data"]][["races"]][["counties"]]
  for (state in dta) {
    state <- format(state)
    if(!exists("name_check")){
      name_all <- names(state)
      name_check <- length(names(state))
    } else if (length(names(state)) > name_check) {
      name_all<- names(state)
      name_check <- length(names(state))
    }
  }
  out <- data.frame()
  for (state in dta) {
    temp <- format(state)
    for (col in name_all) {
      if (!col %in% names(temp)) {
        temp <- cbind(temp, "REPLACE" = vector("numeric", nrow(temp)))
        names(temp)[names(temp) == "REPLACE"] <- col
      }
    }
    temp <- temp[,name_all]
    for (i in grep("results", names(temp), fixed = T, value = T)) {
      temp[[i]] <- as.numeric(temp[[i]])
    }
    temp[["votes"]] <- as.numeric(temp[["votes"]])
    temp[["fips"]] <- as.numeric(temp[["fips"]])
    out <- rbind(out, temp)
  }
  out
}

getURL(
  'https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json'
) %>%
  parse_nyt() %>%
  select(fips,
         name,
         votes,
         reporting,
         contains("results")) %>%
  transmute(
    fips,
    st_fips = substr(fips, 1, 2),
    county_name = name,
    votes_2020_all = votes,
    votes_2020_biden = results.bidenj,
    votes_2020_trump = results.trumpd,
    votes_2020_jorgensen = results.jorgensenj,
    votes_2020_hawkins = results.hawkinsh
  ) -> pr

getURL(
  'https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json'
) %>%
  parse_nyt() %>%
  select(
    contains("results") &
      !contains("absentee") &
      !contains("bidenj") &
      !contains("trumpd") &
      !contains("jorgensenj") &
      !contains("hawkinsh")
  ) %>%
  rowSums(na.rm = TRUE) -> pr$votes_2020_other


# COUNTY ------------------------------------------------------------------

cn <- read.csv("counties.csv")
#cn$fips_code <- str_pad(cn$fips_code, 5, "left", "0")


# MERGE -------------------------------------------------------------------

da <- inner_join(cn, pr, by = c("fips_code" = "fips"))
da <- inner_join(da, cl, by = c("fips_code" = "FIPS"))

da[["prop_cases"]] <- da$total_cases / (da$pop_2017_18plus/1000)
da[["prop_new_cases"]] <- da$new_cases / (da$pop_2017_18plus/1000)

da[["dem_diff_16_20"]] <-
  (da$votes_2020_biden / da$votes_2020_all) - (da$votes_2016_clinton / da$votes_2016_all)
da[["rep_diff_16_20"]] <-
  (da$votes_2020_trump / da$votes_2020_all) - (da$votes_2016_trump / da$votes_2016_all)


de <- da[da$date == "2020-11-03",]


# MODEL -------------------------------------------------------------------

# Did COVID have a statistically significant effect on support for Biden?
biden <- list()

biden[["m1"]] <- lm(I(votes_2020_biden / votes_2020_all) ~ prop_cases + I(votes_2016_clinton / votes_2016_all), data = de)
summary(biden[["m1"]])

biden[["m2"]] <- lm(I(votes_2020_biden / votes_2020_all) ~ prop_cases + I(votes_2016_clinton / votes_2016_all) + cdc_urbanicity, data = de)
summary(biden[["m2"]])

biden[["m3"]] <- lm(dem_diff_16_20 ~ prop_cases, data = de)
summary(biden[["m3"]])


# Did COVID have a statistically significant effect on support for Trump?
trump <- list()
trump[["m1"]] <- lm(I(votes_2020_trump / votes_2020_all) ~ prop_cases + I(votes_2016_trump / votes_2016_all), data = de)
summary(trump[["m1"]])


# Did COVID have a statistically significant effect on turnout?
turnout <- list()
turnout[["m1"]] <- lm(votes_2020_all ~ total_cases + votes_2016_all, data = de)
summary(turnout[["m1"]])
