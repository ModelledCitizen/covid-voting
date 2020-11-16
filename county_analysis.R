require(tidyr)
require(dplyr)
require(RCurl)
require(jsonlite)
require(stringr)
require(ggplot2)


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

de <- da[da$date == "2020-11-03",]

de[["log_cases"]] <- ifelse(de$total_cases > 0, log(de$total_cases), 0)
de[["prop_log_cases"]] <- de$log_cases / (de$pop_2017_18plus/1000)

de[["prop_cases"]] <- de$total_cases / (de$pop_2017_18plus/1000)
de[["prop_new_cases"]] <- de$new_cases / (de$pop_2017_18plus/1000)

de[["dem_share_16"]] <- (de$votes_2016_clinton / de$votes_2016_all)
de[["dem_share_20"]] <- (de$votes_2020_biden / de$votes_2020_all)
de[["dem_diff_16_20"]] <-de$dem_share_20 - de$dem_share_16

de[["rep_share_16"]] <- (de$votes_2016_trump / de$votes_2016_all)
de[["rep_share_20"]] <- (de$votes_2020_trump / de$votes_2020_all)
de[["rep_diff_16_20"]] <-de$rep_share_20 - de$rep_share_16


de[["dem_margin_16"]] <- de$dem_share_16 - de$rep_share_16
de[["dem_margin_20"]] <- de$dem_share_20 - de$rep_share_20
de[["dem_margin_diff"]] <- de$dem_margin_20 - de$dem_margin_16

de[["covid_binary"]] <-
  ifelse(de$total_cases > median(de$total_cases), "hi", "lo")

de[["covid_categorical"]] <-
  cut(
    de$prop_cases,
    c(
      min(de$prop_cases),
      quantile(de$prop_cases, 0.2),
      quantile(de$prop_cases, 0.4),
      quantile(de$prop_cases, 0.6),
      quantile(de$prop_cases, 0.8),
      max(de$prop_cases) + 1
    ),
    c("Bottom 20%", "Low 20%", "Middle 20%", "High 20%", "Top 20%"),
    right = FALSE
  )

de[["margin_categorical"]] <-
  ifelse(de$dem_margin_20 > de$dem_margin_16,
         "more_biden",
         "more_clinton")
de$margin_categorical[de$dem_margin_20 == de$dem_margin_16] <- "equal"




# VISUALIZE ---------------------------------------------------------------



prop.table(table(de$cdc_urbanicity, de$margin_categorical), margin = 1)

plot(de$prop_cases[de$state_code == "55"], de$dem_diff_16_20[de$state_code == "55"])

plot(de$prop_cases[de$state_code == "55"], (de$votes_2016_all[de$state_code == "55"] - de$votes_2020_all[de$state_code == "55"])/de$votes_2016_all[de$state_code == "55"])



plot(de$prop_cases, (de$votes_2016_all - de$votes_2020_all)/de$votes_2016_all)



plot(x = de$rep_share_16, de$prop_cases)


plot(de$prop_cases,
     de$dem_margin_diff,
     xlab = "COVID Cases / Population",
     ylab = "Dem. Margin Diff. 16-20",
     pch = ".")
abline(lm(dem_margin_diff ~ prop_cases, data = de))



ggplot(aes(x = dem_margin_16, 
           y = dem_margin_20, 
           size = prop_cases,
           color = covid_binary), 
       data = de[de$st_fips == "55",]) +
  geom_abline() +
  geom_point()


pdf("urbanicity_margin.pdf", height = 8.5, width = 11)
ggplot(aes(x = dem_margin_16, 
           y = dem_margin_20, 
           size = pop_2017_18plus,
           color = cdc_urbanicity), 
       data = de) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ cdc_urbanicity, ncol = 3)
dev.off()


options(scipen = 999)
pdf("covid_urbanicity_margin.pdf", width = 11, height = 8.5)
ggplot(aes(x = dem_margin_16, 
           y = dem_margin_20, 
           size = pop_2017_18plus,
           color = cdc_urbanicity), 
       data = de) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ covid_categorical, ncol = 3) +
  labs(title = "Democratic Margin in Counties by COVID Intensity as of E-Day",
       color = "Urbanicity",
       size = "County Pop.") +
  xlab("Dem. Margin 2016") +
  ylab("Dem. Margin 2020")
dev.off()

# MODEL -------------------------------------------------------------------

# Did COVID have a statistically significant effect on support for Biden?
# H0: COVID cases have no effect on vote share for the Democrat.
# H1: COVID cases have an effect on vote share for the Democrat.

# We use a Welch Two Sample t-test to observe whether there is a difference in 
# counties with COVID rates above the median vs. below the median.
# Our outcome is the difference between democratic vote share in 2016 and 2020.

t.test(de[de$prop_cases < 35, "dem_diff_16_20"], de[de$prop_cases >= 35, "dem_diff_16_20"])

# This doesn't tell us whether the difference is caused by COVID, though.
# We need to include other potential explanations to rule them out.

biden <- list()

biden[["m1"]] <- lm(
  dem_share_20 ~ prop_cases + dem_share_16, 
  data = de
)
summary(biden[["m1"]])

biden[["m2"]] <- lm(
  dem_share_20 ~ prop_cases + dem_share_16 + cdc_urbanicity, 
  data = de
)
summary(biden[["m2"]])

biden[["m3"]] <- lm(
  dem_diff_16_20 ~ prop_cases, 
  data = de
)
summary(biden[["m3"]])

biden[["m4"]] <- lm(
  dem_diff_16_20 ~ prop_cases + dem_share_16, 
  data = de
)
summary(biden[["m4"]])

biden[["m5"]] <- lm(
  dem_diff_16_20 ~ prop_log_cases + dem_share_16, 
  data = de
)
summary(biden[["m5"]])


biden[["m6"]] <- lm(
  dem_margin_diff ~ prop_log_cases + dem_share_16, 
  data = de
)
summary(biden[["m6"]])



# Did COVID have a statistically significant effect on support for Trump?
trump <- list()
trump[["m1"]] <- lm(
  rep_share_20 ~ prop_cases + rep_share_16, 
  data = de
)
summary(trump[["m1"]])


# Did COVID have a statistically significant effect on turnout?
turnout <- list()
turnout[["m1"]] <- lm(
  votes_2020_all ~ total_cases + votes_2016_all, 
  data = de
)
summary(turnout[["m1"]])

