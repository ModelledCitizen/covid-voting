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


tb.m1 <- lm(trump_biden ~ cases, data = dta)
tb.m2 <- lm(trump_biden ~ cases + weekn, data = dta)
tb.m3 <-
  lm(trump_biden ~ cases + weekn + gender + age + ethnicity, data = dta)
tb.m4 <-
  lm(trump_biden ~ cases + weekn + gender + age + ethnicity + county_trump,
     data = dta)

huxreg(tb.m1, tb.m2, tb.m3, tb.m4)


ct.m1 <- lm(clinton_trump ~ cases, data = dta)
ct.m2 <- lm(clinton_trump ~ cases + weekn, data = dta)
ct.m3 <-
  lm(clinton_trump ~ cases + weekn + gender + age + ethnicity, data = dta)
ct.m4 <-
  lm(clinton_trump ~ cases + weekn + gender + age + ethnicity + county_trump,
     data = dta)

huxreg(ct.m1, ct.m2, ct.m3, ct.m4)



### Variant Model (Without County Margin for Trump)

```{r variant}
nocovid <- b.m6$model
nocovid$propcases <- 0

locovid <- b.m6$model
locovid$propcases <- summary(dta$propcases)[2] # 25th percentile

hicovid <- b.m6$model
hicovid$propcases <- summary(dta$propcases)[5] # 75th percentile

maxcovid <- b.m6$model
maxcovid$propcases <- max(dta$propcases)

mikecovid <- b.m6$model
mikecovid$propcases <- 3

b.vec <-
  c(sum(predict(
    b.m6, newdata = nocovid, type = "response"
  )),
  sum(predict(
    b.m6, newdata = locovid, type = "response"
  )),
  sum(predict(b.m6, type = "response")),
  sum(dta$biden),
  sum(predict(
    b.m6, newdata = hicovid, type = "response"
  )),
  sum(predict(
    b.m6, newdata = maxcovid, type = "response"
  )),
  sum(predict(
    b.m6, newdata = mikecovid, type = "response"
  )))



# these matrices are idential to above except for the first column
# and the first column is ignored by predict()
nocovid <- t.m6$model
nocovid$propcases <- 0

locovid <- t.m6$model
locovid$propcases <- summary(dta$propcases)[2] # 25th percentile

hicovid <- t.m6$model
hicovid$propcases <- summary(dta$propcases)[5] # 75th percentile

maxcovid <- t.m6$model
maxcovid$propcases <- max(dta$propcases)

mikecovid <- t.m6$model
mikecovid$propcases <- 3


t.vec <-
  c(sum(predict(
    t.m6, newdata = nocovid, type = "response"
  )),
  sum(predict(
    t.m6, newdata = locovid, type = "response"
  )),
  sum(predict(t.m6, type = "response")),
  sum(dta$trump),
  sum(predict(
    t.m6, newdata = hicovid, type = "response"
  )),
  sum(predict(
    t.m6, newdata = maxcovid, type = "response"
  )),
  sum(predict(
    t.m6, newdata = mikecovid, type = "response"
  )))

out <- data.frame(
  scenario = c("Zero", "Low", "As-is","Among Respondents",  "High", "Max", "Arbitrary (3)"),
  biden = b.vec,
  trump = t.vec
)
out$diff <- out$biden - out$trump

kable(out)
```


