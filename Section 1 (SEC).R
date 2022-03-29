require(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
polling <- subset(polls, polls$candidate_party == "REP")
approval_polling <- merge(polling,approval,by="end_date")

approval_polling$end_date <- as.Date(approval_polling$end_date, format= "%Y-%m-%d")
approval_polling <- subset(approval_polling, end_date > "2020-01-01")
approval_polling <- subset(approval_polling, select = -c(no))
names(approval_polling)[names(approval_polling) == 'pct'] <- "poll_pct"

national_approval_polling <- approval_polling %>% filter(is.na(state))
to_use = subset(national_approval_polling, select = c("end_date","poll_pct", "yes"))

new_by <- aggregate(approval_polling["Polling"], by=approval_polling["end_date"], mean)

to_use <- to_use %>% group_by(end_date = as.Date(end_date)) %>% summarise(across(c(poll_pct, yes), mean))

no_labels_polls <- ggplot(to_use) + 
  geom_line(aes(y=poll_pct, x=end_date), color = "gray20") +
  stat_smooth(aes(y=poll_pct, x=end_date), method = lm, formula = y ~ poly(x, 22), se = FALSE) +
  theme_light() +
  scale_x_date(
    name = "Date",
    breaks = waiver(),
    date_breaks = "1 month",
    labels = waiver(),
    date_labels = "%B",
    minor_breaks = waiver(),
    date_minor_breaks = waiver(),
    expand = c(0, 0),
    limits = as.Date(c('10/1/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
  scale_y_continuous(breaks = seq(36, 54, 2)) +
  ylab("National Polling Percentage") +
  ggtitle("Trump National Polling Average, 2020") +
  theme(plot.title = element_text(hjust = 0.5))

no_labels_approval <- ggplot(to_use) + 
  geom_line(aes(y=yes, x=end_date), color = "gray20") +
  geom_smooth(aes(y=yes, x=end_date), method ='loess', se = FALSE) +
  theme_light() +
  scale_x_date(
    name = "Date",
    breaks = waiver(),
    date_breaks = "1 month",
    labels = waiver(),
    date_labels = "%B",
    minor_breaks = waiver(),
    date_minor_breaks = waiver(),
    expand = c(0, 0),
    limits = as.Date(c('10/1/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
  scale_y_continuous(breaks = seq(36, 54, 2)) +
  ylab("Trump Approval Rating") +
  ggtitle("Trump Approval Rating, 2020") +
  theme(plot.title = element_text(hjust = 0.5))

labels_polls <- ggplot(to_use) + 
  geom_line(aes(y=poll_pct, x=end_date), color = "gray20") +
  stat_smooth(aes(y=poll_pct, x=end_date), method = lm, formula = y ~ poly(x, 22), se = FALSE) +
  theme_light() +
  scale_x_date(
    name = "Date",
    breaks = waiver(),
    date_breaks = "1 month",
    labels = waiver(),
    date_labels = "%B",
    minor_breaks = waiver(),
    date_minor_breaks = waiver(),
    expand = c(0, 0),
    limits = as.Date(c('10/1/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
  scale_y_continuous(breaks = seq(36, 54, 2)) +
  ylab("National Polling Percentage") +
  ggtitle("Trump National Polling Average, 2020") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-18")), 
             color = "red", 
             lwd = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-12")), 
             color = "forestgreen", 
             lwd = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-09")), 
             color = "purple", 
             lwd = 1) +
  annotate(x=as.Date("2020-05-06"),y=+Inf, color = 'purple', size = 3, label="Daily Deaths in \n US hits 2,000: \n 04/09/2020", vjust=2,geom="label") +
  annotate(x=as.Date("2020-02-20"),y=+Inf, color = 'red', size = 3, label="Date of First \n Confirmed US Case: \n 01/18/2020", vjust=2,geom="label") +
  annotate(x=as.Date("2020-04-21"),y=41, color = 'forestgreen', size = 3, label="Trump Enacts European \n Travel Ban \n 03/12/2020", vjust=2,geom="label")

labels_approval <- ggplot(to_use) + 
  geom_line(aes(y=yes, x=end_date), color = "gray20") +
  geom_smooth(aes(y=yes, x=end_date), method ='loess', se = FALSE) +
  theme_light() +
  scale_x_date(
    name = "Date",
    breaks = waiver(),
    date_breaks = "1 month",
    labels = waiver(),
    date_labels = "%B",
    minor_breaks = waiver(),
    date_minor_breaks = waiver(),
    expand = c(0, 0),
    limits = as.Date(c('10/1/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
  scale_y_continuous(breaks = seq(36, 54, 2)) +
  ylab("Trump Approval Rating") +
  ggtitle("Trump Approval Rating, 2020") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-01-18")), 
             color = "red", 
             lwd = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-12")), 
             color = "forestgreen", 
             lwd = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-09")), 
             color = "purple", 
             lwd = 1) +
  annotate(x=as.Date("2020-05-06"),y=+Inf, color = 'purple', size = 3, label="Daily Deaths in \n US hits 2,000: \n 04/09/2020", vjust=2,geom="label") +
  annotate(x=as.Date("2020-02-20"),y=+Inf, color = 'red', size = 3, label="Date of First \n Confirmed US Case: \n 01/18/2020", vjust=2,geom="label") +
  annotate(x=as.Date("2020-04-21"),y=41, color = 'forestgreen', size = 3, label="Trump Enacts European \n Travel Ban \n 03/12/2020", vjust=2,geom="label")


us_covid <- dplyr::filter(covid2, location=="United States")
us_covid$date <- as.Date(us_covid$date, format= "%Y-%m-%d")
us_covid <- subset(us_covid, date < "2020-11-04")
to_use_cov = subset(us_covid, select = c("end_date","new_cases", "total_cases", "new_deaths", "total_deaths"))
to_use_cov <- to_use_cov %>%
  mutate(new_cases = if_else(is.na(new_cases), 0, new_cases))
to_use_cov <- to_use_cov %>%
  mutate(new_deaths = if_else(is.na(new_deaths), 0, new_deaths))
names(to_use_cov)[names(to_use_cov) == 'date'] <- "end_date"

all_data <- merge(to_use, to_use_cov, by="end_date")
all_data <- subset(all_data, end_date > "2020-02-29")

all_data <- all_data %>% 
  mutate(daily_death_rate = 100 * (total_deaths - lag(total_deaths))/lag(total_deaths)) 
all_data <- all_data %>% 
  mutate(daily_cases_rate = 100 * (total_cases - lag(total_cases))/lag(total_cases)) 

all_data <- all_data %>%
  mutate(rate = if_else(is.na(rate), 0, rate))

all_data <- all_data %>%
  mutate(rate = if_else(is.infinite(rate), 0, rate))

summary(lm(all_data$poll_pct ~ all_data$new_cases))
summary(lm(all_data$poll_pct ~ all_data$new_deaths))


ylim.prim <- c(0, 2350)
ylim.sec <- c(40, 47)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

ggplot(all_data, aes(end_date, new_deaths)) +
  stat_smooth(method = lm, formula = y ~ poly(x, 22), se = FALSE) +
  stat_smooth(aes(y=a + yes*b), method = lm, formula = y ~ poly(x, 22), se = FALSE, color="red") +
  scale_y_continuous("New Deaths", sec.axis = sec_axis(~ (. - a)/b, name = "Approval Rating")) +
  theme_light() +
  scale_x_date(
    name = "Date",
    breaks = waiver(),
    date_breaks = "1 month",
    labels = waiver(),
    date_labels = "%B",
    minor_breaks = waiver(),
    date_minor_breaks = waiver(),
    expand = c(0, 0),
    limits = as.Date(c('01/3/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
  ggtitle("Trump Approval Rating and New Deaths from Covid, 2020") +
  theme(plot.title = element_text(hjust = 0.5))


new_death_graph <- ggplot(all_data) + 
  geom_line(aes(y=new_deaths, x=end_date), color = "gray20") +
  stat_smooth(aes(y=new_deaths, x=end_date), method = lm, formula = y ~ poly(x, 22), se = FALSE) +
  theme_light() +
  scale_x_date(
    name = "Date",
    breaks = waiver(),
    date_breaks = "1 month",
    labels = waiver(),
    date_labels = "%B",
    minor_breaks = waiver(),
    date_minor_breaks = waiver(),
    expand = c(0, 0),
    limits = as.Date(c('10/1/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
    scale_y_continuous(
        name = "First Axis",
        sec.axis = sec_axis( trans=~.*0.02076923, breaks = seq(36, 54, 2), name="Second Axis")) +
  geom_line(aes(y=(yes*48.14815)+36, x=end_date), color = "gray20")
  ) 
new_death_graph

march_june <- subset(all_data, end_date > "2020-04-15")
march_june <- subset(all_data, end_date < "2020-07-15")

summary(lm(march_june$yes ~ march_june$daily_cases_rate))
summary(lm(march_june$yes ~ march_june$daily_death_rate))

2600/54



write.csv(all_data, "~\\all_data.csv")
