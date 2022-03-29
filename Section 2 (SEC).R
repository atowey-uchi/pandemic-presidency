require(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(jtools)

approval <- read.csv("~/Downloads/president_approval_polls.csv")
polls <- read.csv("~/Downloads/president_polls.csv")
polls[polls == ""] <- NA
approval[approval == ""] <- NA
polls <- polls %>% filter(is.na(state))
polls <- polls %>% filter(is.na(partisan))
approval <- approval %>% filter(is.na(state))
polls <- subset(polls, polls$answer == "Trump")
approval <- subset(approval, select = c("end_date", "yes"))
polls <- subset(polls, select = c("end_date", "pct"))
approval <- aggregate(approval["yes"], by=approval["end_date"], mean)
polls <- aggregate(polls["pct"], by=polls["end_date"], mean)
polls$end_date <- as.Date(polls$end_date, format= "%m/%d/%y")
approval$end_date <- as.Date(approval$end_date, format= "%m/%d/%y")
approval <- subset(approval, end_date > "2020-01-01")
approval <- subset(approval, end_date < "2020-11-04")
polls <- subset(polls, end_date > "2020-01-01")
polls <- subset(polls, end_date < "2020-11-04")
names(polls)[names(polls) == 'pct'] <- "poll_pct"

approval_polling <- merge(polls,approval,by="end_date")
approval_polling$end_date <- as.Date(approval_polling$end_date, format= "%m/%d/%y")

approval_polling <- subset(approval_polling, end_date > "2020-01-01")
approval_polling <- subset(approval_polling, end_date < "2020-11-04")
approval_polling <- subset(approval_polling, select = -c(no))

#covid approve of job done
by_party <- read.csv("~/Downloads/covid_approval_polls_adjusted.csv")
by_party$enddate <- as.Date(by_party$enddate, format= "%m/%d/%Y")
by_party <- subset(by_party, enddate > "2020-01-01")
by_party <- subset(by_party, enddate < "2020-11-04")
by_party <- filter(by_party, party != 'all')
ggplot(by_party, aes(x = enddate, y = approve_adjusted, colour = party)) +
  scale_color_manual(values = c('darkblue', 'forestgreen', 'red')) +
  geom_line() +
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
  ylab("Approval Rating of Trump's Handling of Pandemic") +
  ggtitle("Approval Rating of Trump's Handling of Pandemic by Party, 2020") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Party Affiliation")
  


by_p <- ggplot(by_party) + 
  geom_line(aes(y=approve_adjusted, x=enddate, group = 'party', col = "party")) +
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


c_app <- read.csv("~/Downloads/covid_approval_polls_adjusted.csv")
c_app$enddate <- as.Date(c_app$enddate, format= "%m/%d/%Y")
c_app <- subset(c_app, enddate > "2020-01-01")
c_app <- subset(c_app, enddate < "2020-11-04")
c_app <- filter(c_app, party == 'all')
c_app <- subset(c_app, select = c("enddate", "approve_adjusted"))
c_app <- aggregate(c_app["approve_adjusted"], by=c_app["enddate"], mean)
names(c_app)[names(c_app) == 'enddate'] <- "end_date"
c_app2 <- merge(c_app,approval_polling,by="end_date")

summary(lm(c_app2$yes ~ c_app2$approve_adjusted))
summary(lm(c_app2$poll_pct ~ c_app2$approve_adjusted))

ind <- read.csv("~/Downloads/covid_approval_polls_adjusted.csv")
ind$enddate <- as.Date(ind$enddate, format= "%m/%d/%Y")
ind <- subset(ind, enddate > "2020-01-01")
ind <- subset(ind, enddate < "2020-11-04")
ind <- filter(ind, party == 'all')
ind <- subset(ind, select = c("enddate", "approve_adjusted"))
ind <- aggregate(ind["approve_adjusted"], by=ind["enddate"], mean)
names(ind)[names(ind) == 'enddate'] <- "end_date"
ind2 <- merge(ind,approval_polling,by="end_date")

summary(lm(ind2$yes ~ ind2$approve_adjusted))
summary(lm(ind2$poll_pct ~ ind2$approve_adjusted))

ggplot(c_app2) +
  geom_point(aes(x=end_date, y=approve_adjusted)) +
  geom_smooth(aes(x=end_date, y=approve_adjusted,)) +
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
    limits = as.Date(c('08/3/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
  ylab("Approval Rating of Trump's Handling of Pandemic") +
  ggtitle("Overall Approval Rating of Trump's Handling of Pandemic, 2020") +
  theme(plot.title = element_text(hjust = 0.5))
  
  
#covid concern economy
econ <- read.csv("~/Downloads/covid_concern_polls_adjusted.csv")
econ$enddate <- as.Date(econ$enddate, format= "%m/%d/%Y")
econ <- subset(econ, enddate > "2020-01-01")
econ <- subset(econ, enddate < "2020-11-04")
econ <- filter(econ, subject == "concern-economy")
econ$concerned <- econ$very_adjusted + econ$somewhat_adjusted
econ <- subset(econ, select = c("enddate", "concerned"))
econ <- aggregate(econ["concerned"], by=econ["enddate"], mean)
names(econ)[names(econ) == 'enddate'] <- "end_date"
econ2 <- merge(econ,approval_polling,by="end_date")
summary(lm(econ2$yes ~ econ2$concerned))
summary(lm(econ2$poll_pct ~ econ2$concerned))

#covid concern infected
infect <- read.csv("~/Downloads/covid_concern_polls_adjusted.csv")
infect$enddate <- as.Date(infect$enddate, format= "%m/%d/%Y")
infect <- subset(infect, enddate > "2020-01-01")
infect <- subset(infect, enddate < "2020-11-04")
infect <- filter(infect, subject == "concern-infected")
infect$concerned <- infect$very_adjusted + infect$somewhat_adjusted
infect <- subset(infect, select = c("enddate", "concerned"))
infect <- aggregate(infect["concerned"], by=infect["enddate"], mean)
names(infect)[names(infect) == 'enddate'] <- "end_date"
infect2 <- merge(infect,approval_polling,by="end_date")
summary(lm(infect2$yes ~ infect2$concerned))
summary(lm(infect2$poll_pct ~ infect2$concerned))


full <- read.csv("~/Downloads/covid_concern_polls_adjusted.csv")
full$enddate <- as.Date(full$enddate, format= "%m/%d/%Y")
full <- subset(full, enddate > "2020-01-01")
full <- subset(full, enddate < "2020-11-04")
full <- filter(full, subject == "concern-economy")
names(full)[names(full) == 'enddate'] <- "end_date"
ggplot(full) +
  geom_smooth(aes(x=end_date, y=very_adjusted), color = 'Very', se = FALSE) +
  geom_smooth(aes(x=end_date, y=somewhat_adjusted), color = 'Somewhat', se = FALSE) +
  geom_smooth(aes(x=end_date, y=not_very_adjusted), color = 'Not_Very', se = FALSE) +
  geom_smooth(aes(x=end_date, y=not_at_all_adjusted), color = 'Not_at_All', se = FALSE) +
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
    limits = as.Date(c('08/3/2020', '3/11/2020'), format="%d/%m/%Y"),
    guide = waiver(),
    position = "bottom",
    sec.axis = waiver()
  ) +
  ylab("Percentage Concerned") +
  ggtitle("US Concern About Economy Due to Pandemic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(name="",
                      values=c(Very="red", Somewhat="darkblue", Not_Very ="forestgreen", Not_at_All="purple"))

