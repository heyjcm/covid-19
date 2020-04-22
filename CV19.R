# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(RCurl)
library(scales)

# set working directory
# setwd('~/Users/Christian/Desktop/CV/COVID-19/csse_covid_19_data')

# load csv from GitHub
# US numbers
deaths_us <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
confirmed_us <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
recovered_us <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-21-2020.csv"))

# global numbers
world_deaths <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
world_recovered <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
world_confirmed <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

# deaths_us <- read.csv("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# confirmed_us <- read.csv("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# columns that will not be used from full DFs deaths_us and confirmed_us
positions_to_remove <- c(1:6, 8:81)

#### deaths by state ####
states_death <- deaths_us %>%
  group_by(Province_State) %>%
  filter(Province_State == "Colorado" |
           Province_State == "Texas" |
           Province_State == "California" |
           Province_State == "Wisconsin" |
           Province_State == "Oklahoma" |
           Province_State == "Kentucky" |
           Province_State == "North Carolina") %>% 
  select(-positions_to_remove)

date_columns <- colnames(states_death[, 2:ncol(states_death)])
death_data <- melt(states_death, id.vars = "Province_State", measure.vars = date_columns)
death_data$variable<- str_replace(death_data$variable, "X", "")
death_data$variable <- mdy(death_data$variable)
death_data <- rename(death_data, c(variable = "date", value = "deaths"))

death_data <- death_data %>%
  group_by(Province_State, date) %>%
  summarize(total_deaths = sum(deaths))

death_plot_log <- death_data %>% 
  ggplot(aes(x = date, y = total_deaths, color = Province_State, group = Province_State)) +
  ggtitle("COVID-19 Deaths, [Logarithmic Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Deaths") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  scale_y_log10() +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) +
  annotate("text", x = as.Date("2020-04-20"), y = 440, label = "*", color = "Purple", size = 20) +
  annotate("text", x = as.Date("2020-04-11"), y = 15, label = "= Texas, 20 Apr 2020\n[507 Deaths]", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-04-10"), y = 14, label = "*", color = "Purple", size = 20)

death_plot_log

death_plot_reg <- death_data %>% 
  ggplot(aes(x = date, y = total_deaths, color = Province_State, group = Province_State)) +
  ggtitle("COVID-19 Deaths, [Linear Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Deaths") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) +
  annotate("text", x = as.Date("2020-04-20"), y = 470, label = "*", color = "Purple", size = 20) +
  annotate("text", x = as.Date("2020-04-04"), y = 1000, label = "= Texas, 20 Apr 2020\n[507 Deaths]", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-04-03"), y = 990, label = "*", color = "Purple", size = 20)

death_plot_reg

#### confirmed data by state ####
# create df called states_confirmed from full df confirmed_us
# this will be grouped by State and filtered by the following:
# Colorado, Texas, California, Wisconsin, Oklahoma, Kentucky, North Carolina
states_confirmed <- confirmed_us %>%
  group_by(Province_State) %>%
  filter(Province_State == "Colorado" |
           Province_State == "Texas" |
           Province_State == "California" |
           Province_State == "Wisconsin" |
           Province_State == "Oklahoma" |
           Province_State == "Kentucky" |
           Province_State == "North Carolina") %>% 
  select(-positions_to_remove) # selecting all but the the positions_to_remove columns

date_columns <- colnames(states_confirmed[, 2:ncol(states_confirmed)])
confirmed_data <- melt(states_confirmed, id.vars = "Province_State", measure.vars = date_columns)
confirmed_data$variable<- str_replace(confirmed_data$variable, "X", "")
confirmed_data$variable <- mdy(confirmed_data$variable)
confirmed_data <- rename(confirmed_data, c(variable = "date", value = "confirmed_cases"))

confirmed_data <- confirmed_data %>%
  group_by(Province_State, date) %>%
  summarize(total_cases = sum(confirmed_cases))

confirmed_plot_log <- confirmed_data %>% 
  ggplot(aes(x = date, y = total_cases, color = Province_State, group = Province_State)) +
  ggtitle("Confirmed COVID-19 Cases, [Logarithmic Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_line() +
  ylab("Confirmed Cases") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  scale_y_log10() +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) +
  annotate("text", x = as.Date("2020-04-20"), y = 17200, label = "*", color = "Purple", size = 20) +
  annotate("text", x = as.Date("2020-04-15"), y = 1000, label = "= Texas, 20 Apr 2020\n[19,751 Cases]", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-04-14"), y = 1000, label = " *", color = "Purple", size = 20)

confirmed_plot_log

confirmed_plot_reg <- confirmed_data %>% 
  ggplot(aes(x = date, y = total_cases, color = Province_State, group = Province_State)) +
  ggtitle("Confirmed COVID-19 Cases, [Linear Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Confirmed Cases") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) +
  annotate("text", x = as.Date("2020-04-20"), y = 18750, label = "*", color = "Purple", size = 20) +
  annotate("text", x = as.Date("2020-04-03"), y = 30000, label = "= Texas, 20 Apr 2020\n[19,751 Cases]", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-04-02"), y = 29500, label = "*", color = "Purple", size = 20)

confirmed_plot_reg

#### world calculations ####
global_positions_to_remove <- c(1, 3:4) # indeces of the columns that aren't needed in the graph

# create DF of US deaths
US_deaths <- world_deaths %>%
  filter(Country.Region == "US") %>% 
  select(-global_positions_to_remove)

# process to massage data
date_columns <- colnames(US_deaths[, 2:ncol(US_deaths)]) # grab the column headers (except for first one)
US_death_data <- melt(US_deaths, id.vars = "Country.Region", measure.vars = date_columns) # melt states per day and by value
US_death_data$variable<- str_replace(US_death_data$variable, "X", "")
US_death_data$variable <- mdy(US_death_data$variable)
US_death_data <- rename(US_death_data, c(variable = "date", value = "US_deaths"))

# create DF of US recovered
US_recovered <- world_recovered %>%
  filter(Country.Region == "US") %>% 
  select(-global_positions_to_remove)

date_columns <- colnames(US_recovered[, 2:ncol(US_recovered)])
US_recovered_data <- melt(US_recovered, id.vars = "Country.Region", measure.vars = date_columns)
US_recovered_data$variable<- str_replace(US_recovered_data$variable, "X", "")
US_recovered_data$variable <- mdy(US_recovered_data$variable)
US_recovered_data <- rename(US_recovered_data, c(variable = "date", value = "US_recovered"))


US_confirmed <- world_confirmed %>%
  filter(Country.Region == "US") %>% 
  select(-global_positions_to_remove)

date_columns <- colnames(US_confirmed[, 2:ncol(US_confirmed)])
US_confirmed_data <- melt(US_confirmed, id.vars = "Country.Region", measure.vars = date_columns)
US_confirmed_data$variable<- str_replace(US_confirmed_data$variable, "X", "")
US_confirmed_data$variable <- mdy(US_confirmed_data$variable)
US_confirmed_data <- rename(US_confirmed_data, c(variable = "date", value = "US_confirmed_data"))

US_active_df <- US_confirmed_data
US_active_df$US_deaths <- US_death_data$US_deaths
US_active_df$US_recovered <- US_recovered_data$US_recovered
US_active_df <- US_active_df %>%
  group_by(Country.Region, date) %>% 
  summarize(US_active = US_confirmed_data - US_deaths - US_recovered)

US_plot_reg <- US_active_df %>% 
  ggplot(aes(x = date, y = US_active, color = Country.Region, group = Country.Region)) +
  ggtitle("US Active Cases = Confirmed - Deaths - Recovered, [Logarithmic Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Active US Cases") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("US") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 days", minor_breaks = NULL) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) +
  annotate("text", x = as.Date("2020-04-20"), y = 450000, label = "*", color = "Purple", size = 18) +
  annotate("text", x = as.Date("2020-01-27"), y = 30000, label = "= US, 20 Apr 2020\n[669,903 Active Cases]", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-01-24"), y = 29500, label = "*", color = "Purple", size = 18)

US_plot_reg
