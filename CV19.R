#### load libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(RCurl)
library(scales)

#### load csv from GitHub ####
# US numbers
deaths_us <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
confirmed_us <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))

# US numbers local (uncomment this section in case GitHub isn't working, hopefully this local data is up to date!!!)
# deaths_us <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# confirmed_us <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# world numbers
world_deaths <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
world_recovered <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
world_confirmed <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

# world numbers local (uncomment this section in case GitHub isn't working, hopefully this local data is up to date!!!)
# world_deaths <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
# world_recovered <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
# world_confirmed <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

#### end load csv ####

# columns that will not be used from full DFs deaths_us and confirmed_us
positions_to_remove <- c(1:6, 8:11, 13:81)

#### deaths by state ####
states_death <- deaths_us %>%
  group_by(Province_State) %>%
  filter(Province_State == "Colorado" |
           Province_State == "Texas" |
           Province_State == "California" |
           Province_State == "Wisconsin" |
           Province_State == "Oklahoma" |
           Province_State == "Kentucky" |
           Province_State == "North Carolina" |
           Province_State == "Georgia" |
           Province_State == "Illinois") %>%
  select(-positions_to_remove)

# DF for state populations based on states_death filter
state_pop <- deaths_us %>% 
  group_by(Province_State) %>% 
  summarize(Population = sum(Population / 1000000))

# massage data
date_columns <- colnames(states_death[, 3:ncol(states_death)]) # date_columns to be used in melt function below
death_data <- melt(states_death, id.vars = "Province_State", measure.vars = date_columns)
death_data$variable <- str_replace(death_data$variable, "X", "") # just removing the 'X' from the column
death_data$variable <- mdy(death_data$variable) # change to month/day/year format
death_data <- rename(death_data, c(variable = "date", value = "deaths")) # rename column names for use later

# merge the state_pop table with the death_data table
death_data$Population <- state_pop$Population[match(death_data$Province_State, state_pop$Province_State)]

# create death_data DF with State, date, and sum of deaths per State by date to be used for population-weighted graph
deaths_per_million_data <- death_data %>%
  group_by(Province_State, date) %>%
  summarize(total_deaths = sum(deaths / Population))

# create death_data DF with State, date, and sum of deaths per State by date to be used for log and linear plots
death_data_to_plot <- death_data %>%
  group_by(Province_State, date) %>%
  summarize(total_deaths = sum(deaths))

### plot deaths, logarithmic ###
death_plot_log <- death_data_to_plot %>% 
  ggplot(aes(x = date, y = total_deaths, color = Province_State, group = Province_State)) +
  ggtitle("COVID-19 Deaths, [Logarithmic Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Deaths") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # x-axis turned 90 degrees
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) + # x-axis label
  scale_y_log10() +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) + # add a vertical line at 20 Apr 2020
  annotate("text", x = as.Date("2020-04-20"), y = 440, label = "*", color = "Purple", size = 20) + # add a star to Texas at 20 Apr 2020
  annotate("text", x = as.Date("2020-04-11"), y = 15, label = "= Texas, 20 Apr 2020\n[507 Deaths]", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-04-10"), y = 14, label = "*", color = "Purple", size = 20)

death_plot_log
### end plot deaths, logarithmic ###

### plot deaths, linear ###
death_plot_lin <- death_data_to_plot %>% 
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

death_plot_lin
### end deaths plot, linear ###

### plot deaths per million, linear ###
death_per_million_plot_lin <- deaths_per_million_data %>% 
  ggplot(aes(x = date, y = total_deaths, color = Province_State, group = Province_State)) +
  ggtitle("COVID-19 Deaths per Million, [Linear Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Deaths per Million") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3)# +
  # annotate("text", x = as.Date("2020-04-20"), y = 470, label = "*", color = "Purple", size = 20) +
  # annotate("text", x = as.Date("2020-04-04"), y = 1000, label = "= Texas, 20 Apr 2020\n[507 Deaths]", color = "Purple", size = 5, hjust = 0) +
  # annotate("text", x = as.Date("2020-04-03"), y = 990, label = "*", color = "Purple", size = 20)

death_per_million_plot_lin
### end deaths per million plot ###

### plot deaths per million, linear ###
death_per_million_plot_log <- deaths_per_million_data %>% 
  ggplot(aes(x = date, y = total_deaths, color = Province_State, group = Province_State)) +
  ggtitle("COVID-19 Deaths per Million, [Log Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Deaths per Million") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  scale_y_log10() +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3)# +
# annotate("text", x = as.Date("2020-04-20"), y = 470, label = "*", color = "Purple", size = 20) +
# annotate("text", x = as.Date("2020-04-04"), y = 1000, label = "= Texas, 20 Apr 2020\n[507 Deaths]", color = "Purple", size = 5, hjust = 0) +
# annotate("text", x = as.Date("2020-04-03"), y = 990, label = "*", color = "Purple", size = 20)

death_per_million_plot_log
### end deaths per million plot ###


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
           Province_State == "North Carolina" |
           Province_State == "Georgia" |
           Province_State == "Illinois") %>% 
  select(-positions_to_remove,-12) # selecting all but the the positions_to_remove columns and -12 position

# massage the data
date_columns <- colnames(states_confirmed[, 2:ncol(states_confirmed)])
confirmed_data <- melt(states_confirmed, id.vars = "Province_State", measure.vars = date_columns)
confirmed_data$variable<- str_replace(confirmed_data$variable, "X", "")
confirmed_data$variable <- mdy(confirmed_data$variable)
confirmed_data <- rename(confirmed_data, c(variable = "date", value = "confirmed_cases"))

# merge the state_pop table with the death_data table
confirmed_data$Population <- state_pop$Population[match(confirmed_data$Province_State, state_pop$Province_State)]

# create death_data DF with State, date, and sum of deaths per State by date to be used for population-weighted graph
confirmed_per_million_data <- confirmed_data %>%
  group_by(Province_State, date) %>%
  summarize(total_cases = sum(confirmed_cases / Population))

# confirmed data for us in logarithmic and linear plots
confirmed_data_to_plot <- confirmed_data %>%
  group_by(Province_State, date) %>%
  summarize(total_cases = sum(confirmed_cases))

### confirmed cases, logarithmic plot ###
confirmed_plot_log <- confirmed_data_to_plot %>% 
  ggplot(aes(x = date, y = total_cases, color = Province_State, group = Province_State)) +
  ggtitle("Confirmed COVID-19 Cases, [Logarithmic Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
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
### end confirmed cases, logarithmic plot ###

### confirmed cases, linear plot ###
confirmed_plot_lin <- confirmed_data_to_plot %>% 
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

confirmed_plot_lin
### end confirmed cases, linear plot ###

### plot confirmed cases per million, linear ###
confirmed_per_million_plot_lin <- confirmed_per_million_data %>% 
  ggplot(aes(x = date, y = total_cases, color = Province_State, group = Province_State)) +
  ggtitle("COVID-19 Confirmed Cases per Million, [Linear Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Confirmed Cases per Million") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3)# +
# annotate("text", x = as.Date("2020-04-20"), y = 470, label = "*", color = "Purple", size = 20) +
# annotate("text", x = as.Date("2020-04-04"), y = 1000, label = "= Texas, 20 Apr 2020\n[507 Deaths]", color = "Purple", size = 5, hjust = 0) +
# annotate("text", x = as.Date("2020-04-03"), y = 990, label = "*", color = "Purple", size = 20)

confirmed_per_million_plot_lin
### end confirmed cases per million linear plot ###

### confirmed cases per million log plot ###
confirmed_per_million_plot_log <- confirmed_per_million_data %>% 
  ggplot(aes(x = date, y = total_cases, color = Province_State, group = Province_State)) +
  ggtitle("COVID-19 Confirmed Cases per Million, [Log Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_smooth() +
  ylab("Confirmed Cases per Million") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("State") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = NULL) +
  scale_y_log10() +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3)# + # vline on 20 Apr
# annotate("text", x = as.Date("2020-04-20"), y = 470, label = "*", color = "Purple", size = 20) +
# annotate("text", x = as.Date("2020-04-04"), y = 1000, label = "= Texas, 20 Apr 2020\n[507 Deaths]", color = "Purple", size = 5, hjust = 0) +
# annotate("text", x = as.Date("2020-04-03"), y = 990, label = "*", color = "Purple", size = 20)

confirmed_per_million_plot_log
### end confirmed cases per million log plot ###



#### world calculations ####
global_positions_to_remove <- c(1, 3:4) # indeces of the columns that aren't needed in the graph

# create DF of US deaths
global_deaths <- world_deaths %>%
  filter(Country.Region == "US" |
           Country.Region == "Germany" |
           Country.Region == "Korea, South" |
           Country.Region == "Sweden" |
           Country.Region == "Italy") %>%
  select(-global_positions_to_remove)

# process to massage data
date_columns <- colnames(global_deaths[, 2:ncol(global_deaths)]) # grab the column headers (except for first one)
global_death_data <- melt(global_deaths, id.vars = "Country.Region", measure.vars = date_columns) # melt states per day and by value
global_death_data$variable<- str_replace(global_death_data$variable, "X", "")
global_death_data$variable <- mdy(global_death_data$variable)
global_death_data <- rename(global_death_data, c(variable = "date", value = "global_deaths"))

# create DF of US recovered
global_recovered <- world_recovered %>%
  filter(Country.Region == "US" |
           Country.Region == "Germany" |
           Country.Region == "Korea, South" |
           Country.Region == "Sweden" |
           Country.Region == "Italy") %>%
  select(-global_positions_to_remove)

date_columns <- colnames(global_recovered[, 2:ncol(global_recovered)])
global_recovered_data <- melt(global_recovered, id.vars = "Country.Region", measure.vars = date_columns)
global_recovered_data$variable<- str_replace(global_recovered_data$variable, "X", "")
global_recovered_data$variable <- mdy(global_recovered_data$variable)
global_recovered_data <- rename(global_recovered_data, c(variable = "date", value = "global_recovered"))

global_confirmed <- world_confirmed %>%
  filter(Country.Region == "US" |
           Country.Region == "Germany" |
           Country.Region == "Korea, South" |
           Country.Region == "Sweden" |
           Country.Region == "Italy") %>% 
  select(-global_positions_to_remove)

date_columns <- colnames(global_confirmed[, 2:ncol(global_confirmed)])
global_confirmed_data <- melt(global_confirmed, id.vars = "Country.Region", measure.vars = date_columns)
global_confirmed_data$variable<- str_replace(global_confirmed_data$variable, "X", "")
global_confirmed_data$variable <- mdy(global_confirmed_data$variable)
global_confirmed_data <- rename(global_confirmed_data, c(variable = "date", value = "global_confirmed_data"))

global_active_df <- global_confirmed_data
global_active_df$global_deaths <- global_death_data$global_deaths
global_active_df$global_recovered <- global_recovered_data$global_recovered
global_active_df <- global_active_df %>%
  group_by(Country.Region, date) %>% 
  summarize(global_active = global_confirmed_data - global_deaths - global_recovered)

global_plot_reg <- global_active_df %>% 
  ggplot(aes(x = date, y = global_active, color = Country.Region, group = Country.Region)) +
  #scale_colour_manual(values=cbPalette) +
  ggtitle("Active Cases = Confirmed - Deaths - Recovered, [Logarithmic Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = TRUE) +
  geom_line() +
  ylab("Active Cases") + # name the y-axis
  xlab("Date") + # name the x-axis
  scale_color_discrete("Country") + # name the legend
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 days", minor_breaks = NULL) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) +
  annotate("text", x = as.Date("2020-04-20"), y = 450000, label = "*", color = "Purple", size = 18) +
  annotate("text", x = as.Date("2020-01-27"), y = 30000, label = "= US, 20 Apr 2020\n[669,903 Active Cases]", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-01-24"), y = 29500, label = "*", color = "Purple", size = 18)

global_plot_reg


#### write tables ####
# calculate deaths per million aggregated to yesterday's date
deaths_per_million_aggregated <- death_data %>%
  group_by(Province_State, date) %>%
  summarize(total_deaths = sum(deaths / Population)) %>% 
  filter(date == Sys.Date() - 1) # -1 day because today's data may not be populated yet
  #filter(date == as.Date("2020-04-22"))

# write the daily deaths/million table to a .csv
write.csv(deaths_per_million_aggregated, paste("Tables/deaths_per_million, ", Sys.Date(), ".csv", sep = ""))

# calculate confirmed per million aggregated to yesterday's date
confirmed_per_million_aggregated <- confirmed_data %>%
  group_by(Province_State, date) %>%
  summarize(total_cases = sum(confirmed_cases / Population)) %>% 
  filter(date == Sys.Date() - 1) # -1 day because today's data may not be populated yet
  #filter(date == as.Date("2020-04-22"))

# write the daily confirmed cases/million table to a .csv
write.csv(confirmed_per_million_aggregated, paste("Tables/cases_per_million, ", Sys.Date(), ".csv", sep = ""))





pdf(file="cases_per_million_lin_test.pdf", width = 827, height = 643)

confirmed_plot_lin
dev.copy(png,'myplot.png', width = 827, height = 643)
dev.off()

png(file="deaths_per_million_lin_test.png", width = 827, height = 643)
death_per_million_plot_lin
dev.off()
