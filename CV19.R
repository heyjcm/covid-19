#### load libraries ####
library(tidyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(RCurl)
library(scales)
library(data.table)
library(dplyr)
library(ggpubr)
#### end load libraries ####

#### start load csv from Johns Hopkins GitHub repo ####
# US csv
deaths_us <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
confirmed_us <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
rt_value <- read.csv(text = getURL("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv"))


# US numbers local (uncomment this section in case GitHub isn't working, hopefully this local data is up to date!!!)
# deaths_us <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# confirmed_us <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# world numbers
world_deaths <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
world_recovered <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
world_confirmed <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
# csv from Tanu N Prabhu: https://www.kaggle.com/tanuprabhu/population-by-country-2020?select=population_by_country_2020.csv
country_pop <- read.csv(file = 'population_by_country_2020.csv')
# renaming the columns for clarity
country_pop <- country_pop %>% rename(country = Country..or.dependency., population = Population..2020.)

# world numbers local (uncomment this section in case GitHub isn't working, hopefully this local data is up to date!!!)
# world_deaths <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
# world_recovered <- read.csv("JHU Repo/cage <- c(17,18,18,17,18,19,18,16,18,18)sse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
# world_confirmed <- read.csv("JHU Repo/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

#### end load csv from Johns Hopkins GitHub repo ####


#### start functions used in this script ####
### print_plot ###
# function to print plots
print_plot <- function(plot_df, plot_title = NULL, is_state_plot = FALSE, name_of_state = NULL, pl_width = 827, pl_height = 1286, pl_res = 125) {
  # abbreviate name_of_state if name_of_state is populated
  # for use in filename for state bar graph
  if (!is.null(name_of_state)) {
    abb_state_name <- c(state.abb, "DC")[match(name_of_state, c(state.name, "District of Columbia"))]
  }
  
  # variable to hold the Sys.Date() without dashes for use in file naming
  date_for_filenames <- str_replace_all(as.character(Sys.Date()), "-", "")
  
  # go into this IF when function is passed the by-day state graphs because
  # the filenaming is slightly different than the other graphs
  if (is_state_plot == TRUE) {
    png(filename = paste("Graphs/", date_for_filenames, "/", "states_", abb_state_name, "_", date_for_filenames,".png", sep = ""), width = pl_width, height = pl_height, res = pl_res)
  }
  
  # this is for all other graphs
  else {
    # use the plot_title variable as part of the filename
    png(filename = paste("Graphs/", date_for_filenames, "/", plot_title, "_", date_for_filenames,".png", sep = ""),  width = pl_width, height = pl_height, res = pl_res)
  }
  
  # export the plot from the png() function above
  print(plot_df)
  dev.off()
}
### end print_plot ###

### death_func ###
# this function creates all the states death data used for creating plot objects
# this is used once for the primary states, and once for when making the daily
# death plots for all states.
death_func <- function(deaths_us, positions_to_remove, list_of_states, state_pop) {
  states_death <- deaths_us %>%
    group_by(Province_State) %>%
    filter(Province_State %in% list_of_states) %>%
    select(-positions_to_remove)
  
  # massage the data formatting
  date_columns <- colnames(states_death[, 3:ncol(states_death)]) # date_columns to be used in melt function below
  death_data <- reshape2::melt(states_death, id.vars = "Province_State", measure.vars = date_columns) # making a new DF using the melt function to have one row for each date with corresponding deaths on that day
  death_data$variable <- str_replace(death_data$variable, "X", "") # just removing the 'X' from the column
  death_data$variable <- mdy(death_data$variable) # change to month/day/year format
  death_data <- rename(death_data, date = variable, total_deaths = value) # rename column names for use later
  
  # merge the state_pop table with the death_data table
  death_data$Population <- state_pop$Population[match(death_data$Province_State, state_pop$Province_State)]
  
  return(death_data)
}
### end death_func ###

### confirmed_func ###
confirmed_func <- function(confirmed_us, positions_to_remove, list_of_states, state_pop) {
  # create df called states_confirmed from full df confirmed_us
  # this will be grouped by State and filtered by the following:
  # Colorado, Texas, California, Wisconsin, Oklahoma, Kentucky, North Carolina
  states_confirmed <- confirmed_us %>%
    group_by(Province_State) %>%
    filter(Province_State %in% list_of_states) %>% 
    select(-positions_to_remove, -12) # selecting all but the the positions_to_remove columns and -12 position
  
  # massage the data
  date_columns <- colnames(states_confirmed[, 2:ncol(states_confirmed)])
  confirmed_data <- reshape2::melt(states_confirmed, id.vars = "Province_State", measure.vars = date_columns)
  confirmed_data$variable<- str_replace(confirmed_data$variable, "X", "")
  confirmed_data$variable <- mdy(confirmed_data$variable)
  confirmed_data <- rename(confirmed_data, date = variable, confirmed_cases = value)
  
  # merge the state_pop table with the death_data table
  confirmed_data$Population <- state_pop$Population[match(confirmed_data$Province_State, state_pop$Province_State)]
  
  return(confirmed_data)
}
### end confirmed_func ###

### death_data_to_plot_func ###
# function returns the death data to plot
death_data_to_plot_func <- function(death_data) {
  # create death_data DF with State, date, and sum of deaths per State by date to be used for log and linear plots
  death_data_to_plot <- death_data %>%
    group_by(Province_State, date) %>%
    summarize(total_deaths = sum(total_deaths))
  
  return(death_data_to_plot)
}
### end death_data_to_plot_func ###

### start make_graphs_func ###
# Creates a graph in a specific format for this project
make_graphs_func <- function(data_to_plot_df, d_or_c = "d", lg_or_ln = "lg", standardized = FALSE, region_name = "State") {
  # initialized to "death_" to match default arg value
  title_of_plot <- "deaths_"
  
  # initialized to "Deaths" to match default arg value
  graph_type <- "Deaths"
  
  # change graph_type to "Confirmed Cases"; change plot title to start with "confirmed_"
  if (d_or_c == "c") {
    graph_type <- "Confirmed Cases"
    title_of_plot <- "confirmed_"
  }
  
  # add the "per million" designation if using a standardized graph
  if (standardized == TRUE) {
    graph_type <- paste(graph_type, " per Million", sep = "")
    
    # concatenate "per million" to plot title if standardized
    title_of_plot <- paste(title_of_plot, "per_million_", sep = "")
  }
  
  # initialized to "Logarithmic" to match default arg value
  graph_scale <- "Logarithmic"
  
  # change graph_scale to "Linear" 
  if (lg_or_ln == "ln") {
    graph_scale <- "Linear"
    
    # concatenate "lin" to plot title if linear
    title_of_plot <- paste(title_of_plot, "lin", sep = "")
  }
  # concatenate "log" to plot title if set to logarithmic
  else {
    title_of_plot <- paste(title_of_plot, "log", sep = "")
  }
  
  # variable y_aes that holds the column for aes(y = y_aes) in ggplot below
  # switch used to figure out the index of the column name so it's more dynamic
  # rather than a hard-coded index to the column name in question. Assumes the
  # df column names are either total_deaths or total_confirmed
  y_aes <- colnames(data_to_plot_df[grep(switch(d_or_c,
                                                "d" = "total_deaths",
                                                "c" = "total_confirmed"),
                                         colnames(data_to_plot_df))])
  
  ### start plot ###
  df_to_plot <- data_to_plot_df %>%
    ggplot(aes_string(x = "date", y = y_aes, color = "Province_State")) +
    geom_point() +
    scale_color_manual(values = graph_colors, name = region_name) + # manually set the color to state_colors
    geom_smooth() +
    ggtitle(paste("COVID-19 ", graph_type, " by ", region_name, " ", "[", graph_scale, " Scale]", sep = "")) +
    theme(plot.title = element_text(hjust = 0.5)) + # centers the title at the top
    ylab(paste("Total ", graph_type, sep = "")) + # name the y-axis
    xlab("Date") + # name the x-axis
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # x-axis turned 90 degrees
    scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL) + # x-axis label
    geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) #+ # add a vertical line at 20 Apr 2020
  
  if (lg_or_ln == "lg") {
    df_to_plot <- df_to_plot +
      scale_y_log10() # makes the y-axis on a log scale
  }
  
  title_of_plot <- paste(region_name, title_of_plot, sep = "_")
  
  print(paste("Exporting: ", title_of_plot, sep = ""))
  
  print_plot(df_to_plot, title_of_plot,  pl_height = 643, pl_res = 97)
}
### end make_graphs_func ###


confirmed_data_to_plot_func <- function(confirmed_data) {
  # confirmed data for us in logarithmic and linear plots
  confirmed_data_to_plot <- confirmed_data %>%
    group_by(Province_State, date) %>%
    summarize(total_confirmed = sum(confirmed_cases))
  
  return(confirmed_data_to_plot)
}

#### end functions used in this script ####


#### start global variables ####
# primary States that I'm specifically tracking in the main graphs
list_of_primary_states <- c("Colorado",
                            "Texas",
                            "California",
                            "Illinois",
                            "Georgia",
                            "Florida",
                            "Virginia",
                            "Michigan",
                            "Arizona",
                            "Wisconsin")

# colors for the graphs
graph_colors <- c("red",
                  "orange",
                  "chocolate",
                  "green",
                  "blue",
                  "violet",
                  "pink",
                  "turquoise",
                  "skyblue",
                  "saddlebrown",
                  "magenta")

# all the US States (and DC) in a list
# used the built-in state.name and then inserted "District of Columbia" using sort() for alpha order
list_of_all_states <- sort(c(state.name, "District of Columbia"))

# remove columns that will not be used from full DFs deaths_us and confirmed_us
positions_to_remove <- c(1:6, 8:11, 13:82)

# variable that holds a DF with the population of each state (per million)
#state_pop <- state_pop_func(deaths_us)
# DF for state populations based on states_death filter
state_pop <- deaths_us %>%
  group_by(Province_State) %>%
  summarize(Population = sum(Population / 1000000))

#### end global variables ####


#### start deaths by state graphs section ####
# build DF needed for deaths plots
death_data <- death_func(deaths_us, positions_to_remove, list_of_primary_states, state_pop)
death_data_to_plot <- death_data_to_plot_func(death_data)

### end deaths plot, log ###
make_graphs_func(death_data_to_plot, d_or_c = "d", lg_or_ln = "lg", standardized = FALSE)
### end deaths plot, log ###

### start deaths plot, linear ###
make_graphs_func(death_data_to_plot, d_or_c = "d", lg_or_ln = "ln", standardized = FALSE)
### end deaths plot, linear ###

### plot deaths per million, linear ###
# create death_data DF with State, date, and sum of deaths per State by date to be used for population-weighted graph
deaths_per_million_data <- death_data %>%
  group_by(Province_State, date) %>%
  summarize(total_deaths = sum(total_deaths / Population))

### start deaths per million plot, log ###
make_graphs_func(deaths_per_million_data, d_or_c = "d", lg_or_ln = "lg", standardized = TRUE)
### end deaths per million plot, log ###

### start deaths per million plot, linear ###
make_graphs_func(deaths_per_million_data, d_or_c = "d", lg_or_ln = "ln", standardized = TRUE)
### end deaths per million plot, linear ###

#### end deaths by state graphs section ####


#### start confirmed cases section ####
# build DF needed for confirmed cases plots
confirmed_data <- confirmed_func(confirmed_us, positions_to_remove, list_of_primary_states, state_pop)
confirmed_data_to_plot <- confirmed_data_to_plot_func(confirmed_data)

### start confirmed cases, logarithmic plot ###
make_graphs_func(confirmed_data_to_plot, d_or_c = "c", lg_or_ln = "lg", standardized = FALSE)
### end confirmed cases, logarithmic plot ###

### start confirmed cases, linear plot ###
make_graphs_func(confirmed_data_to_plot, d_or_c = "c", lg_or_ln = "ln", standardized = FALSE)
### end confirmed cases, linear plot ###

# create death_data DF with State, date, and sum of deaths per State by date to be used for population-weighted graph
confirmed_per_million_data <- confirmed_data %>%
  group_by(Province_State, date) %>%
  summarize(total_confirmed = sum(confirmed_cases / Population))

### start confirmed cases per million log plot ###
make_graphs_func(confirmed_per_million_data, d_or_c = "c", lg_or_ln = "lg", standardized = TRUE)
### end confirmed cases per million log plot ###

### start confirmed cases per million linear plot ###
make_graphs_func(confirmed_per_million_data, d_or_c = "c", lg_or_ln = "ln", standardized = TRUE)
### end confirmed cases per million linear plot ###

#### end confirmed cases section ####


#### start deaths and confirmed cases per day by State bar graphs ####
### start deaths by day graphs ###
# create new DF with all the states death in it
death_data_for_death_by_day <- death_func(deaths_us, positions_to_remove, list_of_all_states, state_pop)
death_data_to_plot <- death_data_to_plot_func(death_data_for_death_by_day)

# create an empty list to populate with State graphs
states_death_list <- list()

deaths_bar_to_plot_full <- mutate(death_data_to_plot, deaths_delta = total_deaths - lag(total_deaths)) %>%
  filter(!is.na(deaths_delta))

for (i in 1:length(unique(deaths_bar_to_plot_full$Province_State))) {
  # set var to the current State name
  var = unique(deaths_bar_to_plot_full$Province_State)[i]
  
  # just some indicators to show progress in the console
  # comment out if not wanted
  print(i)
  print(var)
  
  deaths_bar_to_plot <- deaths_bar_to_plot_full %>%
    filter(Province_State == var)
  
  # plot the deaths per day
  deaths_bar_plot <- deaths_bar_to_plot %>%
    ggplot(aes(x = date, y = deaths_delta, fill = Province_State)) +
    geom_bar(stat = "identity", color = "blue") +
    geom_smooth(color = "green") +
    scale_fill_manual(values = "turquoise", name = "State") +
    ggtitle("COVID-19 Deaths per Day") +
    theme(plot.title = element_text(hjust = 0.5)) + # centers the title at the top
    xlab("Date") +
    ylab("Number of Deaths") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL)
  
  # add the current State bar plot into the list for use later on
  states_death_list[[i]] <- deaths_bar_plot
}
### end deaths per day section ###

### start confirmed per day section ###
confirmed_data_for_confirmed_by_day <- confirmed_func(confirmed_us, positions_to_remove, list_of_all_states, state_pop)
confirmed_data_to_plot <- confirmed_data_to_plot_func(confirmed_data_for_confirmed_by_day)

# create an empty list to populate with State graphs
states_confirmed_list <- list()

confirmed_bar_to_plot_full <- mutate(confirmed_data_to_plot, confirmed_delta = total_confirmed - lag(total_confirmed)) %>%
  filter(!is.na(confirmed_delta))

for (j in 1:length(unique(confirmed_bar_to_plot_full$Province_State))) {
  # set var to the current State name
  var = unique(confirmed_bar_to_plot_full$Province_State)[j]
  
  # just some indicators to show progress in the console
  # comment out if not wanted
  print(j)
  print(var)
  
  confirmed_bar_to_plot <- confirmed_bar_to_plot_full %>%
    filter(Province_State == var)
  
  # plot the confirmed per day
  confirmed_bar_plot <- confirmed_bar_to_plot %>%
    ggplot(aes(x = date, y = confirmed_delta, fill = Province_State)) +
    geom_bar(stat = "identity", color = "plum") +
    #geom_point() +
    geom_smooth(color = "blue") +
    scale_fill_manual(values = "#008080", name = "State") +
    ggtitle("COVID-19 Confirmed Cases per Day") +
    theme(plot.title = element_text(hjust = 0.5)) + # centers the title at the top
    xlab("Date") +
    ylab("Number of Confirmed Cases") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL) +
    geom_vline(xintercept = as.numeric(as.Date("2020-07-14")), linetype=3)
  
  # add the current State bar plot into the list for use later on
  states_confirmed_list[[j]] <- confirmed_bar_plot
}
### end confirmed per day section ###
#### end deaths and confirmed cases per day by State bar graphs ####

#### start section for hospitalization ####

# read in first file in the folder
daily_US <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-12-2020.csv"))

# columns in daily_US to remove
positions_to_remove_daily_US <- c(2, 4, 5, 10, 11, 14:18)
daily_US_df <- daily_US %>%
  select(-positions_to_remove_daily_US)

# change the Last_Update date to 2020-04-12
daily_US_df$Last_Update <- as.Date("2020-04-12")

# loop through to read each CSV from JHU "csse_covid_19_daily_reports_us" folder and then rbind() each to daily_US_df to get entire data set in one DF
# start with the second file in the folder since first file was already entered above
start_date <- as.Date("13-04-2014", format = "%d-%m-%y")

# end date is today's date (Sys.Date) - 1 because JHU is always a day behind
end_date <- as.Date(Sys.Date() - 1, format = "%d-%m-%y")

# copy start_date var to use in loop
theDate <- start_date

#loop while theDate in question is less than the end_date var
while (theDate <= end_date) {
  # URL created using current theDate in loop
  myURL <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", format(theDate,"%m-%d-%Y"), ".csv", sep = "")
  print(myURL)
  
  # read in theDate CSV
  daily_US <- read.csv(text = getURL(myURL))
  
  # keep the columns that I want to keep
  daily_US <- daily_US %>%
    select(-positions_to_remove_daily_US)
  
  # make the Last_Update column a date using current theDate value
  daily_US$Last_Update <- as.Date(theDate)
  
  # rbind() to existing daily_US_df
  daily_US_df <- rbind(daily_US_df, daily_US)
  
  # increment theDate to the next date
  theDate <- theDate + 1
}

# rename the Last_Update column to date (more intuitive)
daily_US_df <- rename(daily_US_df, date = Last_Update, total_deaths = Deaths, total_confirmed = Confirmed, total_recovered = Recovered, total_active = Active, total_tested = People_Tested, total_hospitalized = People_Hospitalized)

# re-order daily_US_df by Province_State
daily_US_df <- daily_US_df[order(daily_US_df$Province_State),]

# daily_US_bar_deaths_to_plot_full <- subset(mutate(daily_US_df, deaths_delta = total_deaths - lag(total_deaths)),
#                                            select = -c(total_confirmed, total_recovered, total_active, total_tested, total_hospitalized))
# 
# daily_US_bar_confirmed_to_plot_full <- subset(mutate(daily_US_df, confirmed_delta = total_confirmed - lag(total_confirmed)),
#                                               select = -c(total_deaths, total_recovered, total_active, total_tested, total_hospitalized))
# 
# daily_US_bar_tested_to_plot_full <- subset(mutate(daily_US_df, tested_delta = total_tested - lag(total_tested)),
#                                                  select = -c(total_deaths, total_confirmed, total_recovered, total_active, total_hospitalized))

# create an empty list to populate with State graphs
states_hospitalized_list <- list()

daily_US_bar_hospitalized_to_plot_full <- subset(mutate(daily_US_df, hospitalized_delta = total_hospitalized - lag(total_hospitalized)),
                                                 select = -c(total_deaths, total_confirmed, total_recovered, total_active, total_tested))

states_hospitalized <- daily_US_bar_hospitalized_to_plot_full %>%
  group_by(Province_State) %>%
  filter(Province_State %in% list_of_all_states)

states_hospitalized[is.na(states_hospitalized)] <- 0

# confirmed_bar_to_plot_full <- mutate(confirmed_data_to_plot, confirmed_delta = total_confirmed - lag(total_confirmed)) %>%
#   filter(!is.na(confirmed_delta))

for (a in 1:length(unique(states_hospitalized$Province_State))) {
  # set var to the current State name
  var = unique(states_hospitalized$Province_State)[a]
  
  # just some indicators to show progress in the console
  # comment out if not wanted
  print(a)
  print(var)
  
  hospitalized_bar_to_plot <- states_hospitalized %>%
    filter(Province_State == var)
  
  # plot the confirmed per day
  hospitalized_bar_plot <- hospitalized_bar_to_plot %>%
    ggplot(aes(x = date, y = hospitalized_delta, fill = Province_State)) +
    geom_bar(stat = "identity", color = "pink") +
    geom_smooth(color = "orange") +
    scale_fill_manual(values = "#008080", name = "State") +
    ggtitle("COVID-19 Hospitalized per Day") +
    theme(plot.title = element_text(hjust = 0.5)) + # centers the title at the top
    xlab("Date") +
    ylab("Total Hospitalized") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL) +
    geom_vline(xintercept = as.numeric(as.Date("2020-07-14")), linetype=3)
  
  # add the current State bar plot into the list for use later on
  states_hospitalized_list[[a]] <- hospitalized_bar_plot
}

#### end section for hospitalization ####


#### start loop to export States section ####
# loop to export states' confirmed/deaths by day plots
for (k in 1:length(list_of_all_states)) {
  # get State's name on the plot
  current_state <- as.character(unique(states_confirmed_list[[k]]$data[1]$Province_State))
  
  # print progress in the console
  print(paste("Exporting #", k, ": ", current_state, sep = ""))
  
  # use ggarrange to plot the states_confirmed and states_death plots on a single plot
  daily_plot <- ggarrange(states_confirmed_list[[k]], states_hospitalized_list[[k]], states_death_list[[k]], ncol = 1, nrow = 3)
  
  #export plot to png
  print_plot(daily_plot, is_state_plot = TRUE, name_of_state = current_state)
}
#### end loop to export States section ####


#### start countries graph section ####
# all the countries in the Active cases plot
countries <- c("US",
               "Germany",
               "Korea, South",
               "Sweden",
               "Italy",
               "Philippines",
               "Japan",
               "Argentina",
               "Chile",
               "New Zealand",
               "Brazil")

# Alternate countries
# countries <- c("US",
#                "China",
#                "Canada"
#                "Sweden",
#                "Denmark")

# colors to use for each country (countries will show up in alphabetical order
# and the first country will be red, second is orange, and so on)
# countries_colors <- c("red",
#                       "orange",
#                       "chocolate",
#                       "green",
#                       "blue",
#                       "violet",
#                       "pink",
#                       "turquoise",
#                       "skyblue",
#                       "saddlebrown")


################################################################################
################################################################################

# variable that holds a DF with the population of each country (per million)
country_pop <- country_pop %>%
  group_by(country) %>%
  summarize(population = sum(population / 1000000))

# list_of_countries <- data.frame(world_confirmed$Country.Region)
# list_of_countries <- rename(list_of_countries, country_name = world_confirmed.Country.Region)
# # Remove duplicates based on country_name columns
# list_of_countries <- list_of_countries[!duplicated(list_of_countries$country_name), ]
# list_of_countries <- data.frame(list_of_countries)

# change the country names from country_pop DF to match the country names in the JHU DF
country_pop$country <- gsub("United States", "US", country_pop$country)
country_pop$country <- gsub("South Korea", "Korea, South", country_pop$country)
country_pop$country <- gsub("Taiwan", "Taiwan*", country_pop$country)
country_pop$country <- gsub("Côte d'Ivoire", "Cote d'Ivoire", country_pop$country)

################################################################################
################################################################################



global_positions_to_remove <- c(1, 3:4) # indeces of the columns that aren't needed in the graph

# create DF of global deaths
global_deaths <- world_deaths %>%
  filter(Country.Region %in% countries) %>%
  select(-global_positions_to_remove)

# process to massage data
date_columns <- colnames(global_deaths[, 2:ncol(global_deaths)]) # grab the column headers (except for first one)
global_death_data <- melt(global_deaths, id.vars = "Country.Region", measure.vars = date_columns) # melt states per day and by value
global_death_data$variable<- str_replace(global_death_data$variable, "X", "")
global_death_data$variable <- mdy(global_death_data$variable)
global_death_data <- rename(global_death_data, date = variable, global_deaths = value)#rename(global_death_data, c(variable = "date", value = "global_deaths"))

# this line aggregates each country's numbers by date
# (some countries have multiple entries for each date due to provinces/regions)
# some day I will turn this into a function
# https://datascienceplus.com/aggregate-data-frame-r/
global_death_data <- aggregate(x = global_death_data[c("global_deaths")],
                              by = global_death_data[c("Country.Region", "date")],
                              FUN = function(smash_together) {
                                sum(pmax(smash_together, 0))
                              }
)

# merge the country_pop table with the global_death_data table
global_death_data$Population <- country_pop$population[match(global_death_data$Country.Region, country_pop$country)]



################################################################################
################################################################################

### plot deaths per million, linear ###
# create death_data DF with State, date, and sum of deaths per State by date to be used for population-weighted graph
global_deaths_per_million_data <- global_death_data %>%
  group_by(Country.Region, date) %>%
  summarize(total_deaths = sum(global_deaths / Population))

global_deaths_per_million_data <- rename(global_deaths_per_million_data, Province_State = Country.Region)

### start deaths per million plot, log ###
make_graphs_func(global_deaths_per_million_data, d_or_c = "d", lg_or_ln = "lg", standardized = TRUE, region_name = "Country")
### end deaths per million plot, log ###

### start deaths per million plot, linear ###
make_graphs_func(global_deaths_per_million_data, d_or_c = "d", lg_or_ln = "ln", standardized = TRUE, region_name = "Country")
### end deaths per million plot, linear ###

################################################################################
################################################################################


# create DF of recovered
global_recovered <- world_recovered %>%
  filter(Country.Region %in% countries) %>%
  select(-global_positions_to_remove)

date_columns <- colnames(global_recovered[, 2:ncol(global_recovered)])
global_recovered_data <- melt(global_recovered, id.vars = "Country.Region", measure.vars = date_columns)
global_recovered_data$variable<- str_replace(global_recovered_data$variable, "X", "")
global_recovered_data$variable <- mdy(global_recovered_data$variable)
global_recovered_data <- rename(global_recovered_data, date = variable, global_recovered = value)
# this line aggregates each country's numbers by date
# (some countries have multiple entries for each date due to provinces/regions)
# some day I will turn this into a function
# https://datascienceplus.com/aggregate-data-frame-r/
global_recovered_data <- aggregate(x = global_recovered_data[c("global_recovered")],
                               by = global_recovered_data[c("Country.Region", "date")],
                               FUN = function(smash_together) {
                                 sum(pmax(smash_together, 0))
                               }
)

# merge the country_pop table with the global_recovered_data table
global_recovered_data$Population <- country_pop$population[match(global_recovered_data$Country.Region, country_pop$country)]


global_confirmed <- world_confirmed %>%
  filter(Country.Region %in% countries) %>%
  select(-global_positions_to_remove)

date_columns <- colnames(global_confirmed[, 2:ncol(global_confirmed)])
global_confirmed_data <- melt(global_confirmed, id.vars = "Country.Region", measure.vars = date_columns)
global_confirmed_data$variable<- str_replace(global_confirmed_data$variable, "X", "")
global_confirmed_data$variable <- mdy(global_confirmed_data$variable)
global_confirmed_data <- rename(global_confirmed_data, date = variable, global_confirmed = value)
# this line aggregates each country's numbers by date
# (some countries have multiple entries for each date due to provinces/regions)
# some day I will turn this into a function
# https://datascienceplus.com/aggregate-data-frame-r/
global_confirmed_data <- aggregate(x = global_confirmed_data[c("global_confirmed")],
                                   by = global_confirmed_data[c("Country.Region", "date")],
                                   FUN = function(smash_together) {
                                     sum(pmax(smash_together, 0))
                                   }
)

# merge the country_pop table with the global_confirmed_data table
global_confirmed_data$Population <- country_pop$population[match(global_confirmed_data$Country.Region, country_pop$country)]

################################################################################
################################################################################

### plot deaths per million, linear ###
# create death_data DF with State, date, and sum of deaths per State by date to be used for population-weighted graph
global_confirmed_per_million_data <- global_confirmed_data %>%
  group_by(Country.Region, date) %>%
  summarize(total_confirmed = sum(global_confirmed / Population))

global_confirmed_per_million_data <- rename(global_confirmed_per_million_data, Province_State = Country.Region)

### start deaths per million plot, log ###
make_graphs_func(global_confirmed_per_million_data, d_or_c = "c", lg_or_ln = "lg", standardized = TRUE, region_name = "Country")
### end deaths per million plot, log ###

### start deaths per million plot, linear ###
make_graphs_func(global_confirmed_per_million_data, d_or_c = "c", lg_or_ln = "ln", standardized = TRUE, region_name = "Country")
### end deaths per million plot, linear ###

################################################################################
################################################################################






global_active_df <- global_confirmed_data
global_active_df$global_deaths <- global_death_data$global_deaths
global_active_df$global_recovered <- global_recovered_data$global_recovered
global_active_df <- global_active_df %>%
  group_by(Country.Region, date) %>% 
  summarize(global_active = global_confirmed - global_deaths - global_recovered)


US_active_today <- global_active_df %>% filter(Country.Region == "US" & date == Sys.Date() - 1)

countries_active_log <- global_active_df %>% 
  ggplot(aes(x = date, y = global_active, color = Country.Region)) +
  geom_point() +
  scale_color_manual(values = graph_colors, name = "Country") + # manually set the color to graph_colors
  geom_line() +
  ggtitle("Active Cases = Confirmed - Deaths - Recovered, [Logarithmic Scale]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Active Cases") + # name the y-axis
  xlab("Date") + # name the x-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-20")), linetype=3) +
  annotate("text", x = as.Date("2020-04-20"), y = 450000, label = "*", color = "Purple", size = 18) + # * on line
  annotate("text", x = as.Date("2020-01-24"), y = 5000000, label = "*", color = "purple", size = 18) + # * on note
  annotate("text", x = as.Date("2020-02-01"), y = 7500000, label = "= US 20 Apr: 669,903 Active Cases", color = "Purple", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2020-02-01"), y = 4100000, label = paste("   US Today: ", format(US_active_today$global_active, big.mark = ",", scientific = FALSE), " Active Cases", sep = ""), color = "Purple", size = 5, hjust = 0)

print_plot(countries_active_log, plot_title = "countries_active_log", pl_height = 643, pl_res = 97)

 #### end countries graph section ####

#### start US daily active, confirmed, and deaths section ####

# plot the US active per day
US_active_data <- global_active_df %>% filter(Country.Region == "US")
US_active_bar_to_plot <- mutate(US_active_data, US_active_delta = global_active - lag(global_active)) %>%
  filter(!is.na(US_active_delta))

US_active_bar_plot <- US_active_bar_to_plot %>%
  ggplot(aes(x = date, y = US_active_delta, fill = Country.Region)) +
  geom_bar(stat = "identity", color = "purple") +
  geom_smooth(color = "green") +
  scale_fill_manual(values = "turquoise", name = "Country") +
  ggtitle("COVID-19 Net New Active per Day") +
  theme(plot.title = element_text(hjust = 0.5)) + # centers the title at the top
  xlab("Date") +
  ylab("Net New Active Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-14")), linetype=3)

# plot the US confirmed per day
US_confirmed_data <- global_confirmed_data %>% filter(Country.Region == "US")
US_confirmed_bar_to_plot <- mutate(US_confirmed_data, US_confirmed_delta = global_confirmed - lag(global_confirmed)) %>%
  filter(!is.na(US_confirmed_delta))

US_confirmed_bar_plot <- US_confirmed_bar_to_plot %>%
  ggplot(aes(x = date, y = US_confirmed_delta, fill = Country.Region)) +
  geom_bar(stat = "identity", color = "plum") +
  geom_smooth(color = "blue") +
  scale_fill_manual(values = "#008080", name = "Country") +
  ggtitle("COVID-19 Confirmed Cases per Day") +
  theme(plot.title = element_text(hjust = 0.5)) + # centers the title at the top
  xlab("Date") +
  ylab("Number of Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-14")), linetype=3)

# plot the US deaths per day
US_death_data <- global_death_data %>% filter(Country.Region == "US")
US_deaths_bar_to_plot <- mutate(US_death_data, US_deaths_delta = global_deaths - lag(global_deaths)) %>%
  filter(!is.na(US_deaths_delta))

US_deaths_bar_plot <- US_deaths_bar_to_plot %>%
  ggplot(aes(x = date, y = US_deaths_delta, fill = Country.Region)) +
  geom_bar(stat = "identity", color = "blue") +
  geom_smooth(color = "blue") +
  scale_fill_manual(values = "orange", name = "Country") +
  ggtitle("COVID-19 Deaths per Day") +
  theme(plot.title = element_text(hjust = 0.5)) + # centers the title at the top
  xlab("Date") +
  ylab("Number of Deaths") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "7 days", minor_breaks = NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-14")), linetype=3)

# print three plots on one graph
US_daily_plot <- ggarrange(US_active_bar_plot, US_confirmed_bar_plot, US_deaths_bar_plot, ncol = 1, nrow = 3)
print_plot(US_daily_plot, plot_title = "US_daily", pl_width = 827, pl_height = 1286, pl_res = 97)

#### end US daily active, confirmed, and deaths section ####

#### Summary variables ####

sum_US_confirmed <- global_confirmed_data %>% filter(Country.Region == "US", date == Sys.Date())
sum_US_deaths <- global_deaths_data %>% filter(Country.Region == "US", date == Sys.Date())
sum_US_recovered <- global_recovered_data %>% filter(Country.Region == "US", date == Sys.Date())
#### end Summary variables ####


#### write tables ####
# note: this section removed from use, but wanted to keep this in case I
# want to use it again one day
# # calculate deaths per million aggregated to yesterday's date
# deaths_per_million_aggregated <- death_data %>%
#   group_by(Province_State, date) %>%
#   summarize(total_deaths = sum(deaths / Population)) %>% 
#   filter(date == Sys.Date() - 1) # -1 day because today's data may not be populated yet
# #filter(date == as.Date("2020-04-22"))
# 
# # write the daily deaths/million table to a .csv
# write.csv(deaths_per_million_aggregated, paste("Tables/deaths_per_million, ", Sys.Date(), ".csv", sep = ""))
# 
# # calculate confirmed per million aggregated to yes terday's date
# confirmed_per_million_aggregated <- confirmed_data %>%
#   group_by(Province_State, date) %>%
#   summarize(total_cases = sum(confirmed_cases / Population)) %>% 
#   filter(date == Sys.Date() - 1) # -1 day because today's data may not be populated yet
# #filter(date == as.Date("2020-04-22"))
# 
# # write the daily confirmed cases/million table to a .csv
# write.csv(confirmed_per_million_aggregated, paste("Tables/cases_per_million, ", Sys.Date(), ".csv", sep = ""))
# 
# 
# # calculate confirmed per million aggregated to yesterday's date
# global_confirmed_summary <- global_confirmed_data %>%
#   group_by(Country.Region, date) %>%
#   filter(date == Sys.Date() - 1) # -1 day because today's data may not be populated yet
# #filter(date == as.Date("2020-04-22"))
# 
# # write the daily confirmed cases/million table to a .csv
# write.csv(global_confirmed_summary, paste("Tables/global_confirmed_count, ", Sys.Date(), ".csv", sep = ""))