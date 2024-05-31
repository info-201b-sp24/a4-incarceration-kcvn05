library(tidyverse)
library(ggplot2)
library(dplyr)

## Load data frame ---- 
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# The functions might be useful for A4
source("~/Desktop/INFO201/a4-incarceration-kcvn05/source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function creates a list of the prison population from each year
get_year_jail_pop <- function(incarceration) {
  yearly_population <- incarceration %>%
    group_by(year) %>%
    summarise(total_population = sum(total_pop))
  return(yearly_population)
}
yearly_pop <- get_year_jail_pop(incarceration)

# This function creates a graph showing the US jail population from 1970 to 2018
plot_jail_pop_for_us <- function(yearly_pop)  {
  pop_chart <- ggplot(data = yearly_pop, aes(x = year, y = total_population)) +
    geom_line() +
    scale_y_continuous(
      labels = scales::comma,
      breaks = scales::pretty_breaks(n = 5),
                      ) +
    xlab("Year") +
    ylab("Population") +
    labs(
      title = "US Jail Population",
      subtitle = "1970-2018"
    )
  return(pop_chart)   
} 
print(plot_jail_pop_for_us(yearly_pop))

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#This function produces a dataset displaying the total number of incarcerated
#per year as well as the change between years.
change_state_pop <- function(incarceration) {
  growth <- incarceration %>%
    group_by(state, year) %>%
    summarize(num_incarcerated = sum(total_pop, na.rm = TRUE)) %>%
    mutate(change_in_pop = num_incarcerated - lag(num_incarcerated),
           change_in_pop = coalesce(change_in_pop, 0))
  return(growth) 
}
print(change_state_pop(incarceration))
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# The first function creates a new data frame that only has population data
#related to race and not age or other circumstances.
inequality_1 <- function(incarceration) {
  incarceration_ethnicities <- incarceration %>% select(2,4,5,6,28:33)
  ethnicities_cleaned <- na.omit(incarceration_ethnicities)
  return(ethnicities_cleaned)
}

#This function creates a new data frame with the total population for reference,
#and then the ratios of each race present in jails.
ethnicity_rates <- function(ethnicities_cleaned) {
  ethnicity_rated <- ethnicities_cleaned %>%
    mutate(
      aapi_jail_pop_ratio = aapi_jail_pop / total_pop,
      black_jail_pop_ratio = black_jail_pop / total_pop,
      latinx_jail_pop_ratio = latinx_jail_pop / total_pop,
      native_jail_pop_ratio = native_jail_pop / total_pop,
      white_jail_pop_ratio = white_jail_pop / total_pop
    ) %>%
    select(-aapi_jail_pop, -black_jail_pop, -latinx_jail_pop, -native_jail_pop, -white_jail_pop) %>%
  return(ethnicity_rated)
}

eth_rated <- ethnicity_rates(ethnicities_cleaned)

#Takes all the rates and averages them for each state per year
#I am sorry this is scuffed, number of states are wonky but I have no idea 
#if thats supposed to happen.
ethnicity_rates_states <-function(eth_rated) {
  rates_by_state <- eth_rated %>%
    group_by(state, year) %>%
    summarize(
      aapi_state_rate = mean(aapi_jail_pop_ratio),
      black_state_rate = mean(black_jail_pop_ratio),
      latinx_state_rate = mean(latinx_jail_pop_ratio),
      native_state_rate = mean(native_jail_pop_ratio),
      white_state_rate = mean(white_jail_pop_ratio)
    ) %>%
  return(rates_by_state)
}

summed_rates <- ethnicity_rates_states(eth_rated)

converted_data <- tidyr::pivot_longer(summed_rates, 
                                      cols = c(aapi_state_rate, black_state_rate, latinx_state_rate, native_state_rate, white_state_rate), 
                                      names_to = "ethnicity", 
                                      values_to = "rate") %>%
              filter(!is.infinite(rate))


eth_rate_graph <- ggplot(data = converted_data, aes(x = year, y = rate, color = ethnicity)) +
  geom_point(size = 2) +
  labs(
    subtitle = "1970 - 2018",
    x = "Year",
    y = "Rate",
    color = "Ethnicity"
  ) +
  theme_minimal() +
  scale_color_discrete(name = "Ethnicity", labels = c("AAPI", "BLACK", "LATINX","NATIVE", "WHITE")) +
  ggtitle("Ethnicity of Jail Population Rates Over Time") +
  theme(
    plot.title = element_text(size = 12)
  )

print(eth_rate_graph)
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#




