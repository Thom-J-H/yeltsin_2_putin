# Yeltsin to Putin :: Data Wrangle Script 


# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(visdat)


# Raw Data from Gapminder -------------------------------------------------



# GDP per capita ----------------------------------------------------------


## Notes data set one
# https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
# constant 2010 USD

# import
gdp_per_cap <- read_csv(file = here::here("data", 
                                          "raw_data", 
                                          "gdppercapita_us_inflation_adjusted.csv") )


# check
gdp_per_cap  %>% vis_dat()

# start Tidy process
gdp_per_cap  <- gdp_per_cap  %>%
  mutate(across(everything(), as.character))


# wide to long pivot
gdp_per_cap_tidy <- gdp_per_cap %>%
  pivot_longer(cols = !country,
               names_to = "Year",
               names_transform = list(Year = as.integer),
               values_to = "gdp_per_cap")

# check
gdp_per_cap_tidy %>% glimpse()


# convert the text to numbers: k = 1000
gdp_per_cap_tidy$gdp_per_cap <- case_when(stringr::str_detect(gdp_per_cap_tidy$gdp_per_cap, 'k') ~ readr::parse_number(gdp_per_cap_tidy$gdp_per_cap) * 1000,
                                          TRUE ~ parse_number(gdp_per_cap_tidy$gdp_per_cap) 
)


# check
gdp_per_cap_tidy %>% vis_dat()


gdp_per_cap_tidy$Year %>% range()


# save
save(gdp_per_cap_tidy, 
     file = here::here("data", 
                       "tidy_data", 
                       "gdp_per_cap_tidy.rda"))




# Life Expectancy ---------------------------------------------------------

## Notes data set two
# http://gapm.io/dlex
# https://www.gapminder.org/data/documentation/gd004/


# import
lex <- read_csv(file = here::here("data", 
                                  "raw_data", 
                                  "lex.csv") )


# check
lex  %>% vis_dat()

# no character issues?

# start Tidy process
# wide to long pivot
lex_tidy <- lex %>%
  pivot_longer(cols = !country,
               names_to = "Year",
               names_transform = list(Year = as.integer),
               values_to = "lex")

# check
lex_tidy   %>% glimpse()

lex_tidy$Year %>% range()

# save
save(lex_tidy, 
     file = here::here("data", 
                       "tidy_data", 
                       "lex_tidy.rda"))


# Child Mortality ---------------------------------------------------------

## Notes data set three
# http://gapm.io/du5mr
# Child mortality under age 5,

# import
child_mort <- read_csv(file = here::here("data", 
                                         "raw_data", 
                                         "child_mortality_0_5_year_olds_dying_per_1000_born.csv") )



child_mort %>% vis_dat()


# start Tidy process
# wide to long pivot
child_mort_tidy <- child_mort%>%
  pivot_longer(cols = !country,
               names_to = "Year",
               names_transform = list(Year = as.integer),
               values_to = "child_mort")

# check
child_mort_tidy   %>% glimpse()
child_mort_tidy %>% vis_dat()




# save
save(child_mort_tidy, 
     file = here::here("data", 
                       "tidy_data", 
                       "child_mort_tidy.rda"))







# GNI per capita ----------------------------------------------------------

## Notes data set four
# https://data.worldbank.org/indicator/NY.GNP.PCAP.PP.CD/countries
# GNI per cap based on PPP


# import
gnipercapita_ppp <- read_csv(file = here::here("data", 
                                               "raw_data", 
                                               "gnipercapita_ppp_current_international.csv") )

gnipercapita_ppp %>% vis_dat()



# start Tidy process
# wide to long pivot

gnipercapita_ppp_tidy <- gnipercapita_ppp%>%
  pivot_longer(cols = !country,
               names_to = "Year",
               names_transform = list(Year = as.integer),
               values_to = "gnipercapita_ppp")

# check
gnipercapita_ppp_tidy   %>% glimpse()
gnipercapita_ppp_tidy %>% vis_dat()


## if needed

gnipercapita_ppp_tidy$gnipercapita_ppp <- case_when(stringr::str_detect(gnipercapita_ppp_tidy$gnipercapita_ppp, 'k') ~ readr::parse_number(gnipercapita_ppp_tidy$gnipercapita_ppp) * 1000,
                                                   TRUE ~ parse_number(gnipercapita_ppp_tidy$gnipercapita_ppp) 
)


# check
gnipercapita_ppp_tidy %>% vis_dat()


gnipercapita_ppp_tidy$Year %>% range()


# save
save(gnipercapita_ppp_tidy, 
     file = here::here("data", 
                       "tidy_data", 
                       "gnipercapita_ppp_tidy.rda"))




# Make the graphing data set ----------------------------------------------

# From 1991 to 2021, Russia


yelt_gdp <- gdp_per_cap_tidy %>%
  filter(country == "Russia") %>%
  filter(between(Year, 1991, 2021))

yelt_gni <- gnipercapita_ppp_tidy %>%
  filter(country == "Russia") %>%
  filter(between(Year, 1991, 2021))

yelt_lex <- lex_tidy %>%
  filter(country == "Russia") %>%
  filter(between(Year, 1991, 2021))

yelt_mort <- child_mort_tidy %>%
  filter(country == "Russia") %>%
  filter(between(Year, 1991, 2021))


comp_data_tidy <-  yelt_mort %>%
  left_join(gdp_per_cap_tidy, by = c("country", "Year") ) %>%
  left_join(pop_tidy_1960 , by = c("country", "Year") ) %>%
  left_join(lex_tidy_1960 , by = c("country", "Year") ) %>%
  left_join(gnipercapita_ppp_tidy , by = c("country", "Year") ) 


since_yeltsin <- yelt_gdp %>%
  left_join(yelt_gni , by = c("country", "Year") ) %>%
  left_join(yelt_lex  , by = c("country", "Year") ) %>%
  left_join(yelt_mort , by = c("country", "Year") ) 


since_yeltsin <- since_yeltsin %>%
  mutate(Leader = case_when(between(Year, 1991, 1999) ~  "Yeltsin",
                          between(Year, 2000, 2021) ~  "Putin",
                          TRUE ~ "Other")   )


since_yeltsin$Leader <- factor(since_yeltsin$Leader, 
                               levels = c("Yeltsin", "Putin" ) )


vis_dat(since_yeltsin)

##
save(since_yeltsin, 
     file = here::here("data", 
                       "tidy_data", 
                       "since_yeltsin.rda"))

