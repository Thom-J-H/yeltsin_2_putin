
# Yeltsin to Putin :: Data Visualize Script  ----------------------------------



# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(visdat)


# Data set ----------------------------------------------------------------

load(here::here("data", "tidy_data", "since_yeltsin.rda"))



# Visualization Prep ------------------------------------------------------

## Custom colors for Yeltsin and Putin
yelt <- c("orange", "navy")


std_cap <- "Data Humanist 2023, CC0 (Public Domain)"

std_x <- "Timeline from 1991 to 2021"


## Grab data for custom segments
since_yeltsin %>% filter(between(Year, 1999, 2000))




# Life Expectancy ---------------------------------------------------------


lex_title <- expression(italic("From Yeltsin to Putin:")~"Life Expectancy in Russia")

lex_sub <- expression(italic("Source:")~"http://gapm.io/dlex")


## y = 66.2, yend = 65.6 for lex data 1999, 2000



lex_graph <-  since_yeltsin %>%
  ggplot( aes(x = Year, y = lex) )+
  geom_segment( aes(x = 1999, xend = 2000, y = 66.2, yend = 65.6), 
                color = "navy" , linewidth = 1.2) +
  geom_line (aes(color = Leader), linewidth = 1.2) +
  scale_color_manual(values = yelt)+
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2022, by = 2))  +
  labs( title = lex_title, 
        subtitle = lex_sub,
        x = std_x,
        y = "Average Life Expectancy", 
        caption = std_cap )  +
  geom_vline(xintercept = 1999, color = "red", 
             lty = 2, alpha = 0.4)



lex_graph 




# GDP Per Capita ----------------------------------------------------------


gdp_title <- expression(italic("From Yeltsin to Putin:")~"GDP Per Capita in Russia")

gdp_sub <- expression(italic("Source:")~"https://data.worldbank.org/indicator/NY.GDP.PCAP.KD")

##  y = 4820, yend = 5320 for PPP 1999, 2000



gdp_graph <- since_yeltsin %>%
  ggplot( aes(x = Year, y = gdp_per_cap)) +
  geom_segment( aes(x = 1999, xend = 2000, y = 4820, yend = 5320), 
                color = "navy" , linewidth = 1.2) +
  geom_line (aes(color = Leader), linewidth = 1.2) +
  scale_color_manual(values = yelt) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2022, by = 2))  +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs( title = gdp_title ,
        subtitle = gdp_sub , 
        caption = std_cap, 
        x = std_x,
        y = "Constant 2010 US$", color = "Leader" )  +
  geom_vline(xintercept = 1999, color = "red", 
             lty = 2, alpha = 0.4)


gdp_graph 



# World Bank GNI PPP ------------------------------------------------------

gni_title <- expression(italic("From Yeltsin to Putin:")~"World Bank's GNI Per Capita for Russia")

gni_sub <- expression(italic("Source:")~"https://data.worldbank.org/indicator/NY.GNP.PCAP.PP.CD")


## y = 5680, yend = 6650 for GNI 1999, 2000


gni_graph <- since_yeltsin %>%
  ggplot( aes(x = Year, y = gnipercapita_ppp)) +
  geom_segment( aes(x = 1999, xend = 2000, y = 5680, yend = 6650), 
                color = "navy" , linewidth = 1.2) +
  geom_line (aes(color = Leader), linewidth = 1.2) +
  scale_color_manual(values = yelt) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2022, by = 2))  +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs( title = gni_title , subtitle = gni_sub ,
        x = std_x, caption = std_cap, 
        y= "International $", color = "Leader") +
  geom_vline(xintercept = 1999, color = "red", 
             lty = 2, alpha = 0.4)



gni_graph 



# Child Mortality ---------------------------------------------------------



mort_title <- expression(italic("From Yeltsin to Putin:")~"Child Mortality in Russia")

mort_sub <- expression(italic("Source:")~"http://gapm.io/du5mr")

##  y = 20.1, yend = 19.3 Child Mort for 1999, 2000



mort_graph <- since_yeltsin %>%
  ggplot( aes(x = Year, y = child_mort)) +
  geom_segment( aes(x = 1999, xend = 2000, y = 20.1, yend = 19.3), 
                color = "navy" , linewidth = 1.2) +
  geom_line (aes(color = Leader), linewidth = 1.2) +
  scale_color_manual(values = yelt) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2022, by = 2))  +
  labs( title = mort_title, subtitle = mort_sub,
        x = std_x, caption = std_cap,
        y= "Deaths Per 1000", color = "Leader") +
  geom_vline(xintercept = 1999, color = "red", 
             lty = 2, alpha = 0.4)



mort_graph 





# Save plots --------------------------------------------------------------

since_yeltsin_graphs <- c("mort_graph", 
                          "gni_graph",
                          "gdp_graph",
                          "lex_graph" )

save(list = since_yeltsin_graphs,
     file = here::here("data",
                       "tidy_data",
                       "since_yeltsin_graphs.rda" ) )


