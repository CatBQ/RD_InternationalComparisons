

# 001- DATA ----------------------------------------------------------

# version 0.1.0 
# prepare datasets for dashboard

# 1.0 LIBRARIES ------------------------------------------------------

# widgets
library(plotly)
library(maps)
library(gghighlight)
library(networkD3)
library(DT)
# core
library(scales)
library(janitor)
library(tidyverse)


# 2.0 DATA IMPORT ----------------------------------------------------

import_oecd <- read.csv("~/../../UKRI/R&I System Analysis - Documents/Balance of Funding/International Comparisons/Dashboard/ICD_Directory/MSTI_import.csv") %>%
  clean_names()

# 3.0 OECD DATA PROPERTIES -------------------------------------------

# column names
names(import_oecd)

# Print head
head(import_oecd) %>%
  as_tibble()

#  List of all different metrics into a spreadsheet
lst_msti_var <- import_oecd %>%
  select(msti_var, msti_variables) %>%
  distinct(msti_variables, .keep_all = TRUE) %>%
  as_tibble()
# there are 130 different metrics included

# Print list of all different countries
unique(import_oecd$country)

# Print list of years
unique(import_oecd$year)

# 4.0 OECD DATAFRAME -------------------------------------------------

# Data
oecd_dt <- import_oecd

# remove European Union + OECD
# filter data to 2015 onwards
oecd_dt <- oecd_dt %>%
  filter(!cou %in% c("EU27_2020", "OECD"),
         year != 2014)

# print data to double check
unique(oecd_dt$country)
unique(oecd_dt$year)

# change the country names

# Korea to South Korea
oecd_dt$country <- replace(oecd_dt$country,
                           oecd_dt$country == "Korea",
                           "South Korea")

# China () to China
oecd_dt$country <- replace(oecd_dt$country,
                           oecd_dt$country == "China (People's Republic of)",
                           "China")

# CT to Taiwan
oecd_dt$country <- replace(oecd_dt$country,
                           oecd_dt$country == "Chinese Taipei",
                           "Taiwan")

# SR to Slovakia
oecd_dt$country <- replace(oecd_dt$country,
                           oecd_dt$country == "Slovak Republic",
                           "Slovakia")

# print list of all countries (double check changes made above)
unique(oecd_dt$country)


# 5.0 GRAPHS ---------------------------------------------------------

# |- 5.1 world map ----

# get map world data
world_dt <- map_data("world")

# make changes to the world map data
# change region to country (clmn name)
# remove Antarctica
world_dt <- world_dt %>%
  rename(country = region) %>%
  filter(country != "Antarctica")
# change uk and usa to match the names in the oecd_dt
world_dt$country <- replace(world_dt$country,
                            world_dt$country == "UK",
                            "United Kingdom")

world_dt$country <- replace(world_dt$country,
                            world_dt$country == "USA",
                            "United States")

# subset of oecd data
# world map will look at GERD as a percentage of GDP
G_XGDP <-
  oecd_dt %>%
  filter(msti_var == "G_XGDP")

# round the value + change column name 
G_XGDP$value <- round(G_XGDP$value, digits = 2)

G_XGDP <- G_XGDP %>% 
  rename(percentage = value)

# join the 2 datasets - oecd + world
oecd_world_dt <- world_dt %>%
  left_join(G_XGDP, by = "country") %>%
  as_tibble()

# select columns
oecd_world_dt <- oecd_world_dt %>%
  select(long,
         lat,
         group,
         country,
         year,
         percentage) 



# world_heatmap
wld_htmap <-
  ggplotly(
    ggplot() +
      geom_polygon(data = oecd_world_dt,
                   aes(
                     x = long,
                     y = lat,
                     group = group,
                     fill = percentage
                   ),
                   na.rm= FALSE) +
      theme_minimal() +
      theme(axis.ticks = element_blank()) +
      labs(x = NULL, y = NULL)
  )

# |- 5.2 GERD Percentage -Page ----

# To provide further insights - I will include a line chart 
ggplotly(
  ggplot(
    data = G_XGDP,
    aes(
      x = year, y = percentage, colour = country
    )) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "none")
)

# create a data table output 
G_XGDP_output <- G_XGDP %>% 
  select(country, year, percentage)

# |- 5.3 GERD @ current PPP ----
# develop a line graph that shows the funding levels of GERD at PPP

G_PPP <-
  oecd_dt %>%
  filter(msti_var == "G_PPP") %>%
  select(msti_variables,
         country,
         year,
         value)

# print last availabe year of data
max(G_PPP$year) #2020

# geom_point chart
g_ppp_geompt <- 
  ggplotly(
    G_PPP %>%
      ggplot(mapping = aes(x = year,
                           y = value,
                           color = country),
             na.rm = TRUE) +
      geom_point() +
      scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "calendar year",
           y = "current PPP($)") 
  )

# |- 5.3 GERD Percentage by sectors ----
# build a sankey diagram 

# percentage of gerd finance by dtset 
G_XSects <-
  oecd_dt %>%
  filter(
    msti_variables %in% c(
      "Percentage of GERD financed by the business enterprise sector",
      "Percentage of GERD financed by government",
      "Percentage of GERD financed by the Higher Education and PNP sectors",
      "Percentage of GERD financed by the rest of the world"
    )
  ) %>%
  select(msti_variables,
         country,
         year,
         value) %>%
  as_tibble()

# create a new column
G_XSects <-
  G_XSects %>%
  mutate(
    sector = ifelse(
      msti_variables %in% "Percentage of GERD financed by the business enterprise sector",
      "business",
      ifelse(
        msti_variables %in% "Percentage of GERD financed by government",
        "government",
        ifelse(
          msti_variables %in% "Percentage of GERD financed by the Higher Education and PNP sectors",
          "higher education",
          "rest of the world"
        )
      )
    )
  )

# A connection data frame is a list of flows with intensity for each flow
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name = c(
  as.character(G_XSects$country),
  as.character(G_XSects$sector)
) %>%
  unique())

# With networkD3, connection must be provided using id, not using real name
#like in the links dataframe.. So we need to reformat it.
G_XSects$IDcountry <- match(G_XSects$country, nodes$name) - 1
G_XSects$IDsector <- match(G_XSects$sector, nodes$name) - 1
# Make the Network
G_XSects_sankey <- sankeyNetwork(
  Links = G_XSects,
  Nodes = nodes,
  Source = "IDsector",
  Target = "IDcountry",
  Value = "value",
  NodeID = "name",
  sinksRight = FALSE,
  fontSize = 14,
  nodeWidth = 30
)        

# |- 5.4 GERD per capita population (current PPP $) ----

# data table
G_XPOP <- oecd_dt %>%
  filter(msti_var == "G_XPOP") %>%
  select(msti_variables,
         country,
         year,
         value)

g_xpop_bc <-
  ggplotly(
    ggplot(G_XPOP, aes(country, value, fill = country)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = label_dollar(prefix = "", suffix = "$")) +
      labs(x = "country",
           y = "current PPP($)") +
      theme_minimal() +
      theme(axis.ticks = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(angle = 90)) 
  )











































