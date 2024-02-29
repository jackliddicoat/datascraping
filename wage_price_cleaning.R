library(dplyr)
library(readxl)
library(rvest)

# Get the urls and the data sets
wage_url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_median_wage_and_mean_wage"
home_url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_by_median_home_price"
perm_percap <- read_excel("home_permits_state.xlsx", sheet = "Data")
fert <- read_excel("fertility.xlsx")

# clean the first data set
fert <- fert %>% 
  filter(year == 2021) %>% 
  select(fr, state)
fert$state <- state.name[match(fert$state,state.abb)] # this changes abbreviated states to their full names
fert$state

# clean this data
perm_percap <- perm_percap[-c(1:2),]
perm_percap <- perm_percap %>% 
  rename(state = `New home construction permits per capita in the U.S. 2021, by state`,
         permits = ...2)

# scrape the data from wikipedia
wage <- wage_url %>% 
  read_html()
# this gets all the tables on the page we read in
wage_tables <- wage %>% 
  html_elements("table") %>% 
  html_table()
# we want the third table, which shows median and average wages by state
wage <- wage_tables[[3]]
# cleaning the cols
wage_cleaned <- wage %>% 
  select(`Stateor territory`, `Median wage in US$[4]`) %>% 
  rename(state = `Stateor territory`,
         med_wage = `Median wage in US$[4]`)
wage_cleaned <- wage_cleaned[-c(52:56),]
# we need to make sure the wage col is a double not a character vector
wage_cleaned <- wage_cleaned %>% 
  mutate(med_wage = gsub("\\$","", med_wage)) %>% 
  mutate(med_wage = gsub("\\,","", med_wage)) %>% 
  mutate(med_wage = as.numeric(med_wage))

# same thing we did above
home <- home_url %>% 
  read_html()
home_tables <- home %>% 
  html_elements("table") %>% 
  html_table()

# we want the second table
home <- home_tables[[2]]

# cleaning
home_cleaned <- home %>%
  select(`State or territory`, `Median home price in US$[1]`) %>% 
  rename(state = `State or territory`, med_price = `Median home price in US$[1]`) %>% 
  mutate(med_price = gsub("\\$", "", med_price)) %>% 
  mutate(med_price = gsub("\\,", "", med_price)) %>% 
  mutate(med_price = as.numeric(med_price))

# right join all of these columns based on their state value
df <- right_join(home_cleaned, wage_cleaned)
df <- right_join(df, perm_percap, by = 'state')
df <- right_join(df, fert, by = 'state')

# make a new variable that weighs the median price of a home against the median wage
df <- df %>% 
  mutate(price_to_wage = med_price / med_wage)

df <- df[-c(52),]

df %>% 
  View()
