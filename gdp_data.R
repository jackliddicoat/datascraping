library(rvest) # required packages
library(dplyr)
library(tidyverse)
library(ggthemes)

# load data and select the tables we want
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita"
dat <- url %>% 
  read_html()
dat <- dat %>% 
  html_elements("table") %>% 
  html_table()
dat <- dat[[2]]
dat <- dat[-c(1),]

# we need to change the column names
v <- as.character(c(colnames(dat)))
colnames(dat) <- make.unique(v)

# now lets clean the data
data <- dat %>%
  rename(country_territory = `Country/Territory`,
         imf_est = `IMF[5][6]`,
         cia_est = `CIA[8][9][10]`,
         wb_est = `World Bank[7]`,
         region = `UN Region`,
         wb_year = `World Bank[7].1`,
         imf_year = `IMF[5][6].1`,
         cia_year = `CIA[8][9][10].1`) %>% 
  mutate(country_territory = gsub("\\*", "", country_territory),
         cia_est = gsub(",", "", cia_est),
         imf_est = gsub(",", "", imf_est),
         wb_est = gsub(",", "", wb_est),
         imf_year = as.numeric(imf_year),
         wb_year = as.numeric(wb_year),
         cia_year = as.numeric(cia_year)) %>% 
  mutate(cia_est = as.numeric(cia_est),
         imf_est = as.numeric(imf_est),
         wb_est = as.numeric(wb_est))

data %>% 
  group_by(region)

region_data <- data %>% 
  select(country_territory, region, imf_est, wb_est, cia_est) %>% 
  pivot_longer(cols = c("imf_est", "wb_est", "cia_est"), names_to = "est",
               values_to = "gdp_ppp", values_drop_na = TRUE)

region_data <- region_data %>% 
  group_by(region, est) %>% 
  dplyr::summarise(mean_gdp = mean(gdp_ppp))

region_data %>% 
  ggplot(aes(reorder(region, +mean_gdp), mean_gdp, fill = est)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_base() +
  theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top") +
  scale_y_continuous(labels = scales::dollar_format(),
                     limits = c(0, 60000)) +
  scale_fill_discrete(labels = c("CIA", "IMF", "World Bank"),
                      name = "Estimate") +
  labs(title = "Average GDP By Region")
