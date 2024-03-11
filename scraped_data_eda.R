# run the cleaning file before running this one !!!
library(ggplot2)
library(sjPlot)
library(ggpubr)
library(usmap)
library(PNWColors)
library(jtools)
library(patchwork)

theme_set(theme_sjplot2())

p1 <- df %>% 
  ggplot(aes(med_wage, med_price)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_sjplot2() +
  theme(axis.title.x = element_text(size = 10, hjust = .85),
        axis.title.y = element_text(size = 10, hjust = .85),
        plot.title = element_text(size = 12, face = 'bold')) +
  labs(y = "median price", x = "median wage",
       title = "Median Wage and Home Price, 2021")
# wages tend to be correlated with home prices, which makes sense

p2 <- df %>% 
  ggplot(aes(permits, med_price)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_sjplot2() +
  theme(axis.title.x = element_text(size = 10, hjust = .85),
        axis.title.y = element_text(size = 10, hjust = .85),
        plot.title = element_text(size = 12, face = 'bold')) +
  labs(y = "median price", x = "permits per 1,000 residents",
       title = "Housing Permits and Median Home Price, 2021")

p3 <- df %>% 
  ggplot(aes(med_price, fr)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_sjplot2() +
  theme(axis.title.x = element_text(size = 10, hjust = .85),
        axis.title.y = element_text(size = 10, hjust = .85),
        plot.title = element_text(size = 12, face = 'bold')) +
  labs(x = "median price", y = "births per 1,000 women",
       title = "Fertility Rate and Median Home Price, 2021")

p4 <- df %>% 
  ggplot(aes(price_to_wage, fr)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  # scale_x_continuous(labels = scales::dollar_format()) +
  theme_sjplot2() +
  theme(axis.title.x = element_text(size = 10, hjust = .85),
        axis.title.y = element_text(size = 10, hjust = .85),
        plot.title = element_text(size = 12, face = 'bold')) +
  labs(x = "home price-to-wage ratio", y = "births per 1,000 women",
       title = "Fertility Rate and Median Price to Wage Ratio, 2021")

p1 + p2 + p3 + p4

pal <- pnw_palette("Bay", n = 100, type = "continuous")
plot_usmap(regions = "state", values = "price_to_wage",
           data = df) +
  scale_fill_gradientn(colours = pal,
                       name = "ratio",
                       limits = c(1, 15)) +
  labs(title = "Median Home Price to Wage Ratio, 2021") +
  theme(plot.title = element_text(size = 11, face = "bold"))
  
# regressions
# price to wage index vs fertility rates
lm1 <- lm(fr ~ price_to_wage, data = df) 
summ(lm1)

# housing permitting vs fertility rates
lm2 <- lm(fr ~ permits, data = df) 
summ(lm2)

# log(wages) vs fertility rates
lm3 <- lm(fr ~ log(med_wage), data = df)
summ(lm3)
# very strong, negative relationship

df %>% 
  ggplot(aes(med_wage, fr)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_sjplot2() +
  theme(axis.title.x = element_text(size = 10, hjust = .85),
        axis.title.y = element_text(size = 10, hjust = .85),
        plot.title = element_text(size = 12, face = 'bold')) +
  labs(y = "births per 1,000 women", x = "median wage",
       title = "Median Wage and Fertility Rate, 2021")
