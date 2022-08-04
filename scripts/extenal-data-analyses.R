load("objects/all-objects.RData")

packages <- append(packages, c("lubridate", "ggokabeito", "ggthemes"))

librarian::shelf(packages)

# Road Fatalities ---------------------------------------------------------
road.fatalities.raw <- read_csv("data/2022-01-07_ARDD-Fatalities.csv", na = "-9") %>% clean_names()

start.date <- dmy("01-01-1990")
end.date <- dmy("01-12-2021")
  
road.fatalities <- road.fatalities.raw %>% 
  mutate(year.month = ym(str_c(year, ".", month))) %>% 
  filter(between(year.month, start.date, end.date))


road.fatalities %>% 
  group_by(year) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.2) +
  geom_smooth(method = "lm", alpha = .2) 
  # scale_x_continuous(breaks = seq(2000, 2020, 1))



# VIC Hospitalised Drivers ------------------------------------------------
vic.hosp.raw <- read_csv("data/dirago-clean.csv") %>% clean_names()

vic.hosp <- vic.hosp.raw %>% 
  mutate(substance = if_else(substance == "Meth", "Methamphetamine", substance))

vic.hosp %>% 
  filter(substance != "No_Substance") %>% 
  ggplot(aes(
    y = prop_drugs, 
    x = year_ending, 
    col = substance)) +
  theme_bw() +
  scale_color_okabe_ito() +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_line(size = 1, alpha = 0.3)
  # geom_line(data = filter(vic.hosp, substance == "Methamphetamine"),
  #            aes(
  #              x = year_ending,
  #              y = prop_drugs,
  #              col = substance
  #            ), alpha = 1, size = 1.5)


#### Might be better to just focus on the three most common recreational substances (Alcohol, THC and Methamphetamine)
vic.hosp.full <- read_csv("data/2009-2018_Dirago-Data.csv") %>% clean_names()

vic.hosp.rec <- vic.hosp.full %>% 
  mutate(substance = if_else(substance == "Meth", "Methamphetamine", substance),
         year_ending = factor(year_ending, levels = c(2009, seq(2014, 2018)))) %>% 
  filter(substance %in% c("Alcohol", "THC", "Methamphetamine"))

vic.hosp.rec %>% 
  ggplot(aes(
    y = prop_injured * 100, 
    x = year_ending, 
    fill = substance)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  scale_fill_solarized() +
  # scale_x_continuous(breaks = c(2009, 2014, 2015, 2016, 2017, 2018))+
  geom_col(position = "dodge", col = "black")
  # geom_line(size = 1)
# geom_line(data = filter(vic.hosp, substance == "Methamphetamine"),
#            aes(
#              x = year_ending,
#              y = prop_drugs,
#              col = substance
#            ), alpha = 1, size = 1.5)




# BAC related fatalities Victoria -----------------------------------------
vic.bac.fatal <- read_csv("data/1987-2016_victoria-road-fatalities_BAC-over.csv") %>% clean_names()

vic.bac.fatal %>% 
  ggplot(aes(x = year, y = fatalities)) +
  geom_col() +
  scale_x_continuous(breaks = c(1987, seq(1985, 2016, 5))) +
  labs(subtitle = "RBT became law in 1985")

