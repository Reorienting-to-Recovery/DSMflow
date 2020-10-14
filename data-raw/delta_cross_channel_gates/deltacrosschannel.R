library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)

flows_cfs %>%
  select(date, `Sutter Bypass`, `Yolo Bypass`) %>%
  gather(bypass, flow, -date) %>%
  filter(month(date) == 3) %>%
  ggplot(aes(x = date, y = flow, fill = bypass)) +
  geom_col(position = 'dodge') +
  theme_minimal()


gate <- read_csv('data-raw/DeltaCrossChannelGateOperations.csv') %>%
  mutate(Date = mdy(Date))


View(gate)

# use 2009 and on, that is when biological opinion went into effect
# double checking mike's rule set

start_end_dates <- gate %>%
  filter(year(Date) >= 2009) %>%
  mutate(opened = lag(Date),
         days_opened = as.numeric(Date - opened),
         month_opened = month(opened), month_closed = month(Date)) %>%
  filter(month_opened != month_closed, Action == 'Closed') %>%
  select(opened, closed = Date, days_opened)

start_date <- start_end_dates %>% pull(opened)
end_date <- start_end_dates %>% pull(closed)

dates <- purrr::map2(start_date, end_date, seq, 'day') %>% flatten_dbl() %>% as_date()

special_cases <- tibble(date = dates) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(days_opened = n()) %>%
  ungroup()

days_opened <- gate %>%
  filter(year(Date) >= 2009) %>%
  mutate(opened = lag(Date),
         days_opened = as.numeric(Date - opened),
         month_opened = month(opened), month_closed = month(Date),
         year = year(Date), month = month(Date)) %>%
  filter(month_opened == month_closed, Action == 'Closed') %>%
  select(year, month, days_opened) %>%
  bind_rows(special_cases) %>%
  arrange(year, month) %>%
  group_by(year, month) %>%
  summarise(days_opened = sum(days_opened)) %>%
  ungroup()

#by month
days_opened %>%
  mutate(year_abb = paste0("'", str_extract(year, '..$'))) %>%
  ggplot(aes(x = year_abb, y = days_opened)) +
  geom_col() +
  facet_wrap(~month, ncol = 3) +
  theme_minimal()

#by year
days_opened %>%
  mutate(month = factor(month, labels = month.abb[c(2, 5:12)])) %>%
  ggplot(aes(x = month, y = days_opened)) +
  geom_col() +
  facet_wrap(~year, ncol = 3) +
  theme_minimal()


days_opened %>%
  group_by(month) %>%
  summarise(mean = mean(days_opened), med = median(days_opened),
            min = min(days_opened), max = max(days_opened), total = n())
