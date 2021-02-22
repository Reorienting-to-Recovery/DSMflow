library(tidyverse)
library(lubridate)
library(stringr)
library(devtools)

# Explore Calsim node C126, C128, and C129
wilkins_slough_nodes <- c("C126", "C128", "C129")

# ANd freeport node for comparison
freeport_node <- "C400"

# Read in nodes
column_names <- read_csv('data-raw/MikeWrightCalSimOct2017/C1_C169.csv', skip = 1) %>% names()
temp_flow_data <- read_csv('data-raw/MikeWrightCalSimOct2017/C1_C169.csv', skip = 7, col_names = column_names)

wilkins_cleaned_nodes <- temp_flow_data %>%
  select(date = X2, wilkins_slough_nodes) %>%
  mutate(date = dmy(date)) %>%
  filter(year(date) >= 1922, year(date) <= 2002)

# Visualize wilkins nodes to decide which one makes the most sense to use

wilkins_cleaned_nodes %>%
  gather(spill, flow, -date) %>%
  filter(year(date) >= 1980, year(date) < 2000) %>%
  ggplot(aes(x = date, y = flow, fill = spill)) +
  geom_col(position = 'dodge') +
  theme_minimal()

write_rds(wilkins_cleaned_nodes, 'data-raw/MikeWrightCalSimOct2017/wilkins_node.rds')

# Compare to freeport
# Baseline data contains same calsim data as Wright (just in tidy format)
baseline_data <- read_csv("data-raw/delta-dsm-calsim/FullObsJul18_NoNotch_Base_DV_filtered.csv")
freeport_node <- c("C400")
freeport_node <- baseline_data %>%
  filter(
    year(Date_Time) >= 1980, year(Date_Time) <= 1999,
    Variable == "C400") %>%
  select(Date_Time, Value)

# Graphical comparison of wilkins vs freeport nodes shows freeport C400 has much higher flows than wilkins C126, C128, C129
wilkins_vs_freeport_nodes <- wilkins_cleaned_nodes %>%
  left_join(freeport_node, by = c("date" = "Date_Time")) %>%
  rename("C400" = Value)  %>%
  gather(spill, flow, -date) %>%
  filter(year(date) >= 1980, year(date) < 2000) %>%
  ggplot(aes(x = date, y = flow, fill = spill)) +
  geom_col(position = 'dodge') +
  theme_minimal()
wilkins_vs_freeport_nodes

# Create matrix with wilkins flow node to replace freeport_flows
# I used node C128 for wilkins(all are very similar but this one was in the middle)
wilkins_flow <- wilkins_cleaned_nodes %>%
  select(date, "C128") %>%
  filter(year(date) >= 1980, year(date) <= 1999) %>%
  transmute(
    year = year(date),
    month = month(date),
    wilklinsQcfs = C128,
    wilkinsQcms = cfs_to_cms(C128))  %>%
  select(year, month, wilkinsQcms) %>%
  spread(year, wilkinsQcms) %>%
  select(-month) %>%
  as.matrix()

# View wilkins flow matrix.
wilkins_flow
rownames(wilkins_flow) <- month.abb

wilkins_flow

