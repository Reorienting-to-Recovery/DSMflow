library(tidyverse)
library(lubridate)
library(stringr)
library(devtools)

cvpia_nodes <- read_csv('data-raw/calsim_run_of_river/cvpia_calsim_nodes.csv', skip = 1)
hab_col_need_split <- cvpia_nodes$calsim_habitat_flow %>% str_detect(', ')
habitat_split <- cvpia_nodes$calsim_habitat_flow[hab_col_need_split] %>% str_split(', ') %>% flatten_chr()
habitat_nodes <- c(cvpia_nodes$calsim_habitat_flow[!hab_col_need_split], habitat_split, 'C134', 'C160')[-20]

flow_col_need_split <- cvpia_nodes$cal_sim_flow_nodes %>% str_detect(', ')
div_split <- cvpia_nodes$cal_sim_flow_nodes[flow_col_need_split] %>% str_split(', ') %>% flatten_chr()
div_flow_nodes <- c(cvpia_nodes$cal_sim_flow_nodes[!flow_col_need_split], div_split)

div_col_need_split <- cvpia_nodes$cal_sim_diversion_nodes %>% str_detect(', ')
div_split <- cvpia_nodes$cal_sim_diversion_nodes[div_col_need_split] %>% str_split(', ') %>% flatten_chr()
div_nodes <- c(cvpia_nodes$cal_sim_diversion_nodes[!div_col_need_split], div_split)
diversion_nodes <- div_nodes[!is.na(div_nodes)] %>% str_trim('both') %>% str_replace(',', '')

# Add wilkins nodes
wilkins_nodes <- c("C126", "C128", "C129")

delta_nodes <- c('C400', 'C157', 'C401B', 'C417A', 'C504', 'C508', 'C639', 'C644', 'D403A',
                 'D403B', 'D403C', 'D403D', 'D404', 'D418', 'D419', 'D412', 'D410', 'D413',
                 'D409B', 'D416', 'D408_OR', 'D408_VC')

combined_flow_nodes <- c('C11305', 'C11301')

bypass_nodes <- c('D117', 'C135', 'C136A', 'C137', 'D160', 'D165A')

del_total_nodes <- c('DEL_SWP_TOTAL', 'DEL_CVP_TOTAL')

#combine all nodes to select columns
all_nodes <- c(habitat_nodes, div_flow_nodes, diversion_nodes, delta_nodes, combined_flow_nodes, bypass_nodes, wilkins_nodes, del_total_nodes,'X2') %>% unique()

pick_columns <- function(file, nodes) {
  col_nm <- readxl::read_excel(paste0('data-raw/calsim_run_of_river/max_flow_data/CalSim/', file), skip = 1) %>% names()
  temp <- readxl::read_excel(paste0('data-raw/calsim_run_of_river/max_flow_data/CalSim/', file), skip = 7, col_names = col_nm)

  desired_nodes <- col_nm %in% nodes

  filter <- temp |>
    rename(date = `...2`) |>
    select(date, col_nm[desired_nodes]) |>
    mutate(date = as.Date(date)) |>
    filter(year(date) <= 2003)

  cleaned <- temp |>
    rename(date = `...2`) |>
    select(date, col_nm[desired_nodes]) |>
    filter(year(date) > 2003) |>
    mutate(date = as.Date(date)-lubridate::years(100)) |>
    bind_rows(filter) |>
    filter(year(date) >= 1922, year(date) <= 2002)

  return(cleaned)
}

file_names <- list.files('data-raw/calsim_run_of_river/max_flow_data/CalSim/', '*.xlsx')
# cvpia_calsim <- map_dfc(file_names, pick_columns, all_nodes) %>%
#   select(-date1, -date2, -date3, -date4, -date5, -date6)
run_of_river_r2r_calsim <- map(file_names, pick_columns, all_nodes) %>%
  reduce(full_join, by = 'date')


write_rds(run_of_river_r2r_calsim, 'data-raw/calsim_run_of_river/run_of_river_r2r_calsim.rds')

# testing things to pick flow nodes------------------------------------
comparison_nodes <- c('C157', 'D160', 'D166A', 'D117', 'D124', 'D125', 'D126', 'C166', 'C165', 'X2', 'C134', 'C160')
ttt <- map_df(file_names, pick_columns, comparison_nodes)
testnodes <- ttt %>%
  gather(node, flow, -date) %>%
  filter(!is.na(flow)) %>%
  mutate(flow = as.numeric(flow)) %>%
  spread(node, flow)

# Yolo: C157 vs. D160+D166a (use C157)
testnodes %>%
  select(date, C157, D160, D166A) %>%
  mutate(`D160 + D166A` = D160 + D166A) %>%
  select(-D160, -D166A) %>%
  gather(nodes, flow, -date) %>%
  filter(year(date) >= 1980, year(date) < 1990) %>%
  ggplot(aes(x = date, y = flow, color = nodes)) +
  geom_line() +
  theme_minimal()

# Sutter: D117 vs. D124 Vs. D125 vs. D126
testnodes %>%
  select(date, D117, D124, D125, D126) %>%
  gather(spill, flow, -date) %>%
  filter(year(date) >= 1980, year(date) < 2000) %>%
  ggplot(aes(x = date, y = flow, fill = spill)) +
  geom_col(position = 'dodge') +
  # geom_col() +
  theme_minimal()

# Lower-Mid Sacramento: C160 vs. C134 (use both in proporition of stream length above and below fremont weir for habitat)
testnodes %>%
  select(date, C160, C134) %>%
  gather(nodes, flow, -date) %>%
  filter(year(date) >= 1980, year(date) < 2000) %>%
  ggplot(aes(x = date, y = flow, color = nodes)) +
  geom_line() +
  theme_minimal()

low_mid <- testnodes %>%
  select(date, C160, C134) %>%
  gather(nodes, flow, -date) %>%
  filter(year(date) >= 1980, year(date) < 2000)

library(DSMhabitat)
lower_mid_sacramento_river_floodplain %>% View()
# lmfp <- lower_mid_sacramento_river_floodplain_approx('fr')
#
# low_mid %>%
#   mutate(floodplain = lmfp(flow)) %>%
#   select(-flow) %>%
#   spread(nodes, floodplain) %>%
#   mutate(floodplain = 35.6/58*C134 + 22.4/58*C160) %>%
#   ggplot(aes(x = date, y = floodplain)) +
#   geom_col() +
#   theme_minimal()

testnodes %>%
  select(date, D160, D166A) %>%
  gather(nodes, flow, -date) %>%
  filter(year(date) >= 1980, year(date) < 1990) %>%
  ggplot(aes(x = date, y = flow, fill = nodes)) +
  geom_col(position = 'dodge') +
  theme_minimal()
