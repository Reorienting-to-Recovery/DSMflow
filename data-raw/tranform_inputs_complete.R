library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(readxl)
library(purrr)

source('R/utils.R')

# watershed ordering --------
watershed_ordering <- read_csv('data-raw/watershed_ordering.csv')
usethis::use_data(watershed_ordering, overwrite = TRUE)

# calsim prep (all versions) ---------------------------------------------------
cvpia_nodes <- read_csv('data-raw/calsim_2008_2009/MikeWrightCalSimOct2017/cvpia_calsim_nodes.csv', skip = 1)
watersheds <- cvpia_nodes$watershed

need_split_habitat <- cvpia_nodes$calsim_habitat_flow %>% str_detect(', ')
habitat_split <- cvpia_nodes$calsim_habitat_flow[need_split_habitat] %>% str_split(', ') %>% flatten_chr()
habitat_node <- c(cvpia_nodes$calsim_habitat_flow[!need_split_habitat], habitat_split, 'C134', 'C160')[-20]

# Flow Function ----------------------------------------------------------------
generate_flow_cfs <- function(calsim_data, nodes){
  node_columns <- names(calsim_data) %in% c(nodes, 'date')

  flow_calsim <- calsim_data[, node_columns]

  flow <- flow_calsim %>%
    mutate(`Upper Sacramento River` = C104,
           `Antelope Creek` = C11307,
           `Battle Creek` = C10803,
           `Bear Creek` = C11001,
           `Big Chico Creek` = C11501,
           `Butte Creek` = C217A,
           `Clear Creek` = C3,
           `Cottonwood Creek` = C10802,
           `Cow Creek` = C10801,
           `Deer Creek` = C11309,
           `Elder Creek` = C11303,
           `Mill Creek` = C11308,
           `Paynes Creek` = C11001,
           `Stony Creek` = C142A,
           `Thomes Creek` = C11304,
           `Upper-mid Sacramento River` = C115,
           `Bear River` = C285,
           `Feather River` = C203,
           `Yuba River` = C230,
           `Lower-mid Sacramento River1` = C134, # low-mid habitat = 35.6/58*habitat(C134) + 22.4/58*habitat(C160),
           `Lower-mid Sacramento River2` = C160,
           `American River` = C9,
           `Lower Sacramento River` = C166,
           `Calaveras River` = C92,
           `Cosumnes River` = C501,
           # `Mokelumne River` = ?, # TODO figure out what to do with Moke with new calsim
           `Merced River` = C561,
           `Stanislaus River` = C520,
           `Tuolumne River` = C540,
           `San Joaquin River` = C630) %>%
    select(date, `Upper Sacramento River`:`San Joaquin River`)
  return(flow)
}

# Original (2008 - 2009 BiOp) ------------------------------------------------
calsim_2008_2009 <- read_rds('data-raw/calsim_2008_2009/MikeWrightCalSimOct2017/cvpia_calsim.rds')

flow_2008_2009 <- generate_flow_cfs(calsim_data = calsim_2008_2009, nodes = habitat_node)

# testing Moke flows from exteranl model to calsim II - C503 vs 04-501
# moke_test <- read_excel('data-raw/calsim_2008_2009/EBMUDSIM/CVPIA_SIT_Data_RequestEBMUDSIMOutput_ExCond.xlsx', sheet = 'Tableau Clean-up') %>%
#   mutate(date = as_date(Date), C503...11) %>%
#   select(date, C503 = C503...11) %>% glimpse()
#
# c501_504 <- read_csv('data-raw/calsim_2008_2009/MikeWrightCalSimOct2017/C422-C843.csv', skip = 1) %>%
#   select(date = `...2`, C504, C501) %>%
#   filter(!is.na(date)) %>%
#   mutate(date = dmy(date)) %>% glimpse()
#
# moke_test %>%
#   left_join(c501_504) %>%
#   mutate(calsim = as.numeric(C504) - as.numeric(C501)) %>%
#   select(date, ebmudsim = C503, calsim) %>%
#   gather(model, flow, -date) %>%
#   filter(year(date) >= 1980, year(date) < 2000) %>%
#   ggplot(aes(x = date, y = flow, color = model)) +
#   geom_line() +
#   theme_minimal() +
#   theme(text = element_text(size = 18))
# #looks great

# bring in Moke flow from other model run
moke <- read_excel('data-raw/calsim_2008_2009/EBMUDSIM/CVPIA_SIT_Data_RequestEBMUDSIMOutput_ExCond.xlsx',
                   sheet = 'Tableau Clean-up') %>%
  mutate(date = as_date(Date), `Mokelumne River` = C91) %>%
  select(date, `Mokelumne River`)

# tributary and mainstem habitat flow ------------------------------------------
# TODO once we add Moke into model update to remove moke column before joining new one
flows_cfs_2008_2009 <- flow_2008_2009 %>%
  left_join(moke) %>%
  select(date:`Cosumnes River`, `Mokelumne River`, `Merced River`:`San Joaquin River`)

# Add in new calsim run (2018 - 2019 Biop/Itp) data-----------------------------
calsim_2019_biop_itp <- read_rds('data-raw/calsim_2019_BiOp_ITP/biop_cvpia_calsim.rds')

flow_cfs_2019_biop_itp <- generate_flow_cfs(calsim_data = calsim_2019_biop_itp, nodes = habitat_node)

# create flow_cfs with both 2008-2009 biop and 2018-2019 biop/itp
flows_cfs <- list(biop_2008_2009 = flows_cfs_2008_2009,
                 biop_itp_2018_2019 = flow_cfs_2019_biop_itp # missing moke
)

# Write flow cfs data object
usethis::use_data(flows_cfs, overwrite = TRUE)

# bypasses habitat flow --------------------------------------------------------
generate_bypass_flows <- function(calsim_run) {
  bypass_flows <- calsim_run %>%
    select(date,
           sutter1 = D117,
           sutter2 = C135,
           sutter3 = C136A,
           sutter4 = C137,
           yolo1 = D160,
           yolo2 = C157) %>%
    mutate(sutter2 = sutter2 + sutter1,
           sutter3 = sutter3 + sutter2,
           sutter4 = sutter4 + sutter3,
           yolo2 = yolo2 + yolo1)
  return(bypass_flows)
}

bypass_2008_2009 <-  generate_bypass_flows(calsim_run = calsim_2008_2009)
bypass_2019_biop_itp <- generate_bypass_flows(calsim_run = calsim_2019_biop_itp)

# create bypass flows with both 2008-2009 biop and 2018-2019 biop/itp
bypass_flows <- list(biop_2008_2009 = bypass_2008_2009,
                     biop_itp_2018_2019 = bypass_2019_biop_itp # missing moke
)

usethis::use_data(bypass_flows, overwrite = TRUE)

# diversions -------
need_split <- cvpia_nodes$cal_sim_flow_nodes %>% str_detect(', ')
div_split <- cvpia_nodes$cal_sim_flow_nodes[need_split] %>% str_split(', ') %>% flatten_chr()
div_flow_nodes <- c(cvpia_nodes$cal_sim_flow_nodes[!need_split], div_split)

need_split <- cvpia_nodes$cal_sim_diversion_nodes %>% str_detect(', ')
div_split <- cvpia_nodes$cal_sim_diversion_nodes[need_split] %>% str_split(', ') %>% flatten_chr()
div_nodes <- c(cvpia_nodes$cal_sim_diversion_nodes[!need_split], div_split)
diversion_nodes <- div_nodes[!is.na(div_nodes)] %>% str_trim('both') %>% str_replace(',', '')

combined_flow_nodes <- c('C11305', 'C11301')

all_div_nodes <- c(div_flow_nodes, diversion_nodes, combined_flow_nodes, 'date') %>% unique()
all_div_nodes
node_columns <- names(calsim) %in% all_div_nodes

div_calsim <- calsim[, node_columns]

# total diverted------------------------------------
temp_diver <- div_calsim %>%
  mutate(`Upper Sacramento River` = D104,
         `Antelope Creek` = ifelse(C11307 == 0, 0, (C11307 / (C11307 + C11308 + C11309) * D11305)),
         `Battle Creek` = NA,
         `Bear Creek` = NA,
         `Big Chico Creek` = NA,
         `Butte Creek` = (C217B + D217),
         `Clear Creek` = NA,
         `Cottonwood Creek` = NA,
         `Cow Creek` = NA,
         `Deer Creek` = ifelse(C11309 == 0 ,0, (C11309 / (C11307 + C11308 + C11309) * D11305)),
         `Elder Creek` = ifelse(C11303 == 0, 0, (C11303 / (C11303 + C11304) * D11301)),
         `Mill Creek` = ifelse(C11308 == 0, 0, (C11308 / (C11307 + C11308 + C11309) * D11305)),
         `Paynes Creek` = NA,
         `Stony Creek` = D17301,
         `Thomes Creek` = ifelse(C11304 == 0, 0, (C11304 / (C11303 + C11304) * D11301)),
         `Upper-mid Sacramento River` = (D109 + D112 + D113A + D113B + D114 + D118 + D122A + D122B
                                         + D123 + D124A + D128_WTS + D128),
         `Sutter Bypass` = NA,
         `Bear River` = D285,
         `Feather River` = (D201 + D202 + D7A + D7B),
         `Yuba River` = D230,
         `Lower-mid Sacramento River` = (D129A + D134 + D162 + D165),
         `Yolo Bypass` = NA,
         `American River` = D302,
         `Lower Sacramento River` = (D167 + D168 + D168A_WTS),
         `Calaveras River` = (D506A + D506B + D506C + D507),
         `Cosumnes River` = NA,
         # `Mokelumne River` = NA, # other run from mike U ebmud
         `Merced River` = (D562 + D566),
         `Stanislaus River` = D528,
         `Tuolumne River` = D545,
         `San Joaquin River` = (D637 + D630B + D630A + D620B)) %>%
  select(date, watersheds[-27])

# bring in Moke diversions from other model run
moke <- read_excel('data-raw/calsim_2008_2009/EBMUDSIM/CVPIA_SIT_Data_RequestEBMUDSIMOutput_ExCond.xlsx',
                   sheet = 'Tableau Clean-up') %>%
  mutate(date = as_date(Date), `Mokelumne River` = (D503A + D503B + D503C + D502A + D502B)) %>%
  select(date, `Mokelumne River`)

total_diverted <- temp_diver %>%
  left_join(moke) %>%
  select(date:`Cosumnes River`, `Mokelumne River`, `Merced River`:`San Joaquin River`) %>%
  filter(year(date) >= 1980, year(date) <= 2000) %>%
  gather(watershed, tot_diver, -date) %>%
  mutate(tot_diver = DSMflow::cfs_to_cms(tot_diver)) %>%
  spread(date, tot_diver) %>%
  left_join(DSMflow::watershed_ordering) %>%
  mutate_all(~replace_na(., 0)) %>%
  arrange(order) %>%
  select(-watershed, -order) %>%
  create_model_array()

dimnames(total_diverted) <- list(watershed_ordering$watershed, month.abb[1:12], 1980:2000)
usethis::use_data(total_diverted, overwrite = TRUE)

temp_prop_diver <- div_calsim %>%
  mutate(`Upper Sacramento River` = D104 / C104,
         `Antelope Creek` = (C11307 / (C11307 + C11308 + C11309) * D11305) / C11307,
         `Battle Creek` = NA,
         `Bear Creek` = NA,
         `Big Chico Creek` = NA,
         `Butte Creek` = (C217B + D217) / (C217B + D217 + C217A),
         `Clear Creek` = NA,
         `Cottonwood Creek` = NA,
         `Cow Creek` = NA,
         `Deer Creek` = (C11309 / (C11307 + C11308 + C11309) * D11305) / C11309,
         `Elder Creek` = (C11303 / (C11303 + C11304) * D11301) / C11303,
         `Mill Creek` = (C11308 / (C11307 + C11308 + C11309) * D11305) / C11308,
         `Paynes Creek` = NA,
         `Stony Creek` = D17301 / C42,
         `Thomes Creek` = (C11304 / (C11303 + C11304) * D11301) / C11304,
         `Upper-mid Sacramento River` = (D109 + D112 + D113A + D113B + D114 + D118 + D122A + D122B
                                         # + D122_EWA  #not in baseline calsim run
                                         # + D122_WTS  #not in baseline calsim run
                                         # + D128_EWA  #not in baseline calsim run
                                         + D123 + D124A + D128_WTS + D128) / C110,
         `Sutter Bypass` = NA,
         `Bear River` = D285 / (C285 + D285),
         `Feather River` = (D201 + D202 + D7A + D7B) / C6,
         `Yuba River` = D230 / (C230 + D230),
         `Lower-mid Sacramento River` = (D129A + D134 + D162 + D165) / C128, # D165A does not exist
         `Yolo Bypass` = NA,
         `American River` = D302 / C9,
         `Lower Sacramento River` = (D167 + D168 + D168A_WTS) / C166,
         `Calaveras River` = (D506A + D506B + D506C + D507) / C92,
         `Cosumnes River` = NA,
         # `Mokelumne River` = NA, # external model
         `Merced River` = (D562 + D566) / C561,
         `Stanislaus River` = D528 / C520,
         `Tuolumne River` = D545 / C540,
         `San Joaquin River` = (D637 + D630B + D630A + D620B) / (D637 + D630B + D630A + D620B + C637)) %>%
  select(date, watersheds[-27]) %>%
  gather(watershed, prop_diver, -date) %>%
  mutate(prop_diver = round(prop_diver, 6),
         prop_diver = case_when(
           is.infinite(prop_diver) ~ 0,
           is.nan(prop_diver) ~ 0,
           prop_diver > 1 ~ 1,
           TRUE ~ prop_diver
         )) %>%
  spread(watershed, prop_diver)

# bring in Moke diversions from other model run
moke <- read_excel('data-raw/calsim_2008_2009/EBMUDSIM/CVPIA_SIT_Data_RequestEBMUDSIMOutput_ExCond.xlsx', sheet = 'Tableau Clean-up') %>%
  mutate(date = as_date(Date), `Mokelumne River` = (D503A + D503B + D503C + D502A + D502B) / C91) %>%
  select(date, `Mokelumne River`)

proportion_diverted <- temp_prop_diver %>%
  left_join(moke) %>%
  filter(year(date) >= 1980, year(date) <= 2000) %>%
  gather(watershed, prop_diver, -date) %>%
  spread(date, prop_diver) %>%
  left_join(DSMflow::watershed_ordering) %>%
  mutate_all(~replace_na(., 0)) %>%
  arrange(order) %>%
  select(-watershed, -order) %>%
  create_model_array()

dimnames(proportion_diverted) <- list(watershed_ordering$watershed, month.abb[1:12], 1980:2000)

usethis::use_data(proportion_diverted, overwrite = TRUE)

# misc flow nodes ----
cs <- read_csv('data-raw/calsim_2008_2009/MikeWrightCalSimOct2017/C1_C169.csv', skip = 1) %>%
  select(date = X2, C134, C165, C116, C123, C124, C125, C109) %>%
  filter(!is.na(date)) %>%
  mutate(date = dmy(date))

ds <- read_csv('data-raw/calsim_2008_2009/MikeWrightCalSimOct2017/D100_D403.csv', skip = 1) %>%
  select(date = X2, D160, D166A, D117, D124, D125, D126) %>%
  filter(!is.na(date)) %>%
  mutate(date = dmy(date))

misc_flows <- left_join(cs, ds) %>%
  gather(node, flow, -date) %>%
  filter(!is.na(flow)) %>%
  mutate(flow = as.numeric(flow)) %>%
  spread(node, flow)

# tributary --------------
# using bypass node that is activated the most for meanQ
bypass <- DSMflow::bypass_flows %>%
  select(date, `Sutter Bypass` = sutter4, `Yolo Bypass` = yolo2)

mean_flow <- DSMflow::flows_cfs %>%
  left_join(bypass) %>%
  filter(between(year(date), 1980, 2000)) %>%
  gather(watershed, flow_cfs, -date) %>%
  filter(watershed != 'Lower-mid Sacramento River1') %>%
  mutate(flow_cms = DSMflow::cfs_to_cms(flow_cfs),
         watershed = ifelse(watershed == 'Lower-mid Sacramento River2', 'Lower-mid Sacramento River', watershed)) %>%
  select(-flow_cfs) %>%
  spread(date, flow_cms) %>%
  left_join(DSMflow::watershed_ordering) %>%
  arrange(order) %>%
  select(-watershed, -order) %>%
  create_model_array()

dimnames(mean_flow) <- list(DSMflow::watershed_ordering$watershed, month.abb[1:12], 1980:2000)
usethis::use_data(mean_flow, overwrite = TRUE)

# Replaces upsacQ
# flow at Bend C109, CALSIMII units cfs, sit-model units cms
upper_sacramento_flows <- misc_flows %>%
  select(date, upsacQcfs = C109) %>%
  mutate(upsacQcms = DSMflow::cfs_to_cms(upsacQcfs)) %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year >= 1980, year <= 2000) %>%
  select(-date, -upsacQcfs) %>%
  spread(year, upsacQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(upper_sacramento_flows) <- month.abb[1:12]
usethis::use_data(upper_sacramento_flows, overwrite = TRUE)

# Replaces retQ
# proportion flows at tributary junction coming from natal watershed using october average flow
# create lookup vector for retQ denominators based on Jim's previous input
tributary_junctions <- c(rep(watersheds[16], 16), NA, watersheds[19], watersheds[21], watersheds[19],
                  watersheds[21], NA, rep(watersheds[24],2), watersheds[25:27], rep(watersheds[31],4))

names(tributary_junctions) <- watersheds

denominator <- DSMflow::flows_cfs %>%
  select(-`Lower-mid Sacramento River1`) %>% #Feather river comes in below Fremont Weir use River2 for Lower-mid Sac
  rename(`Lower-mid Sacramento River` = `Lower-mid Sacramento River2`) %>%
  gather(watershed, flow, -date) %>%
  filter(month(date) == 10, watershed %in% unique(tributary_junctions)) %>%
  rename(denominator = watershed, junction_flow = flow)

proportion_flow_natal <- DSMflow::flows_cfs %>%
  select(-`Lower-mid Sacramento River1`) %>% #Feather river comes in below Fremont Weir use River2 for Lower-mid Sac
  rename(`Lower-mid Sacramento River` = `Lower-mid Sacramento River2`) %>%
  gather(watershed, flow, -date) %>%
  filter(month(date) == 10) %>%
  mutate(denominator = tributary_junctions[watershed]) %>%
  left_join(denominator) %>%
  mutate(retQ = ifelse(flow / junction_flow > 1, 1, flow / junction_flow),
         retQ = replace(retQ, watershed %in% c('Calaveras River', 'Cosumnes River', 'Mokelumne River'), 1)) %>%
  select(watershed, date, retQ) %>%
  mutate(year = year(date)) %>%
  filter(year >= 1979, year <= 2000) %>%
  select(watershed, year, retQ) %>%
  bind_rows(tibble(
    year = 1979,
    watershed = c('Yolo Bypass', 'Sutter Bypass'),
    retQ = 0
  )) %>%
  spread(year, retQ) %>%
  left_join(cvpiaData::watershed_ordering) %>%
  arrange(order) %>%
  mutate_all(~replace_na(., 0)) %>%
  select(-order, -watershed) %>%
  as.matrix()

rownames(proportion_flow_natal) <- watersheds

usethis::use_data(proportion_flow_natal, overwrite = TRUE)

# Replaces prop.pulse
proportion_pulse_flows <- DSMflow::flows_cfs %>%
  filter(between(year(date), 1980, 1999)) %>%
  mutate(`Lower-mid Sacramento River` = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 *`Lower-mid Sacramento River2`) %>%
  select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`) %>%
  gather(watershed, flow, -date) %>%
  group_by(month = month(date), watershed) %>%
  summarise(prop_pulse = sd(flow)/median(flow)/100) %>% # TODO why divide by 100?
  mutate(prop_pulse = replace(prop_pulse, is.infinite(prop_pulse), 0)) %>%
  select(month, watershed, prop_pulse) %>%
  bind_rows(tibble(
    month = rep(1:12, 2),
    watershed = rep(c('Yolo Bypass', 'Sutter Bypass'), each = 12),
    prop_pulse = 0
  )) %>%
  spread(month, prop_pulse) %>%
  left_join(cvpiaData::watershed_ordering) %>%
  arrange(order) %>%
  select(-order, -watershed) %>%
  as.matrix()

colnames(proportion_pulse_flows) <- month.abb[1:12]
rownames(proportion_pulse_flows) <- DSMflow::watershed_ordering$watershed

usethis::use_data(proportion_pulse_flows, overwrite = TRUE)

# DELTA ----
#  C400 flow at freeport
# 1) daily discharge of the Sacramento River at Freeport
# 2) an indicator variable for whether the DCC is open (1) or closed (0).
# Replaces dlt.gates
delta_cross_channel_closed <- read_csv('data-raw/delta_cross_channel_gates/DeltaCrossChannelTypicalOperations.csv', skip = 2) %>%
  mutate(Month = which(month.name == Month), prop_days_closed = `Days Closed` / days_in_month(Month)) %>%
  select(month = Month, days_closed = `Days Closed`, prop_days_closed) %>%
  gather(metric, value, -month) %>%
  spread(month, value) %>%
  select(-metric) %>%
  as.matrix()

colnames(delta_cross_channel_closed) <- month.abb[1:12]
rownames(delta_cross_channel_closed) <- c('count', 'proportion')

usethis::use_data(delta_cross_channel_closed, overwrite = TRUE)

# delta flows and diversions --------------------
# North Delta inflows: C400 + C157
# South Delta inflow: C401B + C504 + C508 + C644
# North Delta diversions: D403A + D403B + D403C + D403D + D404
# South Delta diversions: D418 + D419 + D412 + D410 + D413 + D409B + D416 + D408_OR + D408_VC
delta_flows <- calsim %>%
  select(date, C400, C157, C401B, C504, C508, C644, D403A, D403B, D403C, D403D,
         D404, D418, D419, D412, D410, D413, D409B, D416, D408_OR, D408_VC) %>%
  mutate(n_dlt_inflow_cfs = C400 + C157,
         s_dlt_inflow_cfs = C401B + C504 + C508 + C644,
         n_dlt_div_cfs =  D403A + D403B + D403C + D403D + D404,
         s_dlt_div_cfs = D418 + D419 + D412 + D410 + D413 + D409B + D416 + D408_OR + D408_VC,
         n_dlt_prop_div = n_dlt_div_cfs / n_dlt_inflow_cfs,
         s_dlt_prop_div = s_dlt_div_cfs / s_dlt_inflow_cfs,
         s_dlt_prop_div = ifelse(s_dlt_prop_div > 1, 1, s_dlt_prop_div)) %>%
  select(date,
         n_dlt_inflow_cfs,
         s_dlt_inflow_cfs,
         n_dlt_div_cfs,
         s_dlt_div_cfs,
         n_dlt_prop_div,
         s_dlt_prop_div)

usethis::use_data(delta_flows, overwrite = TRUE)

# delta inflows
# Replaces Dlt.inf
inflow <- delta_flows %>%
  filter(year(date) >= 1980, year(date) <= 2000) %>%
  mutate(n_dlt_inflow_cms = DSMflow::cfs_to_cms(n_dlt_inflow_cfs),
         s_dlt_inflow_cms = DSMflow::cfs_to_cms(s_dlt_inflow_cfs)) %>%
  select(date, n_dlt_inflow_cms, s_dlt_inflow_cms) %>%
  gather(delta, inflow, -date) %>%
  spread(date, inflow) %>%
  select(-delta)

delta_inflow <- array(NA, dim = c(12, 21, 2))
delta_inflow[ , , 1] <- as.matrix(inflow[1, ])
delta_inflow[ , , 2] <- as.matrix(inflow[2, ])

dimnames(delta_inflow) <- list(month.abb[1:12], 1980:2000, c('North Delta', 'South Delta'))

usethis::use_data(delta_inflow, overwrite = TRUE)

# delta prop diverted
# Replaces dlt.divers
dl_prop_div <- delta_flows %>%
  filter(year(date) >= 1980, year(date) <= 2000) %>%
  select(date, n_dlt_prop_div, s_dlt_prop_div) %>%
  gather(delta, prop_div, -date) %>%
  spread(date, prop_div) %>%
  select(-delta)

delta_proportion_diverted <- array(NA, dim = c(12, 21, 2))
delta_proportion_diverted[ , , 1] <- as.matrix(dl_prop_div[1, ])
delta_proportion_diverted[ , , 2] <- as.matrix(dl_prop_div[2, ])

dimnames(delta_proportion_diverted) <- list(month.abb[1:12], 1980:2000, c('North Delta', 'South Delta'))

usethis::use_data(delta_proportion_diverted, overwrite = TRUE)

# delta total diversions
# Replaces dlt.divers.tot
dl_tot_div <- delta_flows %>%
  filter(year(date) >= 1980, year(date) <= 2000) %>%
  mutate(n_dlt_div_cms = DSMflow::cfs_to_cms(n_dlt_div_cfs),
         s_dlt_div_cms = DSMflow::cfs_to_cms(s_dlt_div_cfs)) %>%
  select(date, n_dlt_div_cms, s_dlt_div_cms) %>%
  gather(delta, tot_div, -date) %>%
  spread(date, tot_div) %>%
  select(-delta)

delta_total_diverted <- array(NA, dim = c(12, 21, 2))
delta_total_diverted[ , , 1] <- as.matrix(dl_tot_div[1, ])
delta_total_diverted[ , , 2] <- as.matrix(dl_tot_div[2, ])

dimnames(delta_total_diverted) <- list(month.abb[1:12], 1980:2000, c('North Delta', 'South Delta'))

usethis::use_data(delta_total_diverted, overwrite = TRUE)

# bypasses -------------

# Replaces prop.Q.bypasses
# cap values greater than 1 at 1
bypass_prop_flow <- misc_flows %>%
  mutate(yolo = pmin(D160/C134, 1),
         sutter = (D117 + D124 + D125 + D126)/C116,
         year = year(date), month = month(date)) %>%
  select(month, year, yolo, sutter) %>%
  filter(between(year, 1980, 2000)) %>%
  gather(bypass, prop_flow, -month, -year) %>%
  spread(year, prop_flow) %>%
  arrange(bypass, month) %>%
  select(-month, -bypass) %>%
  as.matrix()

proportion_flow_bypasses <- array(NA, dim = c(12, 21, 2))
dimnames(proportion_flow_bypasses) <- list(month.abb[1:12], 1980:2000, c('Sutter Bypass', 'Yolo Bypass'))
proportion_flow_bypasses[ , , 1] <- bypass_prop_flow[1:12, ]
proportion_flow_bypasses[ , , 2] <- bypass_prop_flow[13:24, ]

usethis::use_data(proportion_flow_bypasses, overwrite = TRUE)

# Adds gates_overtopped
bypass_overtopped <- read_csv("data-raw/delta_cross_channel_gates/bypass_overtopped.csv")
bypass_overtopped <- bypass_overtopped %>%
  mutate(year = year(date),
         month = month(date)) %>%
  filter(between(year, 1980, 2000)) %>%
  select(-date) %>%
  gather(bypass, overtopped, -month, -year) %>%
  spread(year, overtopped) %>%
  arrange(bypass, month)  %>%
  select(-month, -bypass) %>%
  as.matrix()

gates_overtopped <- array(NA, dim = c(12, 21, 2))
dimnames(gates_overtopped) <- list(month.abb[1:12], 1980:2000, c('Sutter Bypass', 'Yolo Bypass'))
gates_overtopped[ , , 1] <- bypass_overtopped[1:12, ]
gates_overtopped[ , , 2] <- bypass_overtopped[13:24, ]

usethis::use_data(gates_overtopped, overwrite = TRUE)
# Delta Routing Flows ---------------------

library(DSMflow)
library(tibble)

# Adds wilkins flow node to replace freeport flow
# I used node C129 for wilkins(Cyril recommended C129)
wilkins_node <- c("C129")

wilkins_flow <- calsim %>%
  select(date, wilkins_node) %>%
  filter(year(date) >= 1980, year(date) <= 2000) %>%
  transmute(
    year = year(date),
    month = month(date),
    wilklinsQcfs = C129,
    wilkinsQcms = cfs_to_cms(C129))  %>%
  select(year, month, wilkinsQcms) %>%
  spread(year, wilkinsQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(wilkins_flow) <- month.abb
usethis::use_data(wilkins_flow, overwrite = TRUE)

# freeport flow
freeport_node <- c("C400")

freeport_flow <- calsim %>%
  select(date, freeport_node) %>%
  filter(
    year(date) >= 1980, year(date) <= 2000) %>%
  transmute(
    year = year(date),
    month = month(date),
    freeportQcfs = C400,
    freeportQcms = cfs_to_cms(C400)
  ) %>%
  select(year, month, freeportQcms) %>%
  spread(year, freeportQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(freeport_flow) <- month.abb
usethis::use_data(freeport_flow, overwrite = TRUE)

# vernalis flow
vernalis_node <- "C639"

vernalis_flow <- calsim  %>%
  select(date, vernalis_node) %>%
  filter(
    year(date) >= 1980, year(date) <= 2000) %>%
  transmute(
    year = year(date),
    month = month(date),
    vernalisQcfs = C639,
    vernalisQcms = cfs_to_cms(C639)
  ) %>%
  select(year, month, vernalisQcms) %>%
  spread(year, vernalisQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(vernalis_flow) <- month.abb

usethis::use_data(vernalis_flow, overwrite = TRUE)

# stockton flow

stockton_node <- "C417A"

stockton_flow <- calsim %>%
  select(date, stockton_node) %>%
  filter(
    year(date) >= 1980, year(date) <= 2000) %>%
  transmute(
    year = year(date),
    month = month(date),
    stocktonQcfs = C417A,
    stocktonQcms = cfs_to_cms(C417A)
  ) %>%
  select(year, month, stocktonQcms) %>%
  spread(year, stocktonQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(stockton_flow) <- month.abb

usethis::use_data(stockton_flow, overwrite = TRUE)


# cvp exports
cvp_exports <- calsim %>%
  select(date, DEL_CVP_TOTAL) %>%
  filter(
    year(date) >= 1980, year(date) <= 2000) %>%
  transmute(
    date = date,
    year = year(date),
    month = month(date),
    cvpExportsQcfs = DEL_CVP_TOTAL,
    cvpExportsQcms = cfs_to_cms(DEL_CVP_TOTAL)
  ) %>%
  select(year, month, cvpExportsQcms) %>%
  spread(year, cvpExportsQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(cvp_exports) <- month.abb

usethis::use_data(cvp_exports, overwrite = TRUE)

# swp exports

swp_exports <- calsim %>%
  select(date, DEL_SWP_TOTAL) %>%
  filter(
    year(date) >= 1980, year(date) <= 2000) %>%
  transmute(
    date = date,
    year = year(date),
    month = month(date),
    swpExportsQcfs = DEL_SWP_TOTAL,
    swpExportsQcms = cfs_to_cms(DEL_SWP_TOTAL)
  ) %>%
  select(year, month, swpExportsQcms) %>%
  spread(year, swpExportsQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(swp_exports) <- month.abb

usethis::use_data(swp_exports, overwrite = TRUE)
