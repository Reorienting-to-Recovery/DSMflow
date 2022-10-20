library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(devtools)
library(CDECRetrieve)


# Notes from Mike Wright about WilkinsSlough.csv (cell A1)--------------

# date is the CalLite-CV date column, the next column with the name of a
# CalLite-CV element is the LocalInflow data from the CT_noCC run for the
# CalLite-CV element from which we're going to be deriving these DSM stream flow
# time series in cfs (I couldn't find an output file in the Basin Study
# documentation for this like I did for the Outflow data in the master csv but
# spot checks indicated that the GoldSim model file I'm pulling these from have
# identical data to those outputs), cs_date is for my reference to avoid
# screwing up the 10/2014=10/1925 matching and it sounds like you can use it for
# something too, and the CalSim II records corresponding to the local inflows to
# the CalLite-CV element are listed afterward (I've got them in cfs now, to
# match the CalLite-CV data). 'matches for other csvs.csv' contains the mappings
# between the DSM streams and CalSim II elements, as a one-stop summary for
# which DSM streams are showing up in which specific .csvs like this one. I
# renamed the CalSim II elements that map to DSM streams in this csv; everything
# that still has a CalSim II name is one of the elements that contributes to the
# total (the denominator) but is NOT a DSM stream. The CalSim II run I'm using
# at the moment is the most recent representation of current operations I've
# received; probably we'll be choosing a different baseline in the future but
# the matches csv mentioned above contains the CalSim II match for DSM streams,
# so I can go back and re-pull those time series if/when necessary. C17603 is
# Sites Reservoir outflows into the Sacramento in this region; it's zeroed out
# in the run I'm using but that should be re-checked if a different run is used.
# NOTE that matching 2014 to 1925 results in the last ~7 years of the CalLite-CV
# run not having an equivalent CS2 number...
# EXAMPLE CALCULATION: For 10/2014,
# Elder Creek has 13.8 cfs, the total of all WiSl inflows sum(D3:M3)=550.4, and
# 13.8/550.4=0.025155, the fraction of total CalSim II inflow in Elder Creek.
# Applying that fraction to the CalLite-CV WilkinsSlough LocalInflow number we
# get Elder Creek DSM flow = 0.25155*189.9=4.78 cfs.

wilkins <- read_csv("data-raw/calLite_mapping/WilkinsSlough.csv", skip=1)

wilkins_disaggregated <- wilkins %>%
  mutate(denom = `Elder Creek` + `Thomes Creek` + `Antelope Creek` +
           `Mill Creek` + `Deer Creek` + `Big Chico Creek` + `Stony Creek` +
           C184A + I118 + I123,
         `Elder Creek` = `Elder Creek` / denom * WilkinsSlough,
         `Thomes Creek` = `Thomes Creek` / denom * WilkinsSlough,
         `Antelope Creek` = `Antelope Creek` / denom * WilkinsSlough,
         `Mill Creek` = `Mill Creek` / denom * WilkinsSlough,
         `Deer Creek` = `Deer Creek` / denom * WilkinsSlough,
         `Big Chico Creek` = `Big Chico Creek` / denom * WilkinsSlough,
         `Stony Creek` = `Stony Creek` / denom * WilkinsSlough,
         cs_date = dmy(cs_date)) %>%
  select(date, cs_date, `Elder Creek`: `Stony Creek`)

View(wilkins_disaggregated)

# Mike Wrights notes on RedBluff.csv------------------

# date is the CalLite-CV date column, the next column with the name of a
# CalLite-CV element is the LocalInflow node, cs_date is for my reference to
# avoid screwing up the 10/2014=10/1925 matching and it sounds like you can use
# it for something too, and the CalSim II records corresponding to the local
# inflows to the CalLite-CV element are listed afterward (I've got them in cfs
# now, to match the CalLite-CV data). 'matches for other csvs.csv' contains the
# mappings between the DSM streams and CalSim II elements, as a one-stop summary
# for which DSM streams are showing up in which specific .csvs like this one. I
# renamed the CalSim II elements that map to DSM streams in this csv; everything
# that still has a CalSim II name is one of the elements that contributes to the
# total (the denominator) but is NOT a DSM stream.
# EXAMPLE CALCULATION: For 10/2014, Cow Creek has 27.1 cfs,
# the total of all inflows sum(D3:I3)=405.8,
# and 27.1/405.8=0.066668, the fraction of total CalSim II inflow in Cow Creek.
# Applying that fraction to the CalLite-CV RedBluff LocalInflow number we get
# Cow Creek DSM flow = 0.066668*259.7=17.31 cfs.

redbluff <- read_csv('data-raw/calLite_mapping/RedBluff.csv', skip = 1)

# bear creek pick a neighbor to represent flows, no gage data
redbluff %>%
  select(cs_date, `Cow Creek`, `Battle Creek`, I109) %>%
  mutate(cs_date = dmy(cs_date), `Bear Creek` = I109/2) %>%
  gather(watershed, flow, -cs_date) %>%
  filter(year(cs_date) >= 1980, watershed != 'I109') %>%
  ggplot(aes(x = cs_date, y = flow, color = watershed)) +
  geom_line() +
  theme_minimal() +
  theme(text = element_text(size = 18))

redbluff_disaggregated <- redbluff %>%
  mutate(denom = `Cow Creek` + `Cottonwood Creek` + `Battle Creek` +
           `Paynes Creek` + I109 + I112,
         `Cow Creek` = `Cow Creek` / denom * RedBluff,
         `Cottonwood Creek` = `Cottonwood Creek` / denom * RedBluff,
         `Battle Creek` = `Battle Creek` / denom * RedBluff,
         `Paynes Creek` = `Paynes Creek` / denom * RedBluff,
         `Bear Creek` = I109 / 2 / denom * RedBluff,
         cs_date = dmy(cs_date)) %>%
  select(date, cs_date, `Cow Creek`:`Bear Creek`)

# Mike Wrights notes on YubaFeather.csv-------------

#date is the CalLite-CV date column, the next column with the name of a
#CalLite-CV element is the LocalInflow node (note occasional negative
#values...), cs_date is for my reference to avoid screwing up the
#10/2014=10/1925 matching and it sounds like you can use it for something too,
#and the CalSim II records corresponding to the local inflows to the CalLite-CV
#element are listed afterward (I've got them in cfs now, to match the CalLite-CV
#data). 'matches for other csvs.csv' contains the mappings between the DSM
#streams and CalSim II elements, as a one-stop summary for which DSM streams are
#showing up in which specific .csvs like this one. I renamed the CalSim II
#elements that map to DSM streams in this csv; everything that still has a
#CalSim II name is one of the elements that contributes to the total (the
#denominator) but is NOT a DSM stream. Here we have only the Bear River (C282)
#and a small 'projected gain in DSA69' element I207 in the area the
#documentation considers the YubaFeather node's Local Inflow area.
# EXAMPLE CALCULATION: For 1/2015, Bear River has 12.24 cfs, the total of all inflows
#sum(D6:E6)=3138.24, and 12.24/3138.24=0.003901, the fraction of total CalSim II
#inflow in the Bear River. Applying that fraction to the CalLite-CV YubaFeather
#LocalInflow number we get Bear River DSM flow = 0.003901*258.1=1.01 cfs. Given
#the bizarre spikiness of I207, maybe the CSII Bear River numbers unadjusted for
#anything or the YubaFeather LocalInflows alone would be better representations
#of the Bear River...

yubafeather <- read_csv('data-raw/calLite_mapping/YubaFeather.csv', skip = 1)


# compare monthly mean gage flow during the period to see if YubaFeather node is better
bb <- yubafeather %>%
  mutate(denom = `Bear River` + I207,
         BearRiver = `Bear River` / denom * YubaFeather,
         cs_date = dmy(cs_date)) %>%
  select(date, cs_date, `Bear River`, BearRiver, YubaFeather)
View(bb)

bear <- dataRetrieval::readNWISdv(siteNumbers = '11424000', parameterCd = '00060',
                          '1980-01-01', '1999-12-31')
glimpse(bear)

gage_data <- bear %>%
  mutate(year = year(Date), month = month(Date), flow =  X_00060_00003) %>%
  group_by(year, month) %>%
  summarise(monthly_mean_flow = mean(flow, na.rm = TRUE)) %>%
  mutate(type = 'gage data')

bb %>%
  gather(Node, flow, -date, -cs_date) %>%
  mutate(type = case_when(
    Node == 'Bear River' ~'BearRiver',
    Node == 'YubaFeather' ~ 'YubaFeather',
    TRUE ~ 'disaggregated'),
         year = year(cs_date), month = month(cs_date)) %>%
  select(year, month, monthly_mean_flow = flow, type) %>%
  filter(year >= 1980, year <= 1999, type != 'disaggregated') %>%
  bind_rows(gage_data) %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  ggplot(aes(x = date, y = monthly_mean_flow, color = type)) +
  geom_line() +
  theme_minimal() +
  labs(y = 'monthly mean flow') +
  theme(text = element_text(size = 18))

# after investigating, selected Bear River column to represent Bear River
bear_river <- yubafeather %>%
  mutate(cs_date = dmy(cs_date)) %>%
  select(date, cs_date, `Bear River`)

# Mike Wrights notes on Eastside.csv--------------

# date is the CalLite-CV date column, the next column with the name of a
# CalLite-CV element is in this case the OUTFLOW node because the LocalInflow
# one isn't saving results and when I run the model it has negative values
# pretty regularly (this is different from other nodes but this node is unique
# with only two inflows so maybe that's defensible... but note negative value in
# (only) first time step...), cs_date is for my reference to avoid screwing up
# the 10/2014=10/1925 matching and it sounds like you can use it for something
# too, and the CalSim II records corresponding to the local inflows to the
# CalLite-CV element are listed afterward (I've got them in cfs now, to match
# the CalLite-CV data). 'matches for other csvs.csv' contains the mappings
# between the DSM streams and CalSim II elements, as a one-stop summary for
# which DSM streams are showing up in which specific .csvs like this one. I
# renamed the CalSim II elements that map to DSM streams in this csv; everything
# that still has a CalSim II name is one of the elements that contributes to the
# total (the denominator) but is NOT a DSM stream. The CalSim II run I'm using
# at the moment is the most recent representation of current operations I've
# received; probably we'll be choosing a different baseline in the future but
# the matches csv mentioned above contains the CalSim II match for DSM streams,
# so I can go back and re-pull those time series if/when necessary. NOTE that
# matching 2014 to 1925 results in the last ~7 years of the CalLite-CV run not
# having an equivalent CS2 number... EXAMPLE CALCULATION: For 11/2014, Mokelumne
# River has 240.7 cfs, the total of all inflows sum(D4:E4)=269.4, and
# 240.7/269.4=0.893467, the fraction of total CalSim II inflow in the Mokelumne
# River. Applying that fraction to the CalLite-CV Eastside Outflow number we get
# Mokelumne River DSM flow = 0.893467*79.16=70.73 cfs.

eastside <- read_csv('data-raw/calLite_mapping/Eastside.csv', skip = 1)

eastside_disaggregated <- eastside %>%
  mutate(denom = `Mokelumne River` + `Cosumnes River`,
         `Mokelumne River` = `Mokelumne River` / denom * Eastside,
         `Cosumnes River` = `Cosumnes River` / denom * Eastside,
         cs_date = dmy(cs_date)) %>%
  select(date, cs_date, `Mokelumne River`, `Cosumnes River`) %>%
  gather(river, flow, -date, -cs_date) %>%
  mutate(flow = case_when(
    flow < 0 ~ 0,
    is.nan(flow) ~ 0,
    TRUE ~ flow
  )) %>%
  spread(river, flow) %>%
  arrange(cs_date)

View(eastside_disaggregated)

# Mike Wrights notes on Sutter Bypass and Butte Data----------------

# date is the CalLite-CV date  column, WilkinsSlough_Inflow is the inflow record
# (not the LocalInflow used in WilkinsSlough.csv in the field named
# WilkinsSlough) representing flow into this stretch of the Sacramento River in
# CL, cs_date is the CalSim II date column, C112 is the CalSim II arc
# corresponding to WilkinsSlough_Inflow, and the 4 D* arcs are the weir
# overflows from CalSim II which I'd recommend you sum and plot on the Y axis of
# a scatter plot against C112 on the X axis so you can come up with some Y=f(X)
# relationship that we can apply to the CalLite-CV data so that
# Sutter_estimated_inflow=f(WilkinsSlough_Inflow). Finally, I've included I217,
# labeled Butte Creek here in keeping with my convention of renaming the CalSim
# II arcs we've identified with DSM streams as noted in
# notesforlocalinflowcsvs.csv. You might want to make the final DSM input for
# Sutter flow be the Y from that equation above + I217 aka Butte Creek; it
# depends on whether the R code sends Butte Creek fish (and therefore flow) into
# the Sutter every single month or if the Sutter only activates as a juvenile
# rearing habitat when it gets Sacramento flows/fish. In either case, this is
# the best value we've got for Butte Creek DSM flows, so it can be used for
# that. I figure you can read the R code better than I can, but if deciding
# whether to include Butte Creek in the Sutter flows is a tougher task than I'm
# anticipating I can reacquaint myself with the code and we can talk it through.

sutter_butte <- read_csv('data-raw/calLite_mapping/SutterButte.csv', skip = 1)
View(sutter_butte)

# fitting a model that is the sum of the D arcs (D117, D124, D125, D126) as a function of C112
# Sutter Bypass Flow will be the predicted values computed with WilkinsSlough_Inflow
sutter_butte %>%
  mutate(D_arcs = D117 + D124 + D125 + D126) %>%
  filter(C112 >= 15000) %>% # choose 15000 as threshold, anything under gets a 0 flow
  ggplot(aes(x = C112, y = D_arcs)) +
  geom_point(alpha = .2) +
  geom_smooth(method = 'lm')

sutter_data <- sutter_butte %>%
  mutate(D_arcs = D117 + D124 + D125 + D126) %>%
  filter(C112 >= 15000)

sutter_model <- lm(D_arcs ~ C112, sutter_data)

summary(sutter_model)

# sutter_butte <- sutter_butte %>%
#   mutate(`Sutter Bypass` = ifelse(WilkinsSlough_Inflow >= 15000,
#            predict.lm(sutter_model, data.frame(C112 = WilkinsSlough_Inflow)), 0),
#          cs_date = dmy(cs_date)) %>%
#   select(date, cs_date, `Butte Creek`, `Sutter Bypass`)

butte_creek <- sutter_butte %>%
    mutate(cs_date = dmy(cs_date)) %>%
    select(date, cs_date, `Butte Creek`)

# use cal sim sutter to represent cal lite sutter flows becasue no good corollary exists
cl_dates <- read_csv('data-raw/calLite_mapping/calLite_calSim_date_mapping.csv')

date_mapping <- cl_dates %>%
  mutate(cs_year = year(cs_date), cs_month = month(cs_date),
         cs_date = ymd(paste(cs_year, cs_month, '01', sep = '-'))) %>%
  select(cl_date, cs_date)

sutter <- read_csv('data-raw/Sutter.csv', skip = 1) %>%
  mutate(cs_date = mdy(cs_date))

sutter_calsim <- date_mapping %>%
  left_join(sutter)


# combine all---------------
ord <- read_csv('data-raw/calLite_mapping/calLite_mapping/watershed_order.csv') %>%
  pull(Watershed)

all_flow_cc <- read_csv('data-raw/calLite_mapping/flowmaster.csv', skip = 1) %>%
  bind_cols(select(eastside_disaggregated, -date),
            select(wilkins_disaggregated, -date, -cs_date),
            select(redbluff_disaggregated, -date, -cs_date),
            select(bear_river, -date, -cs_date),
            select(butte_creek, -date, -cs_date),
            select(sutter_calsim, -cl_date, -cs_date)) %>%
  select(date = cs_date, ord) %>%
  filter(!is.na(date))

use_data(all_flow_cc, overwrite = TRUE)

# TODO need to fix negative values and QA/QC

# Mike Wrights notes on Delta--------------------------------

# date is CalLite-CV time series. Unlike the other csv's I've left the original
# labels, although I replaced the .'s with _'s to fit with the nomenclature I
# think Sadie prefers, since we might use these differently than I've laid out
# in the next sentence (and we'll probably need the DCC time series in
# particular separated out for apportioning Sacramento River fish between Delta
# regions). It should be as 'simple' as this: North Delta flow = Hood.Outflow,
# Central Delta flow = Eastside.Outflow + DCC.Diversion, and South Delta flow =
# SJR_Calaveras.Outflow + SJR_FlowSplit.C417B + CentralDelta.Outflow. If the
# Central Delta is really just a constituent part of the South Delta then
# replace CentralDelta.Outflow with Eastside.Outflow + DCC.Diversion. I hope
# that's all there is to it... NOTE the one (and only one) negative value for
# Eastside.Outflow, in the first time step

Delta <- read_csv('data-raw/calLite_mapping/Delta.csv', skip = 1)
glimpse(Delta)

# assuming central delta is included in south delta
delta_flow_cc <- Delta %>%
  mutate(`North Delta` = Hood_Outflow,
         `South Delta` = Eastside_Outflow + DCC_Diversion +
           SJR_Calaveras_Outflow + SJR_FlowSplit_C417B) %>%
  left_join(sutter_butte) %>%
  select(date = cs_date, `North Delta`, `South Delta`) %>%
  filter(!is.na(date))

use_data(delta_flow)
#use_data(delta_flow_cc)
