library(tidyverse)


process_dss_output <- function(file) {
  raw_data <- readxl::read_excel(file)
  raw_data |>
    pivot_longer(-c(B, date), values_to = "flow_cfs", names_to = "node") |>
    mutate(
      date = date - years(
        case_when(
          year(date) >= 2022 ~ 100,
          year(date) == 2021 & month(date) %in% 10:12 ~ 100,
          TRUE ~ 0
        )
      )
    ) |>
    filter(!is.na(flow_cfs))
}

calsim_data <- map_df(list.files("data-raw/calsim3/", pattern = ".xlsx", full.names = TRUE), process_dss_output)

n_dlt_inflow_cfs_nodes <- c("C_SAC041", "C_CSL005")
s_dlt_inflow_cfs_nodes <- c("C_SAC029B", "D_SAC030_MOK014",
                            "C_MOK022", "C_CLV004", "C_SJR056")
n_dlt_div_cfs_nodes <- c("C_CSL004B", "DD_SAC017_SACS")
s_dlt_div_cfs_nodes <- c("D_OMR028_DMC000", "D_OMR027_CAA000",
                         "DD_SJR026_SJRE", "DD_SJR013_SJRW",
                         "DD_MOK004_MOK", "DD_OMR027_OMR",
                         "D_RSL004_CCC004", "D_OMR021_ORP000",
                         "D_VCT002_ORP000")

north_delta_inflow <- calsim_data |>
  filter(node %in% n_dlt_inflow_cfs_nodes)

south_delta_inflow <- calsim_data |>
  filter(node %in% s_dlt_inflow_cfs_nodes)

north_delta_diversions <- calsim_data |>
  filter(node %in% n_dlt_div_cfs_nodes)

south_delta_diversions <- calsim_data |>
  filter(node %in% s_dlt_div_cfs_nodes)

watershed_to_nodes <- c(`Upper Sacramento River` = "C_SAC273", `Antelope Creek` = "C_ANT010",
                `Battle Creek` = "C_BTL006", `Bear Creek` = "C_BCN005", `Big Chico Creek` = "C_BCC004",
                `Butte Creek` = "C_BTC012", `Clear Creek` = "C_CLR009", `Cottonwood Creek` = "C_CWD003",
                `Cow Creek` = "C_COW003", `Deer Creek` = "C_DRC005", `Elder Creek` = "C_ELD005",
                `Mill Creek` = "C_MLC004", `Paynes Creek` = "C_PYN001", `Stony Creek` = "C_STN004",
                `Thomes Creek` = "C_THM005", `Upper-mid Sacramento River` = "C_SAC193",
                `Bear River` = "C_CMPFW", `Feather River` = "C_FTR059", `Yuba River` = "C_YUB002",
                `Lower-mid Sacramento River1` = "C_SAC093", `Lower-mid Sacramento River2` = "C_SAC048",
                `American River` = "C_NTOMA", `Lower Sacramento River` = "C_SAC063",
                `Calaveras River` = "C_NHGAN", `Cosumnes River` = "C_CSM005",
                `Mokelumne River` = "C_CMCHE", `Merced River` = "C_MCD050", `Stanislaus River` = "C_STS059",
                `Tuolumne River` = "C_TUO054", `San Joaquin River` = "C_SJR081"
)

nodes_to_watershed <- names(watershed_to_nodes)
names(nodes_to_watershed) <- as.character(watershed_to_nodes)


lto_calsim3_flows <- calsim_data |> filter(node %in% watershed_to_nodes) |>
  mutate(watershed = nodes_to_watershed[node]) |>
  select(-B, -node) |>
  pivot_wider(names_from = "watershed", values_from = "flow_cfs") |>
  mutate(date = as_date(date))

write_rds(lto_calsim3_flows, "data-raw/calsim3/calsim3-flow-baseline.rds")


# bypass flows ----------------------------------------
nodes_df <- data.frame(inputs=c("sutter1",
                                "sutter1",
                                "sutter1",
                                "sutter2",
                                "sutter3",
                                "sutter4",
                                "yolo1",
                                "yolo2"),
                       nodes=c("SP_SAC193_BTC003",
                               "SP_SAC188_BTC003",
                               "SP_SAC178_BTC003",
                               "C_BTC003",
                               "C_SBP024",
                               "C_SSL001",
                               "SP_SAC083_YBP037",
                               "C_CSL005"),
                       type=c(rep("RIVER-SPILLS",3),rep("CHANNEL",3),
                              "RIVER-SPILLS","CHANNEL"))

bypass_nodes <- nodes_df$inputs
names(bypass_nodes) <- nodes_df$nodes

lto_calsim3_bypass_flows <- calsim_data |> filter(node %in% names(bypass_nodes)) |>
  mutate(bypass = bypass_nodes[node]) |>
  select(-B, -node) |>
  group_by(date, bypass) |>
  summarise(
    flow_cfs = sum(flow_cfs)
  ) |>
  ungroup() |>
  pivot_wider(names_from = bypass, values_from = flow_cfs) |>
  mutate(date = as_date(date))

readr::write_rds(lto_calsim3_bypass_flows, "data-raw/calsim3/calsim3-lto-bypass-flows.rds")

# proportion diverted ---------------------------------

# from the documentation:

# o	Upper Sacramento River: (D_SAC296_WTPFTH + D_SAC296_02_SA + D_SAC294_WTPBLV + D_SAC294_03_PA + D_SAC289_03_PA + D_SAC281_02_NA + D_SAC273_03_NA) / C_SAC273
# o	Antelope Creek: D_ANT010_05_NA / C_ANT010
# o	Battle Creek: NA
# o	Bear Creek: NA
# o	Big Chico Creek: NA
# o	Butte Creek: (D_BTC045_ESL008 + D_BTC043_10_NA + D_BTC036_10_NA + DBTC012_09_SA2 + D_BTC012_CRK005) / (D_BTC045_ESL008 + D_BTC043_10_NA + D_BTC036_10_NA + DBTC012_09_SA2 + D_BTC012_CRK005 + C_BTC012)
# o	Clear Creek: NA
# o	Cottonwood Creek: NA
# o	Cow Creek: NA
# o	Deer Creek: (D_DRC010_05_NA + D_DRC005_05_NA) / C_DRC005
# o	Elder Creek: D_ELD012_04_NA / C_ELD005
# o	Mill Creek: D_MLC006_05_NA / C_MLC004
# o	Paynes Creek: NA
# o	Stony Creek: D_STN021_06_PA / C_STN026
# o	Thomes Creek: D_THM012_04_NA / C_THM005
# o	Upper-mid Sacramento River: (D_SAC240_TCC001 + D_SAC240_05_NA + D_SAC224_04_NA + D_SAC196_MTC000 + D_SAC185_08N_NA + D_SAC185_09_NA + D_SAC178_08N_SA1 + D_SAC162_09_SA2 + D_SAC159_08S_SA1 + D_SAC159_08N_SA1 + D_SAC146_08S_NA1 + D_SAC136_18_NA + D_SAC136_18_SA + D_SAC129_08S_NA2 + D_SAC122_19_SA) / C_SAC247
# o	Bear River: D_BRR017_23_NA / C_CMPFW
# o	Feather River: (D_THRMF_12_NU1 + D_THRMF_11_NU1 + D_THRMA_WEC000 + D_THRMA_RVC000 + D_THRMA_JBC000) / C_OROVL
# o	Yuba River: D_YUB011_15S_NA2 / (D_YUB011_15S_NA2 + C_YUB002)
# o	Lower-mid Sacramento River: (D_SAC121_08S_SA3 + D_SAC115_19_SA + D_SAC109_08S_SA3 + D_SAC109_19_SA + D_SAC099_19_SA + D_SAC091_19_SA + D_SAC083_21_SA + D_SAC082_22_SA1 + D_SAC081_21_NA + D_SAC078_22_SA1 + D_SAC075_22_NA + D_SAC074_21_SA + D_SAC065_WTPBTB) / C_SAC120
# o	American River: D_AMR007_WTPFBN / C_NTOMA
# o	Lower Sacramento River: (D_SAC050_FPT013 + D_SAC062_WTPSAC) / C_SAC120
# o	Calaveras River: (D_LJC022_60S_PA1 + D_CLV037_CACWD + D_CLV026_60S_PA1 + D_CLV026_WTPWDH) / C_NHGAN
# o	Cosumnes River: NA
# o	Mokelumne River: (D_MOK050_60N_NA3 + D_MOK050_60N_NA5 + D_MOK039_60N_NA5 + D_MOK035_60N_NA4 + D_MOK035_60N_NU1 + D_MOK035_WTPDWS + D_MOK033_60N_NA5) / C_CMCHE
# o	Merced River: (D_MC042_63_NA2 + D_MCD021_63_NA4) / C_MCD050
# o	Stanislaus River: (D_STS030_61_NA4 + D_STS004_61_NA6) / C_STS059
# o	Tuolumne River: (D_TUO047_61_NA3 + D_TUO047_62_NA4 + D_TUO015_61_NA3 + D_TUO015_62_NA4) / C_TUO054
# o	San Joaquin River: (D_SJR062_50_PA1 + D_SJR090_71_NA2 + D_SJR081_61_NA5 + D_SJR116_72_NA1) / (D_SJR062_50_PA1 + D_SJR090_71_NA2 + D_SJR081_61_NA5 + D_SJR116_72_NA1 + C_SJR072)

calsim3_diversion_nodes <- c("D_SAC296_WTPFTH", "D_SAC296_02_SA", "D_SAC294_WTPBLV", "D_SAC294_03_PA",
                             "D_SAC289_03_SA", "D_SAC281_02_NA", "D_SAC273_03_NA", "D_ANT010_05_NA",
                             "D_BTC045_ESL008", "D_BTC043_10_NA", "D_BTC036_10_NA", "D_BTC012_09_SA2",
                             "D_BTC012_CRK005", "D_DRC010_05_NA", "D_DRC005_05_NA", "D_ELD012_04_NA",
                             "D_MLC006_05_NA", "D_STN021_06_PA", "D_THM012_04_NA", "D_SAC240_TCC001",
                             "D_SAC240_05_NA", "D_SAC224_04_NA", "D_SAC207_GCC007", "D_SAC196_MTC000",
                             "D_SAC185_08N_NA", "D_SAC185_09_NA", "D_SAC178_08N_SA1", "D_SAC162_09_SA2",
                             "D_SAC159_08S_SA1", "D_SAC159_08N_SA1", "D_SAC146_08S_NA1", "D_SAC136_18_NA",
                             "D_SAC136_18_SA", "D_SAC129_08S_NA2", "D_SAC122_19_SA", "D_BRR017_23_NA",
                             "D_THRMF_12_NU1", "D_THRMF_11_NU1", "D_THRMA_WEC000", "D_THRMA_RVC000",
                             "D_THRMA_JBC000", "D_YUB011_15S_NA2", "D_SAC121_08S_SA3", "D_SAC115_19_SA",
                             "D_SAC109_08S_SA3", "D_SAC109_19_SA", "D_SAC099_19_SA", "D_SAC091_19_SA",
                             "D_SAC083_21_SA", "D_SAC082_22_SA1", "D_SAC081_21_NA", "D_SAC078_22_SA1",
                             "D_SAC075_22_NA", "D_SAC074_21_SA", "D_SAC065_WTPBTB", "D_AMR007_WTPFBN",
                             "D_SAC050_FPT013", "D_SAC062_WTPSAC", "D_LJC022_60S_PA1", "D_CLV037_CACWD",
                             "D_CLV026_60S_PA1", "D_CLV026_WTPWDH", "D_MOK050_60N_NA3", "D_MOK050_60N_NA5",
                             "D_MOK039_60N_NA5", "D_MOK035_60N_NA4", "D_MOK035_60N_NU1", "D_MOK035_WTPDWS",
                             "D_MOK033_60N_NA5", "D_MCD042_63_NA2", "D_MCD021_63_NA4", "D_STS030_61_NA4",
                             "D_STS004_61_NA6", "D_TUO047_61_NA3", "D_TUO047_62_NA4", "D_TUO015_61_NA3",
                             "D_TUO015_62_NA4", "D_SJR062_50_PA1", "D_SJR090_71_NA2", "D_SJR081_61_NA5",
                             "D_SJR116_72_NA1", "C_SAC273", "C_ANT010", "D_BTC045_ESL008",
                             "D_BTC043_10_NA", "D_BTC036_10_NA", "D_BTC012_09_SA2", "D_BTC012_CRK005",
                             "C_BTC012", "C_DRC005", "C_ELD005", "C_MLC004", "C_STN026", "C_THM005",
                             "C_SAC247", "C_CMPFW", "D_BRR017_23_NA", "C_OROVL", "C_YUB002",
                             "D_YUB011_15S_NA2", "C_SAC120", "C_NTOMA", "C_SAC063", "C_NHGAN",
                             "C_CMCHE", "C_MCD050", "C_STS059", "C_TUO054", "D_SJR062_50_PA1",
                             "D_SJR090_71_NA2", "D_SJR081_61_NA5", "D_SJR116_72_NA1", "C_SJR072"
)

calsim_diversions_wide <- calsim_data |> filter(node %in% calsim3_diversion_nodes) |>
  select(-B) |>
  pivot_wider(names_from = node, values_from = flow_cfs)


lto_total_diverted <-
  calsim_diversions_wide |>
  mutate(
    `div_final_Upper Sacramento River` = sum(D_SAC296_WTPFTH, D_SAC296_02_SA, D_SAC294_WTPBLV, D_SAC294_03_PA, D_SAC289_03_SA, D_SAC281_02_NA, D_SAC273_03_NA),
    `div_final_Antelope Creek` = D_ANT010_05_NA,
    `div_final_Battle Cree` = 0,
    `div_final_Bear Creek` = 0,
    `div_final_Big Chico Creek` = 0,
    `div_final_Butte Creek` = (D_BTC045_ESL008 + D_BTC043_10_NA + D_BTC036_10_NA + D_BTC012_09_SA2 + D_BTC012_CRK005),
    `div_final_Clear Creek` = 0,
    `div_final_Cottonwood Creek` = 0,
    `div_final_Cow Creek` = 0,
    `div_final_Deer Creek` = (D_DRC010_05_NA + D_DRC005_05_NA),
    `div_final_Elder Creek` = D_ELD012_04_NA,
    `div_final_Mill Creek` = D_MLC006_05_NA,
    `div_final_Paynes Creek` = 0,
    `div_final_Stony Creek` = D_STN021_06_PA,
    `div_final_Thomes Creek` = D_THM012_04_NA,
    `div_final_Upper-mid Sacramento River` = (D_SAC240_TCC001 + D_SAC240_05_NA + D_SAC224_04_NA + D_SAC196_MTC000 + D_SAC185_08N_NA + D_SAC185_09_NA + D_SAC178_08N_SA1 + D_SAC162_09_SA2 + D_SAC159_08S_SA1 + D_SAC159_08N_SA1 + D_SAC146_08S_NA1 + D_SAC136_18_NA + D_SAC136_18_SA + D_SAC129_08S_NA2 + D_SAC122_19_SA),
    `div_final_Bear River` = D_BRR017_23_NA,
    `div_final_Feather River` = (D_THRMF_12_NU1 + D_THRMF_11_NU1 + D_THRMA_WEC000 + D_THRMA_RVC000 + D_THRMA_JBC000),
    `div_final_Yuba River` = D_YUB011_15S_NA2,
    `div_final_Lower-mid Sacramento River` = (D_SAC121_08S_SA3 + D_SAC115_19_SA + D_SAC109_08S_SA3 + D_SAC109_19_SA + D_SAC099_19_SA + D_SAC091_19_SA + D_SAC083_21_SA + D_SAC082_22_SA1 + D_SAC081_21_NA + D_SAC078_22_SA1 + D_SAC075_22_NA + D_SAC074_21_SA + D_SAC065_WTPBTB),
    `div_final_American River` = D_AMR007_WTPFBN,
    `div_final_Lower Sacramento River` = (D_SAC050_FPT013 + D_SAC062_WTPSAC),
    `div_final_Calaveras River` = (D_LJC022_60S_PA1 + D_CLV037_CACWD + D_CLV026_60S_PA1 + D_CLV026_WTPWDH),
    `div_final_Cosumnes River` = 0,
    `div_final_Mokelumne River` = (D_MOK050_60N_NA3 + D_MOK050_60N_NA5 + D_MOK039_60N_NA5 + D_MOK035_60N_NA4 + D_MOK035_60N_NU1 + D_MOK035_WTPDWS + D_MOK033_60N_NA5),
    `div_final_Merced River` = (D_MCD042_63_NA2 + D_MCD021_63_NA4),
    `div_final_Stanislaus River` = (D_STS030_61_NA4 + D_STS004_61_NA6),
    `div_final_Tuolumne River` = (D_TUO047_61_NA3 + D_TUO047_62_NA4 + D_TUO015_61_NA3 + D_TUO015_62_NA4),
    `div_final_San Joaquin River` = (D_SJR062_50_PA1 + D_SJR090_71_NA2 + D_SJR081_61_NA5 + D_SJR116_72_NA1)
    )


lto_total_diverted_final <- lto_total_diverted |>
  select(starts_with("div_final")) |>
  rename_with(\(x) str_replace(x, "div_final_", ""))


#write_rds(lto_total_diverted, "data-raw/calsim3/lto-total-diverted.rds")

# lto_proportion_diverted <-
#   lto_total_diverted |>
#   transmute(
#     date,
#     `Upper Sacramento River` = pmin(`div_final_Upper Sacramento River`/C_SAC273, 1),
#     `Antelope Creek` = pmin(`div_final_Antelope Creek`/C_ANT010, 1),
#     `Battle Creek` = 0,
#     `Bear Creek` = 0,
#     `Big Chico Creek` = 0,
#     `Butte Creek` = pmin(`div_final_Butte Creek`/(`div_final_Butte Creek` + C_BTC012), 1),
#     `Clear Creek` = 0,
#     `Cottonwood Creek` = 0,
#     `Cow Creek` = 0,
#     `Deer Creek` = pmin(`div_final_Deer Creek`/C_DRC005, 1),
#     `Elder Creek` = pmin(`div_final_Elder Creek`/C_ELD005, 1),
#     `Mill Creek` = pmin(`div_final_Mill Creek`/C_MLC004, 1),
#     `Paynes Creek` = 0,
#     `Stony Creek` = pmin(`div_final_Stony Creek`/C_STN026, 1),
#     `Thomes Creek` = pmin(`div_final_Thomes Creek`/C_THM005, 1),
#     `Upper-mid Sacramento River` = pmin(`div_final_Upper-mid Sacramento River`/C_SAC247, 1),
#     `Sutter Bypass` = 0,
#     `Bear River` = pmin(`div_final_Bear River`/(`div_final_Bear River` + C_CMPFW), 1),
#     `Feather River` = pmin(`div_final_Feather River`/C_OROVL, 1),
#     `Yuba River` = pmin(`div_final_Yuba River`/(`div_final_Yuba River` + D_YUB011_15S_NA2)),
#     `Lower-mid Sacramento River` = pmin(`div_final_Lower-mid Sacramento River`/(C_SAC120), 1),
#     `Yolo Bypass` = 0,
#     `American River` = pmin(`div_final_American River`/(C_NTOMA), 1),
#     `Lower Sacramento River` = pmin(`div_final_Lower Sacramento River`/(C_SAC063), 1),
#     `Calaveras River` = pmin(`div_final_Calaveras River`/C_NHGAN, 1),
#     `Cosumnes River` = 0,
#     `Mokelumne River` = pmin(`div_final_Mokelumne River`/C_CMCHE, 1),
#     `Merced River` = pmin(`div_final_Merced River`/(C_MCD050), 1),
#     `Stanislaus River` = pmin(`div_final_Stanislaus River`/(C_STS059)),
#     `Tuolumne River` = pmin(`div_final_Tuolumne River`/C_TUO054, 1),
#     `San Joaquin River` = pmin(`div_final_San Joaquin River`/(`div_final_San Joaquin River` + C_SJR072), 1)
#   )


#write_rds(lto_proportion_diverted, "data-raw/calsim3/lto-proportion-diverted.rds")

# Mean flows -------------------------------------------
generate_mean_flow <- function(bypass_flow, flow_cfs) {
  bypass <- bypass_flow |>
    select(date, `Sutter Bypass` = sutter4, `Yolo Bypass` = yolo2)

  mean_flow <- flow_cfs |>
    left_join(bypass) |>
    filter(between(year(date), 1980, 2000)) |>
    # gather(watershed, flow_cfs, -date)
    pivot_longer(`Upper Sacramento River`:`Yolo Bypass`,
                 names_to = "watershed",
                 values_to = "flow_cfs") |>
    filter(watershed != "Lower-mid Sacramento River1") |>
    mutate(flow_cms = DSMflow::cfs_to_cms(flow_cfs),
           watershed = ifelse(watershed == "Lower-mid Sacramento River2",
                              "Lower-mid Sacramento River", watershed)) |>
    select(-flow_cfs) |>
    left_join(DSMflow::watershed_ordering) |>
    pivot_wider(names_from = date,
                values_from = flow_cms) |>
    arrange(order) |>
    select(-order, -watershed) |>
    DSMflow::create_model_array()

  dimnames(mean_flow) <- list(DSMflow::watershed_ordering$watershed,
                              month.abb[1:12],
                              1980:2000)
  return(mean_flow)
}

lto_calsim3_mean_flows <- lto_calsim3_flows |>
  left_join(select(lto_calsim3_bypass_flows, date, `Sutter Bypass` = "sutter4", `Yolo Bypass` = "yolo2")) |>
  filter(between(year(date), 1980, 2000)) |>
  pivot_longer(-date, names_to="watershed", values_to = "flow_cfs") |>
  filter(watershed != "Lower-mid Sacramento River1") |>
  mutate(flow_cms = DSMflow::cfs_to_cms(flow_cfs),
         watershed = ifelse(watershed == "Lower-mid Sacramento River2",
                            "Lower-mid Sacramento River", watershed)) |>
  left_join(DSMflow::watershed_ordering) |>
  arrange(order) |>
  select(-flow_cfs) |>
  pivot_wider(names_from = date, values_from = flow_cms) |>
  select(-order, -watershed) |>
  DSMflow::create_model_array()

write_rds(lto_calsim3_mean_flows, "data-raw/calsim3/calsim3-mean-flow.rds")

# misc flow nodes --------------------------------------

# total diverted --------------------------------------
total_diversion_nodes <- data.frame(input=c(rep("Upper Sacramento River_Div",7), # start diversion terms
                               rep("Antelope Creek_Div",1),
                               rep("Butte Creek_Div",5),
                               rep("Deer Creek_Div",2),
                               rep("Elder Creek_Div",1),
                               rep("Mill Creek_Div",1),
                               rep("Stony Creek_Div",1),
                               rep("Thomes Creek_Div",1),
                               rep("Upper-mid Sacramento River_Div",16), # will need to be updated
                               rep("Bear River_Div",1),
                               rep("Feather River_Div",5),
                               rep("Yuba River_Div",1),
                               rep("Lower-mid Sacramento River_Div",13), # will need to be updated
                               rep("American River_Div",1),
                               rep("Lower Sacramento River_Div",2), # will need to be updated
                               rep("Calaveras River_Div",4),
                               rep("Mokelumne River_Div",7),
                               rep("Merced River_Div",2),
                               rep("Stanislaus River_Div",2),
                               rep("Tuolumne River_Div",4),
                               rep("San Joaquin River_Div",4),
                               rep("Upper Sacramento River_Flow",1), # start flow terms
                               rep("Antelope Creek_Flow",1),
                               rep("Butte Creek_Flow",6),
                               rep("Deer Creek_Flow",1),
                               rep("Elder Creek_Flow",1),
                               rep("Mill Creek_Flow",1),
                               rep("Stony Creek_Flow",1),
                               rep("Thomes Creek_Flow",1),
                               rep("Upper-mid Sacramento River_Flow",1),
                               rep("Bear River_Flow",2),
                               rep("Feather River_Flow",1),
                               rep("Yuba River_Flow",2),
                               rep("Lower-mid Sacramento River_Flow",1),
                               rep("American River_Flow",1),
                               rep("Lower Sacramento River_Flow",1),
                               rep("Calaveras River_Flow",1),
                               rep("Mokelumne River_Flow", 1),
                               rep("Merced River_Flow",1),
                               rep("Stanislaus River_Flow",1),
                               rep("Tuolumne River_Flow",1),
                               rep("San Joaquin River_Flow",5)),
                       nodes=c("D_SAC296_WTPFTH", "D_SAC296_02_SA", "D_SAC294_WTPBLV", "D_SAC294_03_PA", "D_SAC289_03_SA", "D_SAC281_02_NA", "D_SAC273_03_NA",
                               # Above: Upper Sacramento River_Div (prev. D104)
                               "D_ANT010_05_NA", # Antelope Creek_Div (prev. (C11307 / (C11307+C11308+C11309) * D11305))
                               "D_BTC045_ESL008", "D_BTC043_10_NA","D_BTC036_10_NA","D_BTC012_09_SA2","D_BTC012_CRK005", # Butte Creek_Div (previously D217+C217B)
                               "D_DRC010_05_NA", "D_DRC005_05_NA", # Deer Creek_Div (prev. (C11309 / (C11307+C11308+C11309) * D11305))
                               "D_ELD012_04_NA", # Elder Creek_Div (prev. (C11303 / (C11303 + C11304) * D11301))
                               "D_MLC006_05_NA", # Mill Creek_Div (prev. (C11308 / (C11307+C11308+C11309) * D11305))
                               "D_STN021_06_PA", # Stony Creek_Div (prev. D17301)
                               "D_THM012_04_NA", # Thomes Creek_Div (prev. (C11304 / (C11303+C11304) * D11301))
                               "D_SAC240_TCC001", "D_SAC240_05_NA", "D_SAC224_04_NA", "D_SAC207_GCC007", "D_SAC196_MTC000", "D_SAC185_08N_NA", "D_SAC185_09_NA", "D_SAC178_08N_SA1", "D_SAC162_09_SA2", "D_SAC159_08S_SA1", "D_SAC159_08N_SA1", "D_SAC146_08S_NA1", "D_SAC136_18_NA", "D_SAC136_18_SA", "D_SAC129_08S_NA2", "D_SAC122_19_SA",
                               # Above: Upper-mid Sacramento River_Div (prev. D109+D112+D113A+D113B+D114+D118+D122A+D122B+D123+D124A+D128_WTS+D128)
                               "D_BRR017_23_NA", # Bear River_Div (prev. D285)
                               "D_THRMF_12_NU1", "D_THRMF_11_NU1", "D_THRMA_WEC000", "D_THRMA_RVC000", "D_THRMA_JBC000", # Feather River_Div (prev. D201+D202+D7A+D7B)
                               "D_YUB011_15S_NA2", # Yuba River_Div (prev. D230)
                               "D_SAC121_08S_SA3", "D_SAC115_19_SA", "D_SAC109_08S_SA3", "D_SAC109_19_SA", "D_SAC099_19_SA", "D_SAC091_19_SA", "D_SAC083_21_SA", "D_SAC082_22_SA1", "D_SAC081_21_NA", "D_SAC078_22_SA1", "D_SAC075_22_NA", "D_SAC074_21_SA", "D_SAC065_WTPBTB",
                               # Above: Lower-mid Sacramento River_Div (prev. D129A+D134+D162+D165)
                               "D_AMR007_WTPFBN", # American River_Div (prev. D302)
                               "D_SAC050_FPT013", "D_SAC062_WTPSAC", # Lower Sacramento River_Div (prev. D167+D168+D168A_WTS)
                               "D_LJC022_60S_PA1","D_CLV037_CACWD", "D_CLV026_60S_PA1", "D_CLV026_WTPWDH", # Calaveras River_Div (prev. D506A+D506B+D506C+D507)
                               "D_MOK050_60N_NA3","D_MOK050_60N_NA5","D_MOK039_60N_NA5","D_MOK035_60N_NA4","D_MOK035_60N_NU1","D_MOK035_WTPDWS","D_MOK033_60N_NA5", # Mokelumne River_Div (prev. D503A+D503B+D503C+D502A+D502B)
                               "D_MCD042_63_NA2","D_MCD021_63_NA4", # Merced River_Div (prev. D562+D566)
                               "D_STS030_61_NA4","D_STS004_61_NA6", # Stanislaus River_Div (prev. D528)
                               "D_TUO047_61_NA3","D_TUO047_62_NA4","D_TUO015_61_NA3","D_TUO015_62_NA4", # Tuolumne River_Div (prev. D545)
                               "D_SJR062_50_PA1","D_SJR090_71_NA2","D_SJR081_61_NA5","D_SJR116_72_NA1", # San Joaquin River_Div (prev. D637+D630B+D630A+D620B)
                               "C_SAC273", # Upper Sacramento River_Flow (prev. C104)
                               "C_ANT010", # Antelope Creek_Flow (prev. C11307)
                               "D_BTC045_ESL008", "D_BTC043_10_NA","D_BTC036_10_NA","D_BTC012_09_SA2","D_BTC012_CRK005", "C_BTC012", # Butte Creek_Flow (prev. C217B+D217+C217A)
                               "C_DRC005", # Deer Creek_Flow (prev. C11309)
                               "C_ELD005", # Elder Creek_Flow (prev. C11303)
                               "C_MLC004", # Mill Creek_Flow (prev. C11308)
                               "C_STN026", # Stony Creek_Flow (prev. C42)
                               "C_THM005", # Thomes Creek_Flow (prev. C11304)
                               "C_SAC247", # Upper-mid Sacramento River_Flow (prev. C110)
                               "C_CMPFW","D_BRR017_23_NA", # Bear River_Flow (prev. C285+D285)
                               "C_OROVL", # Feather River_Flow (prev. C6)
                               "C_YUB002","D_YUB011_15S_NA2", # Yuba River_Flow (prev. C230+D230)
                               "C_SAC120", # Lower-mid Sacramento River_Flow (prev. C128)
                               "C_NTOMA", # American River_Flow (prev. C9)
                               "C_SAC063", # Lower Sacramento River_Flow (prev. C166)
                               "C_NHGAN", # Calaveras River_Flow (prev. C92)
                               "C_CMCHE", # Mokelumne River_Flow (prev. C91)
                               "C_MCD050", # Merced River_Flow (prev. C561)
                               "C_STS059", # Stanislaus River_Flow (prev. 520)
                               "C_TUO054", # Tuolumne River_Flow (prev. 540)
                               "D_SJR062_50_PA1","D_SJR090_71_NA2","D_SJR081_61_NA5","D_SJR116_72_NA1","C_SJR072"), # San Joaquin River_Flow (prev. D637+D630B+D630A+D620B+C637)
                       type=c(rep("DIVERSION", 81), # These will need to be updated as Sacramento River regions are updated
                              rep("CHANNEL", 2), rep("DIVERSION",5), rep("CHANNEL",8), "DIVERSION",
                              rep("CHANNEL",2), "DIVERSION", rep("CHANNEL",8), rep("DIVERSION",4),
                              "CHANNEL"))



# upper sacramento flow ---------------------------------------------
upper_sacramento_flows <- calsim_data |>
  filter(node == "C_SAC257") |>
  select(-B, -node, upsacQcfs=flow_cfs) |>
  mutate(upsacQcms = DSMflow::cfs_to_cms(upsacQcfs),
         year = year(date),
         month = month(date)) |>
  filter(year >= 1980, year <= 2000) |>
  select(-date, -upsacQcfs) |>
  pivot_wider(names_from = year,
              values_from = upsacQcms) |>
  select(-month) |>
  as.matrix()


# proportion flow natal --------------------------------------------
lto_calsim3_flows |>
  mutate(`Lower-mid Sacramento River` = 35.6/58*`Lower-mid Sacramento River1` + 22.4/58*`Lower-mid Sacramento River2`,
         `Upper Sacramento River` = `Upper Sacramento River`/`Upper-mid Sacramento River`,
         `Antelope Creek` = `Antelope Creek`/`Upper-mid Sacramento River`,
         `Battle Creek` = `Battle Creek`/`Upper-mid Sacramento River`,
         `Bear Creek` = `Bear Creek`/`Upper-mid Sacramento River`,
         `Big Chico Creek` = `Big Chico Creek`/`Upper-mid Sacramento River`,
         `Butte Creek` = `Butte Creek`/`Upper-mid Sacramento River`,
         `Clear Creek` = `Clear Creek`/`Upper-mid Sacramento River`,
         `Cottonwood Creek` = `Cottonwood Creek`/`Upper-mid Sacramento River`,
         `Cow Creek` = `Cow Creek`/`Upper-mid Sacramento River`,
         `Deer Creek` = `Deer Creek`/`Upper-mid Sacramento River`,
         `Elder Creek` = `Elder Creek`/`Upper-mid Sacramento River`,
         `Mill Creek` = `Mill Creek`/`Upper-mid Sacramento River`,
         `Paynes Creek` = `Paynes Creek`/`Upper-mid Sacramento River`,
         `Stony Creek` = `Stony Creek`/`Upper-mid Sacramento River`,
         `Thomes Creek` = `Thomes Creek`/`Upper-mid Sacramento River`,
         `Upper-mid Sacramento River` = 1,
         `Sutter Bypass` = 0,
         `Bear River` = `Bear River`/`Feather River`,
         `Yuba River` = `Yuba River`/`Feather River`,
         `Feather River` = `Feather River`/`Lower-mid Sacramento River2`,
         `Lower-mid Sacramento River` = 1,
         `Yolo Bypass` = 0,
         `American River` = `American River`/`Lower Sacramento River`,
         `Lower Sacramento River` = 1,
         `Calaveras River` = 1,
         `Cosumnes River` = 1,
         `Mokelumne River` = 1,
         `Merced River` = `Merced River`/`San Joaquin River`,
         `Stanislaus River` = `Stanislaus River`/`San Joaquin River`,
         `Tuolumne River` = `Tuolumne River`/`San Joaquin River`,
         `San Joaquin River` = 1) |>
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), year=year(date)) |>
  filter(year %in% (1979:2000), month == "Oct") |>  # summarize for month of October
  as.data.frame() |>
  pivot_longer(cols = `Upper Sacramento River`:`Yolo Bypass`,
               names_to = "input", values_to = "value") |>
  mutate(value = pmin(value,1)) |>  # Ensure no proportions exceed 1
  filter(input %in% inputs) |>
  ungroup() |>
  mutate(input2 = factor(input, levels=inputs)) |>  # re-order according to DSMflow object
  select(year, input2, value) |>
  melt(id=c("year", "input2")) |>
  acast(input2~year)






