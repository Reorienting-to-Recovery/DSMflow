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


calsim_flow <- calsim_data |> filter(node %in% watershed_to_nodes) |>
  mutate(watershed = nodes_to_watershed[node]) |>
  select(-B, -node) |>
  pivot_wider(names_from = "watershed", values_from = "flow_cfs") |>
  mutate(date = as_date(date))

calsim_flow |> View()

write_rds(calsim_flow, "data-raw/calsim3/calsim3-flow-baseline.rds")


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




calsim_diversions_wide <- calsim_data |> filter(node %in% calsim3_diversion_nodes$nodes) |>
  select(-B) |>
  pivot_wider(names_from = node, values_from = flow_cfs)


calsim_diversions_wide |>
  transmute(
    `Upper Sacramento River` = (D_SAC296_WTPFTH + D_SAC296_02_SA + D_SAC294_WTPBLV + D_SAC294_03_PA + D_SAC289_03_SA + D_SAC281_02_NA + D_SAC273_03_NA) / C_SAC273,
    `Antelope Creek` = D_ANT010_05_NA / C_ANT010,
    `Battle Cree` = 0,
    `Bear Creek` = 0,
    `Big Chico Creek` = 0,
    `Butte Creek` = (D_BTC045_ESL008 + D_BTC043_10_NA + D_BTC036_10_NA + D_BTC012_09_SA2 + D_BTC012_CRK005) / (D_BTC045_ESL008 + D_BTC043_10_NA + D_BTC036_10_NA + D_BTC012_09_SA2 + D_BTC012_CRK005 + C_BTC012),
    `Clear Creek` = 0,
    `Cottonwood Creek` = 0,
    `Cow Creek` = 0,
    `Deer Creek` = (D_DRC010_05_NA + D_DRC005_05_NA) / C_DRC005,
    `Elder Creek` = D_ELD012_04_NA / C_ELD005,
    `Mill Creek` = D_MLC006_05_NA / C_MLC004,
    `Paynes Creek` = 0,
    `Stony Creek` = D_STN021_06_PA / C_STN026,
    `Thomes Creek` = D_THM012_04_NA / C_THM005,
    )

# Mean flows -------------------------------------------

# misc flow nodes --------------------------------------

