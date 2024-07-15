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
  pivot_wider(names_from = "watershed", values_from = "flow_cfs")

calsim_flow |> View()





