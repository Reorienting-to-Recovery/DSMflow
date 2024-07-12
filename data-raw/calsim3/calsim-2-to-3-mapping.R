library(tidyverse)


process_dss_output <- function(file) {
  raw_data <- readxl::read_excel(file)
  raw_data |>
    pivot_longer(-c(B, date), values_to = "flow_cfs", names_to = "node") |>
    mutate(
      date = date - years(
        case_when(
          year(date) >= 2022 ~ 100,
          year(date) == 2021 & month(date) < 10 ~ 100,
          TRUE ~ 0
        )
      )
    )
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




