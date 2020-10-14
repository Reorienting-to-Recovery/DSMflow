## depricate

* med_flow - was medQ in model, not used
```
#' Median Flow
#' @description The monthly median flows of 1980-2000 in cubic feet per second
#' @format a dataframe with 31 rows and 13 variables
#' \describe{
#'   \item{watershed}{CVPIA watershed}
#'   \item{1}{January median flow}
#'   \item{2}{February median flow}
#'   \item{3}{March median flow}
#'   \item{4}{April median flow}
#'   \item{5}{May median flow}
#'   \item{6}{June median flow}
#'   \item{7}{July median flow}
#'   \item{8}{August median flow}
#'   \item{9}{September median flow}
#'   \item{10}{October median flow}
#'   \item{11}{November median flow}
#'   \item{12}{December median flow}
#' }
#' @details
#'
#' Calculated using \code{\link{flows_cfs}}
#'
"med_flow"

med_flow <- DSMflow::flows_cfs %>%
  filter(between(year(date), 1980, 2000)) %>%
  mutate(`Lower-mid Sacramento River` = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 *`Lower-mid Sacramento River2`) %>%
  select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`) %>%
  gather(watershed, flow, -date) %>%
  group_by(month = month(date), watershed) %>%
  summarise(median_flow = median(flow)) %>%
  # mutate(prop_pulse = replace(prop_pulse, is.infinite(prop_pulse), 0)) %>%
  select(month, watershed, median_flow) %>%
  spread(month, median_flow) %>%
  bind_rows(byp) %>%
  left_join(cvpiaData::watershed_ordering) %>%
  arrange(order) %>%
  select(-order)

# usethis::use_data(med_flow, overwrite = TRUE)
```

* bypass_over - was gate.top in model, not used

```
#' Sutter and Yolo Bypasses Overtopped
#' @description A dataset containing a boolean representation of bypasses overtopped
#' used for adult pre-spawning survival.
#'
#' @format dataframe with 252 rows and 3 variables:
#' \describe{
#' \item{date}{CALSIM II date}
#' \item{sutter}{TRUE if D117 + D124 + D125 + D126 + C137 >= 100 cfs}
#' \item{yolo}{TRUE if D160 + C157 >= 100 cfs}
#' }
#'
#' @details The flow upstream and down stream of the bypasses are represented using
#' 'FLOW-CHANNEL' and 'FLOW-DELIVERY' nodes from CALSIM II.
#' The nodes for each watershed are outlined above. If the flow into the bypasses is
#' greater than 100 cfs the bypass is considered overtopped.
#'
#'
#' \href{https://s3-us-west-2.amazonaws.com/DSMflow-r-package/BST_CALSIMII_schematic_040110.jpg}{CALSIM II schematic}
#'
#' @source
#' \itemize{
#'   \item \strong{Data Wrangling:} Sadie Gill  \email{sgill@@flowwest.com}
#'   \item \strong{Node Selection:} Mark Tompkins \email{mtompkins@@flowwest.com}
#'   \item \strong{CALSIM Model Output:} Michael Wright \email{mwright@@usbr.gov}
#' }
#'
"bypass_overtopped"

# bypass overtopped --------------------
# overtopped is > 100 cfs
bypass_overtopped <- calsim %>%
  mutate(sutter = D117 + D124 + D125 + D126 + C137,
         yolo = D160 + C157) %>%
  select(date, sutter, yolo) %>%
  filter(between(year(date), 1979, 1999)) %>%
  gather(bypass, flow, - date) %>%
  mutate(overtopped = flow >= 100) %>%
  select(-flow) %>%
  spread(bypass, overtopped)

use_data(bypass_overtopped)

d <- 1:12
names(d) <- month.name

# yolo and sutter(includes tisdale) overtopping
# flow in bypass for adults is 1
# bypass_over_top <-
bpo <- DSMflow::bypass_overtopped %>%
  gather(bypass, overtopped, -date) %>%
  spread(date, overtopped)

sutter_overtopped <- DSMflow::bypass_overtopped %>%
  filter(between(year(date), 1980, 1999)) %>%
  gather(bypass, overtopped, -date) %>%
  filter(bypass == "sutter") %>%
  mutate(month = month(date),
         year = year(date)) %>%
  select(-date, -bypass) %>%
  spread(year, overtopped) %>%
  arrange(month) %>%
  select(-month) %>%
  as.matrix()

yolo_overtopped <- DSMflow::bypass_overtopped %>%
  filter(between(year(date), 1980, 1999)) %>%
  gather(bypass, overtopped, -date) %>%
  filter(bypass == "yolo") %>%
  mutate(month = month(date),
         year = year(date)) %>%
  select(-date, -bypass) %>%
  spread(year, overtopped) %>%
  arrange(month) %>%
  select(-month) %>%
  as.matrix()

bypass_over <- array(as.logical(NA), dim = c(12, 20, 2))
bypass_over[ , , 1] <- sutter_overtopped
bypass_over[ , , 2] <- yolo_overtopped
# usethis::use_data(bypass_over, overwrite = TRUE)
```

* freeportQ, is calib_data$Q_free the same
```
#' The Flow from Lower Sacramento River into the Central/South Delta
#' @description A dataset containing the flow at Freeport Weir. To be used for routing fish from the Lower
#' Sacramento River into the Central/South delta in the SIT Salmon Population Model.
#'
#' @format dataframe with 996 rows and 3 variables:
#' \describe{
#' \item{date}{CALSIM II date}
#' \item{freeportQcfs}{C400 - flow in cubic feet per second}
#' \item{freeportQcms}{C400 - flow in cubic meters per second}
#' }
#'
#' @details The flow at Freeport Weir is represented using node CALSIM II 'FLOW-CHANNEL' C400.
#'
#'
#' \href{https://s3-us-west-2.amazonaws.com/DSMflow-r-package/BST_CALSIMII_schematic_040110.jpg}{CALSIM II schematic}
#'
#' @source
#' \itemize{
#'   \item \strong{Data Wrangling:} Sadie Gill  \email{sgill@@flowwest.com}
#'   \item \strong{Node Selection:} Mark Tompkins \email{mtompkins@@flowwest.com} and Mike Urkov \email{mike.urkov@@gmail.com}
#'   \item \strong{CALSIM Model Output:} Michael Wright \email{mwright@@usbr.gov}
#' }
#'
"freeportQ"
freeportQ <- read_csv('data-raw/MikeWrightCalSimOct2017/C169-422.csv', skip = 1) %>%
  select(date = X2, C400) %>%
  filter(!is.na(date)) %>%
  mutate(date = dmy(date),
         freeportQcfs = as.numeric(C400),
         freeportQcms = cfs_to_cms(freeportQcfs)) %>%
  select(date, freeportQcfs, freeportQcms) %>%
  filter(!is.na(freeportQcfs))

usethis::use_data(freeportQ)

#' Flow at Freeport
#' @description The inflow at Freeport in cubic meters per second from 1980-2000.
#'
#' @format A dataframe with 12 rows and 21 variables
#' Each row represents a month, each column a year from 1980-2000.
#' This data is used to route fish into the delta.
#'
#' @details For more details see:
#'  \itemize{
#'   \item use this link within R \code{\link[DSMflow]{freeportQ}}
#'   \item use this \href{https://flowwest.github.io/DSMflow/reference/freeportQ.html}{link} if in a web browser
#' }
#'
#'
"freeportQcms"
# flow at freeport
# TODO freeport_flows <- calib_data$Q_free, is calib_data$Q_free the same?
freeportQcms <- DSMflow::freeportQ %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year >= 1980, year <= 2000) %>%
  select(-date, -freeportQcfs) %>%
  spread(year, freeportQcms) %>%
  select(-month) %>%
  as.matrix()

rownames(freeportQcms) <- month.abb[1:12]

usethis::use_data(freeportQcms, overwrite = TRUE)

```
