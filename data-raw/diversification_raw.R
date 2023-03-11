## code to prepare `diversification_raw` dataset goes here
# load the necessary libraries
library(tidyverse)
library(rlang)
library(fpp3)
library(future)
library(parallel)
library(furrr)
library(fdutils)

# load the source file containing important functions
# source("~/Dropbox/R_packages/diversification/data-raw/functions.R")

# detect available number of cores
ncores <- detectCores()

# prepare for parallel computing with 'future'
plan(multisession, workers = ncores)

# data
data_raw_pos <- "~/Dropbox/R_packages/fdutils/data-raw/diversification_raw.xlsx"
raw_df <- readxl::read_xlsx(data_raw_pos)

# select data
diversification_df <- raw_df  %>%
  transmute(
    # --- identifiers
    id_panel        = id_panel,
    id_time         = id_time,

    # --- panel info
    brand = brand_name,
    parent = parent_company,
    category = category,
    sector = sector,

    # --- panel info
    year = year,
    weeknum = week_num_in_year,

    # ---- dependent variables
    adawareness     = adaware,
    brandawareness  = aided,
    attention       = attention,
    buzz            = buzz,
    consideration   = consider,
    currentowner    = current_own,
    formerowner     = former_own,
    impression      = impression,
    yougovindex     = index,
    intention       = likelybuy,
    perquality      = quality,
    recommendation  = recommend,
    reputation      = reputation,
    satisfaction    = satisfaction,
    pervalue.       = value,
    wordofmnouth    = wom,

    # ---- advertising variables
    adspend         = total_wk_tv_ads,
    adcount         = airings_count_wk,
    ## advertising spending in $1000
    adspend         = round(adspend / 1000),
    adcount         = adcount * (adspend > 0),

    # --- diversification variables
    num_networks    = num_unique_networks_wk,
    num_genres      = num_unique_genres_wk,
    num_dayparts1   = num_unique_dayparts1_wk,
    num_dayparts2   = num_unique_dayparts2_wk,
    num_dayhours    = num_unique_hours_wk,
    num_weekdays    = num_unique_weekdays_wk,
    hhi_networks    = hhi_network,
    hhi_genres      = hhi_genre,
    hhi_dayparts1   = hhi_daypart1,
    hhi_dayparts2   = hhi_daypart2,
    hhi_dayhours    = hhi_hours_of_day,
    hhi_weekdays    = hhi_days_in_week,
    sd_networks     = stdev_network_share,
    sd_genres       = stdev_genre_share,
    sd_dayparts1    = stdev_daypart1_share,
    sd_dayparts2    = stdev_daypart2_share,
    sd_dayhours     = stdev_hours_of_day_share,
    sd_weekdays     = stdev_days_in_week_share,

    # --- category - level moderators
    risk1           = Risk1,
    risk2           = Risk2,
    risk3           = Risk3,
    involvement1    = Involvement1,
    involvement2    = Involvement2,
    utilitarian1    = Util_value1,
    utilitarian2    = Util_value2,
    hedonic         = Hedonic_value,
    budgetshare     = Share_of_budget,
    purchasefreq    = Purchase_frequency
  ) %>%
  distinct()

panel_df <- diversification_df %>%
  select(
    id_panel,
    id_time,
    year,
    weeknum,
    brand,
    parent,
    category,
    sector,
    adawareness,
    impression,
    consideration,
    intention,
    adspend,
    contains(c("networks", "genres", "dayparts1")) &
      !contains("sd"),
    risk1,
    involvement1,
    utilitarian1,
    hedonic
  )

names(panel_df) <- c(
  "id", "t",
  "yr", "wk",
  str_c("key", seq_along(c("brand", "parent", 'category', "sector"))),
  str_c("y", seq_along(c("adawareness",
                         "impression",
                         "consideration",
                         "intention"))),
  "xa",
  "xn1", "xh1",
  'xn2', "xh2",
  "xn3", "xh3",
  "z1", "z2", "z3", "z4"
)

panel_df <- panel_df %>%
  mutate(
    across(c(id, t, contains("key")), as_factor),
    id = str_c("id_", numpad(as.integer(id))),
    t = str_c("id_", numpad(as.integer(t))),
    # t = as.integer(t),
    key1 = str_c("k1_",numpad(as.integer(key1))),
    key2 = str_c("k2_",numpad(as.integer(key2))),
    key3 = str_c("k3_",numpad(as.integer(key3))),
    key4 = str_c("k4_",numpad(as.integer(key4))),
    across(c(id, t, contains("key")), as_factor),
    yr = as.integer(yr),
    wk = as.integer(wk)
  )

usethis::use_data(diversification_df, overwrite = TRUE)
usethis::use_data(panel_df, overwrite = TRUE)
