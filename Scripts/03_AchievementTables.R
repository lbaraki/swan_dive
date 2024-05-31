# PROJECT: Achievement Tables
# PURPOSE: Munge and Analysis of FY24Q2 Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  c9a8969d
# LICENSE: MIT
# DATE:   2024-05-30
# NOTES:   

# LOCALS & SETUP ============================================================================
# Libraries
library(gagglr)
library(tidyverse)
library(scales)
library(sf)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gt)
library(gtExtras)
library(selfdestructin5)


# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_FY22-24.*Botswana")

# Grab metadata
meta <- get_metadata(file_path)

# REF ID for plots
ref_id <- "fce102b3"

# Functions  


clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

mk_ptr_tbl <- function(df, mech_id)  {
  
  ip_mdb <- 
    df %>% 
    filter(mech_code == mech_id) %>% 
    make_mdb_df() %>% 
    reshape_mdb_df(., metadata$curr_pd) 
  
  mech_name <-  
    df %>% 
    filter(mech_code == mech_id) %>%
    distinct(mech_name) %>% 
    pull(mech_name)
  
  ip_mdb %>%   
    create_mdb(ou = "Botswana", type = "main", metadata$curr_pd, metadata$source) %>% 
    tab_header(
      title = glue::glue("{mech_name} PERFORMANCE SUMMARY")
    ) %>% 
    gtsave(path = "Images", filename = glue::glue("{mech_name}_mdb_main.png"))
}

# Given two metrics, formats achievement
format_achv <- function(x, y){
  str_c(scales::label_number(accuracy = 1, scale_cut = cut_short_scale())(x), 
        scales::label_number(accuracy = 1, scale_cut = cut_short_scale())(y), sep = " / "
  )
}


# LOAD DATA ============================================================================  

df_msd <- read_psd(file_path) %>% filter(
  fiscal_year %in% c(2023:2024),
  #indicator %in% c("OVC_SERV")
  ) %>% 
  clean_agency()


# MUNGE ============================================================================
  
  #Summary Table 
mdb_df <- make_mdb_df(df_msd)
mdb_tbl <- reshape_mdb_df(mdb_df, metadata$curr_pd) #%>% glimpse()

#Treatment Data Frame
#mdb_df_tx <- make_mdb_tx_df(df_msd)


# VIZ ============================================================================

#Agency Table
mdb_tbl %>% 
  filter(indicator == "OVC_SERV") %>%
  create_mdb(ou = "Botswana", type = "main", metadata$curr_pd, metadata$source) %>% 
  gtsave(path = "Images", filename = glue::glue("Botswana_{metadata$curr_pd}_mdb_main.png"))

#Summary Ach Table 
df_usaid <- df_msd %>% 
  filter(funding_agency == "USAID")

df_achv <- df_msd %>% 
  filter(indicator %in% c(#"OVC_SERV"
    "VMMC_CIRC", "PrEP_NEW", "HTS_TST_POS", "TX_NEW", "TX_CURR"
    ),
         standardizeddisaggregate == "Total Numerator", 
         fiscal_year == metadata$curr_fy, 
         funding_agency != "DEDUP",
    mech_name != "[Placeholder - 160043 Botswana USAID]",
    mech_code != "160030" 
    ) %>% 
  group_by(fiscal_year, 
           mech_code, mech_name, 
           funding_agency,
           indicator) %>% 
  summarise(across(matches("cumul|targets"), sum, na.rm = T), .groups = "drop") %>% 
  calc_achievement() %>% 
  adorn_achievement(qtr = metadata$curr_qtr) %>% 
  mutate(tgt_rslt_sum = format_achv(cumulative, targets))

# SPINDOWN ============================================================================

