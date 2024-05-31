# PROJECT: Botswana COP23 Approval Memo 
# PURPOSE: Munge and Analysis of TST 
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  113d1959
# LICENSE: MIT
# DATE:   2024-04-08
# NOTES:   

# LOCALS & SETUP ============================================================================
# REF ID for plots
ref_id <- "288252e0"

# Libraries
install.packages('tameDP', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
#alt: install from GitHub using pak
#install.packages("pak")
#pak::pak("USAID-OHA-SI/tameDP")
library(tameDP)
library(tidyverse)
library(gagglr)
library(here)
library(getPass)

load_secrets()  

# LOAD DATA ============================================================================  

#Load Datapack/TST
dp_filepath <-   glamr::return_latest("Data", "Target Setting Tool_Botswana")



df_tst <- tame_dp(dp_filepath)
glimpse(df_tst)

#Load PSNUxIM - separate file to include once available
psnuim_filepath <- glamr::return_latest("Data", "PSNUxIM_Botswana")


# MUNGE ============================================================================

#Returns PLHIV/SUBNAT data
df_plhiv <- tame_dp(dp_filepath, type = "SUBNAT") 
glimpse(df_plhiv)

#Return Targets data
df_all <- tame_dp(dp_filepath, type = "ALL") #default
glimpse(df_all)

#Return PSNUxIM data
df_dp_mech <- tame_dp(psnuim_filepath, type = "PSNUxIM", map_names = TRUE) #param map_names to map in funding agency, partners, mechs 
glimpse(df_dp_mech)

#Append datasets 
#df_combo <- bind_rows(df_plhiv, df_all)
#glimpse(df_combo)


# VIZ ============================================================================

level_fac <- c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
               "HTS_SELF", "HTS_TST", "HTS_TST_POS", "HTS_RECENT", "HTS_INDEX",
               "PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID", 
               "TB_STAT", "TB_ART","TB_PREV", "TX_TB_D",
               "VMMC_CIRC", "KP_PREV", "PrEP_NEW", "PrEP_CT", "CXCA_SCRN",
               "PP_PREV", "OVC_SERV", "OVC_HIVSTAT",
               "GEND_GBV", "AGYW_PREV")

#Table 1: Prioritization Table - pg 9 & 10 
table_1 <- df_all %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2025,
         indicator %in% c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
                          "HTS_SELF", "HTS_TST", "HTS_TST_POS", "HTS_RECENT", "HTS_INDEX",
                          "PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID", 
                          "TB_STAT", "TB_ART","TB_PREV", "TX_TB_D",
                          "VMMC_CIRC", "KP_PREV", "PrEP_NEW", "PrEP_CT", "CXCA_SCRN",
                          "PP_PREV", "OVC_SERV", "OVC_HIVSTAT",
                          "GEND_GBV", "AGYW_PREV"
         )) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                  ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
                                  TRUE ~ ageasentered)) %>% 
  group_by(fiscal_year, indicator, standardizeddisaggregate, 
           #snuprioritization
           #modality
  ) %>% #include trendcoarse
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_RECENT", "HTS_INDEX")) %>% 
  filter(standardizeddisaggregate %ni% c("KeyPop/Result", "KeyPop/HIVStatus")) %>% 
    pivot_wider(names_from = snuprioritization,
                values_from = targets) %>% 
    mutate_all(replace_na, replace = 0) %>% 
    #mutate(total = `7 - Attained` +`1 - Scale-up: Saturation`) %>% 
    arrange(factor(indicator, levels = level_fac)) %>% 
    View()

#Table 2: Agency Table - pg 19 & 20
table_2 <- df_dp_mech %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2025,
         indicator %in% c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
                          "HTS_SELF", "HTS_TST", "HTS_TST_POS","HTS_RECENT", "HTS_INDEX", 
                          "TB_STAT", "TB_ART",
                          "TB_PREV", "TX_TB_D",
                          "PrEP_NEW", "PrEP_CT")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                  ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
                                  TRUE ~ ageasentered)) %>% 
  group_by(fiscal_year, indicator, #trendscoarse
           funding_agency) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "funding_agency", values_from = "targets") %>% 
  #filter(!is.na(trendscoarse)) %>% 
  arrange(factor(indicator, levels = level_fac)) #%>% 
  #View()
