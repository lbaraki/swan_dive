# PROJECT: Helpers 
# PURPOSE: Prep Files for Data Review
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  86bb3e03
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
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
                               pattern = "PSNU_IM_FY22-24.*Botswana")
    #genie_path <- return_latest(folderpath = merdata,
     #                           pattern = "Genie.*Botswana")
      
  # Grab metadata
   metadata <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "86bb3e03"
    
  # Functions  
    every_nth = function(n) {
      return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
    }  
    

# Helper Functions ============================================================================  


# Shortened mech names ----------------------------------------------------

    # Replaces mechanism names with a shorter version
    fix_mech_names <- function(.data) {
      .data %>%
        dplyr::left_join(mech_names_cw, by = c("mech_code", "mech_name")) %>%
        dplyr::mutate(mech_name = ifelse(
          !is.na(mech_name_short),
          mech_name_short,
          mech_name
        ))
    }  
    

# GT Functions ------------------------------------------------------------

    # Given two metrics, formats achievement
    format_achv <- function(x, y){
      str_c(scales::label_number(accuracy = 1, scale_cut = cut_short_scale())(x), 
            scales::label_number(accuracy = 1, scale_cut = cut_short_scale())(y), sep = " / "
      )
    } 
    
    # Formats an indicator for a table  
    format_indicator <- function(x, y, z){
      name <- stringr::word(x, 1)
      name2 <- stringr::word(y, start = 1, end = 3)
      color <- stringr::word(z)
      
      glue::glue("<div style='line-height:10px'<span style='font-weight:bold;font-size:14px;color:{color}'>{name}</div>
             <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>")
    }
    
    # Create a gt loop to format indicators
    legend_chunk <- gt::md(glue::glue("Achievement legend: <img src= '{selfdestructin5::legend_snapshot}' style='height:15px;'> "))
   
    

# Index Testing Modalities ------------------------------------------------

    munge_modality_mech <- function(df, begin_pd = "FY20Q1", ...){   
      df_hts_full <- df %>% 
        filter(indicator == "HTS_TST_POS",
               standardizeddisaggregate == "Modality/Age/Sex/Result",
               fiscal_year <= metadata$curr_fy) %>% 
        mutate(mod_type = case_when(
          str_detect(modality, "Index") ~ "Index",
          str_detect(modality, "OtherPITC") ~ "Other PITC",
          str_detect(modality, "PMTCT") ~ "PMTCT",
          modality == "VCT" ~ "VCT",
          str_detect(modality, "SNSMod") ~ "Community SNS",
          TRUE ~ "Other")
        ) %>%
        group_by(fiscal_year, mod_type, mech_name) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd() %>%
        select(-period_type) %>%
        group_by(period) %>%
        mutate(contribution = value/sum(value)) %>%
        ungroup() %>%
        mutate(start = case_when(period == min(period) ~ contribution),
               end = case_when(period == max(period) ~ contribution)) %>%
        mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
        complete(mod_type, period, mech_name) %>% 
        group_by(mod_type, mech_name) %>% 
        fill(mod_order, .direction = "up") %>% 
        group_by(period, mech_name) %>% 
        mutate(pd_tot = sum(value, na.rm = T), 
               pd_25 = pd_tot * 0.25, 
               pd_50 = pd_tot * 0.5,
               pd_75 = pd_tot * 0.75) %>% 
        ungroup() %>% 
        mutate(mod_color = case_when(
          mod_type == "Index" ~ "#855C75", 
          mod_type == "VCT" ~ "#D9AF6B",
          mod_type == "Other PITC" ~ "#AF6458",
          mod_type == "PMTCT"~ "#736F4C",
          mod_type == "Community SNS" ~ "#526A83",
          TRUE ~ "#7C7C7C"
        ),
        note = case_when(
          mod_type == "Index" & period == begin_pd ~ "HTS_TST_POS",
          TRUE ~ NA_character_
        )) %>% 
        filter(!is.na(mod_order))
      return(df_hts_full)
    }
    
    munge_modality_index <- function(df, ...){   
      df_hts_full <- df %>% 
        filter(indicator == "HTS_TST_POS",
               standardizeddisaggregate == "Modality/Age/Sex/Result",
               fiscal_year <= metadata$curr_fy) %>% 
        mutate(mod_type = case_when(
          str_detect(modality, "Index") ~ "Index",
          TRUE ~ "Other")
        ) %>%
        group_by(fiscal_year, mod_type, ...) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd() %>%
        select(-period_type) %>%
        group_by(period, ...) %>%
        mutate(contribution = value/sum(value)) %>%
        ungroup() %>%
        mutate(start = case_when(period == min(period) ~ contribution),
               end = case_when(period == max(period) ~ contribution)) %>%
        mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
        complete(mod_type, period, ...) %>% 
        group_by(mod_type, ...) %>% 
        fill(mod_order, .direction = "up") %>% 
        group_by(period, ...) %>% 
        mutate(pd_tot = sum(value, na.rm = T), 
               pd_25 = pd_tot * 0.25, 
               pd_50 = pd_tot * 0.5,
               pd_75 = pd_tot * 0.75) %>% 
        ungroup() %>% 
        mutate(mod_color = case_when(
          mod_type == "Index" ~ "#855C75", 
          TRUE ~ "#D9AF6B"
        ),
        note = case_when(
          mod_type == "Index" & period == "FY20Q1" ~ "HTS_TST_POS",
          TRUE ~ NA_character_
        )) %>% 
        filter(!is.na(mod_order))
      return(df_hts_full)
    }  
    

# Colors ------------------------------------------------------------------

    # Check intersection of two columns
    # Function to compare two dataframe columns, return a count of differences
    compare_vars <- function(x, y) {
      
      # Compare each variable, both ways
      xy <- length(setdiff(x, y))
      yx <- length(setdiff(y, x))
      
      if(xy == 0 & yx == 0){
        return("There are no differences between the columns")
      }
      
      print(str_c(xy, " differences between x and y"))
      print(str_c(yx, " differences between y and x"))
    }
    

# SPINDOWN ============================================================================

