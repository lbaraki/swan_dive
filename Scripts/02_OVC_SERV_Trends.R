# PROJECT: OVC_SERV Achievement
# PURPOSE: Munge and Analysis of FY24Q2 Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  fce102b3
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
      
  # Grab metadata
   meta <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "fce102b3"
    
  # Functions  
    #MSD from Pano
    #grabr::pano_extract_msd(operatingunit = "Botswana",
                           #version = "initial", #defaults to current version
                          #fiscal_year = "2024",
                         #quarter = 2,
     #                   level = "psnu") #default destination is si_path()
    
    clean_number <- function(x, digits = 0){
      dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                       x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                       x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                       TRUE ~ glue("{x}"))
    }

    
      
# LOAD DATA ============================================================================  

 df_ovc <- read_psd(file_path) %>% filter(indicator %in% c("OVC_SERV")) %>% 
      clean_agency()

# MUNGE ============================================================================
  
  #OVC_SERV: Q2 Results/Targets for PEPFAR 
    num_ovc <- df_ovc %>%
      filter(#funding_agency %in% c("USAID"),
             standardizeddisaggregate %in% c("Total Numerator"),
             fiscal_year %in% c(2023:2024)) %>% 
      group_by(fiscal_year, indicator,
               #funding_agency
               ) %>% 
      summarise(across(matches("targets|qtr"), sum, na.rm = T), .groups = "drop") %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results/targets)
    
    #USAID
    usaid_ovc <- df_ovc %>%
      filter(funding_agency %in% c("USAID"),
        standardizeddisaggregate %in% c("Total Numerator"),
        fiscal_year %in% c(2023:2024)) %>% 
      group_by(fiscal_year, indicator,
               funding_agency
      ) %>% 
      summarise(across(matches("targets|qtr"), sum, na.rm = T), .groups = "drop") %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results/targets)
  
# VIZ ============================================================================

  #OVC_SERV Achievement
    num_ovc %>% 
      filter(period == metadata$curr_pd) %>% 
      ggplot(aes(x = period)) + 
        geom_col(aes(y = targets), fill = grey20k,
                 position = position_nudge(x = -0.05), width = 0.75) + 
        geom_col(aes(y = results), fill = scooter, width = 0.75) + 
      si_style_ygrid(facet_space = 0.25) + 
      #scale_x_discrete(labels = c("FY24Q2")) + 
      scale_y_continuous(labels = comma) + 
      geom_label(aes(y = results, label = percent(achv, 1)), #achv text
                family = "Source Sans Pro",
                size = 12/.pt,
                vjust = -.5) + 
      geom_text(aes(y = results, label = comma(results)), #results text
                family = "Source Sans Pro SemiBold",
                size = 12/.pt,
                color = "black",
                vjust = 2.5) + 
      geom_text(aes(y = targets, label = comma(targets)), #targets text
                family = "Source Sans Pro SemiBold",
                size = 12/.pt,
                color = "black",
                vjust = 2.5) + 
      labs(x = NULL, y = NULL,
           title = glue("OVC_SERV ACHIEVEMENT | PEPFAR: {metadata$curr_pd}"),
           subtitle = "Gray bars are OVC_SERV targets",
           caption = glue("{metadata$caption} | {ref_id}"))
    
    si_save("Graphics/OVC_SERV_PEPFAR_FY24Q2_achievement.png")
    
    usaid_ovc %>% 
      filter(period == metadata$curr_pd) %>% 
      ggplot(aes(x = period)) + 
      geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = -0.05), width = 0.75) + 
      geom_col(aes(y = results), fill = scooter, width = 0.75) + 
      si_style_ygrid(facet_space = 0.25) + 
      #scale_x_discrete(labels = c("FY24Q2")) + 
      scale_y_continuous(labels = comma) + 
      geom_label(aes(y = results, label = percent(achv, 1)), #achv text
                 family = "Source Sans Pro",
                 size = 12/.pt,
                 vjust = -.5) + 
      geom_text(aes(y = results, label = comma(results)), #results text
                family = "Source Sans Pro SemiBold",
                size = 12/.pt,
                color = "black",
                vjust = 2.5) + 
      geom_text(aes(y = targets, label = comma(targets)), #targets text
                family = "Source Sans Pro SemiBold",
                size = 12/.pt,
                color = "black",
                vjust = 2.5) + 
      labs(x = NULL, y = NULL,
           title = glue("OVC_SERV ACHIEVEMENT | USAID: {metadata$curr_pd}"),
           subtitle = "Gray bars are OVC_SERV targets",
           caption = glue("{metadata$caption} | {ref_id}"))
    
    
    si_save("Graphics/OVC_SERV_USAID_FY24Q2_achievement.png")
    
    
    #OVC_SERV Trends 
    usaid_ovc %>% 
      filter(period %in% c("FY23Q2","FY23Q4","FY24Q2")) %>% 
      ggplot(aes(x = period)) + 
      geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = -0.05), width = 0.75) + 
      geom_col(aes(y = results), fill = scooter, width = 0.75) +
      #facet_wrap(enquo(facet_var), nrow = nrows, scales = scale_type) +
      si_style_ygrid(facet_space = 0.25) + 
      scale_x_discrete(labels = c("FY23Q2","FY23Q4","FY24Q2")) + 
      scale_y_continuous(labels = comma) + 
      geom_label( aes(y = results, label = percent(achv, 1)), #achv text
                 family = "Source Sans Pro",
                 size = 12/.pt,
                 vjust = -.05) + 
      geom_text(aes(y = results, label = comma(results)), #results text
                family = "Source Sans Pro SemiBold",
                size = 12/.pt,
                color = "black",
                vjust = 2.5) + 
      geom_text(data = .%>% filter(period != "FY23Q4"), 
                aes(y = targets, label = comma(targets)), #targets text
                family = "Source Sans Pro SemiBold",
                size = 12/.pt,
                color = "black",
                vjust = 2.5) + 
      labs(x = NULL, y = NULL,
           title = glue("OVC_SERV TRENDS | USAID: {metadata$curr_pd}"),
           subtitle = "Gray bars are OVC_SERV targets",
           caption = glue("{metadata$caption} | {ref_id}"))
    
    si_save("Graphics/OVC_SERV_USAID_FY23-FY24_trends.png")
      
    

# SPINDOWN ============================================================================

