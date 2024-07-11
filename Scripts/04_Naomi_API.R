# PROJECT: NAOMI API ACCESS
# PURPOSE: Munge and Analysis of Botswana District Level Estimates
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  b26df490
# LICENSE: MIT
# DATE:   2024-07-08
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
  #library(mindthegap)
  library(tidyverse)
  library(httr)
    
  
  # REF ID for plots
    ref_id <- "b26df490"
    
  # Functions  
    get_naomi <- function(iso, indicator, age_band = age, pd = period, gend = sex, level = area_level) {
      
      #create URL
      url2 <- paste0(baseurl,
                     "country=", iso,
                     "&indicator=", indicator,
                     "&ageGroup=", age_band,
                     "&period=",pd,
                     "&sex=", gend,
                     "&areaLevel=", area_level)
      #hit url
      df <- httr::GET(url = url2)
      
      #extract context
      response_content <- df %>%
        httr::content("text")
      
      df_final <- response_content %>%
        read_csv() %>%
        mutate(
          # age_band = age,
          #      sex = sex,
          indicator = indicator,
          period = period,
          iso = iso,
          level = as.numeric(level),
          mean = as.numeric(mean),
          lower = as.numeric(lower),
          upper = as.numeric(upper))
      
      return(df_final)
      
    }

# LOAD DATA ============================================================================  

    glamr::pepfar_country_list %>%
      distinct(country, country_iso) %>% 
      filter(country %in% c("Botswana","Eswatini")) #check iso for your OUs 
    
    
    #grab the base url
    #https://naomi2023.azurewebsites.net/api/v1/data?country=MOZ&indicator=incidence&ageGroup=Y015_049&period=2022-4&sex=both&areaLevel=1
    #https://naomi-spectrum.unaids.org/
    
    baseurl <- "https://naomi2023.azurewebsites.net/api/v1/data?"
    
    iso <- "BWA"
    indic <- "incidence" #HIV incidence per 1000
    age <- "Y015_049" 
    period <- "2022-4" #don't change (q4 is December)
    sex <- "both"
    area_level <- "2" #district/psnu is equivalent to 2 (admin district:1 vs new health district:2) --> gives you all 
    
    url2 <- paste0(baseurl,
                   "country=", iso,
                   "&indicator=", indic,
                   "&ageGroup=", age,
                   "&period=",period,
                   "&sex=", sex,
                   "&areaLevel=", area_level)
    
    df_incid <- httr::GET(url = url2)
    

# MUNGE ============================================================================
  
  #read in API and convert to comma separated string
    response_content <- df_incid %>%
      httr::content("text") 
    
    response_content %>%
      read_csv() %>%
      filter(level == 2) %>% #desired geographic level 
      mutate(age_band = age,
             sex = sex,
             indicator = indic,
             period = period,
             iso = iso) %>% View() #values x1000 for interpretation 
    
  
# VIZ ============================================================================

  #Use Function
    df_final <- get_naomi(iso = "BWA", indicator = "incidence") %>% 
      filter(level %in% c(0,2))
    
    df_final %>%
      write_csv("Dataout/Botswana_naomi-testing-2022.csv")

# SPINDOWN ============================================================================

    #Iteration 
      #get country list 
      pepfar_country_df <- glamr::pepfar_country_list %>%
        distinct(country, country_iso) 
      
      pepfar_iso_list <- pepfar_country_df %>%
        pull(country_iso)
    
    
      map_naomi <- function(indic, sex_param, age_param) {
        
        #indic <- "unaware_plhiv_num"
        # indic <- 'aware_plhiv_prop'
        
        # indic <- indicator
        #
        age <- age_param # 15+
        # period <- "2022-4" #dont change
        sex <- sex_param
        # area_level <- "3"
        
        df_all <- purrr::map_dfr(.x = pepfar_iso_list,
                                 .f = ~ get_naomi(iso = .x, gend = sex, age = age, indicator = indic)
        )
        
        df_all <-  df_all %>%
          mutate(sex = sex_param,
                 age = age_param)
        
        return(df_all)
      }
    
      df_male_adult <- map_naomi(indic ="aware_plhiv_prop", sex_param = "male", age_param = "Y015_999") #indicator not recognized
      df_female_adult <- map_naomi(indic ="unaware_plhiv_num", sex_param = "female", age_param = "Y015_999") #indicator not recognized
      
      
      age_list <- c("Y015_019", "Y015_019", "Y015_019",
                    "Y020_024", "Y020_024", "Y020_024",
                    "Y015_999", "Y015_999", "Y015_999")
      
      gend_list <- c("male", "female", "both",
                     "male", "female", "both",
                     "male", "female", "both")
      
      df_final_num <- map2_dfr(.x = gend_list, .y = age_list,
                               .f = ~map_naomi(indic ="unaware_plhiv_num",
                                               sex_param = .x, age_param = .y))
      
      df_final_prop <- map2_dfr(.x = gend_list, .y = age_list,
                                .f = ~map_naomi(indic ="aware_plhiv_prop",
                                                sex_param = .x, age_param = .y))
      
      df_final_pOP <- map2_dfr(.x = gend_list, .y = age_list,
                               .f = ~map_naomi(indic ="population",
                                               sex_param = .x, age_param = .y))
    
    
      df_final<- rbind(df_final_num, df_final_pOP, df_final_prop) %>%
        mutate(age = str_remove(age, "Y0")) %>%
        mutate(age = str_replace(age, "_0", "-")) %>%
        mutate(age = case_when(age == "15_999" ~ "15+",
                               TRUE ~ age)) %>%
        mutate(period = case_when(period == "2022-4" ~ "Dec 2022",
                                  TRUE ~ period)) %>%
        mutate(operatingunit = ifelse(level == 0, area, NA)) %>%
        fill(operatingunit)
      
      df_final_num <- df_final_num %>%
        mutate(age = str_remove(age, "Y0")) %>%
        mutate(age = str_replace(age, "_0", "-")) %>%
        mutate(age = case_when(age == "15_999" ~ "15+",
                               TRUE ~ age)) %>%
        mutate(period = case_when(period == "2022-4" ~ "Dec 2022",
                                  TRUE ~ period)) %>%
        mutate(operatingunit = ifelse(level == 0, area, NA)) %>%
        fill(operatingunit)
      
      df_final_prop <- df_final_prop %>%
        mutate(age = str_remove(age, "Y0")) %>%
        mutate(age = str_replace(age, "_0", "-")) %>%
        mutate(age = case_when(age == "15_999" ~ "15+",
                               TRUE ~ age)) %>%
        mutate(period = case_when(period == "2022-4" ~ "Dec 2022",
                                  TRUE ~ period)) %>%
        mutate(operatingunit = ifelse(level == 0, area, NA)) %>%
        fill(operatingunit)
      
      df_final_pop <- df_final_pOP %>%
        mutate(age = str_remove(age, "Y0")) %>%
        mutate(age = str_replace(age, "_0", "-")) %>%
        mutate(age = case_when(age == "15_999" ~ "15+",
                               TRUE ~ age)) %>%
        mutate(period = case_when(period == "2022-4" ~ "Dec 2022",
                                  TRUE ~ period)) %>%
        mutate(operatingunit = ifelse(level == 0, area, NA)) %>%
        fill(operatingunit)
      
      df_final %>%
        write_csv("naomi-testing-2022.csv")