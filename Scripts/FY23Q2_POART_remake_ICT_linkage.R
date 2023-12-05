# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q2 POART viz remake
# REF ID:   f3760bce 
# LICENSE:  MIT
# DATE:     2023-06-21
# UPDATED:  2023-12-05 (needs to be updated for q4 analytics)

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata() 
  
  ref_id <- "f3760bce"
  
  data_folder <- "Data/"

  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  
# IMPORT ------------------------------------------------------------------

  #data are extracted from the excel visuals that the team sends
    # extracted and added to a new sheet in a tidy format - NOT an ideal workflow as of now
  
  df_linkage <- data_folder %>% 
    return_latest("FY23Q2_linkage_ICT_data") %>% 
    read_excel(sheet = 1) %>% 
    janitor::clean_names()
  
  df_trace <- data_folder %>% 
    return_latest("FY23Q2_linkage_ICT_data") %>% 
    read_excel(sheet = 2) %>% 
    janitor::clean_names()

# LINKAGE PLOT -------------------------------------------------------------------
  
  #either export these as two svgs an add them together in AI or use v2 / v1
  
  nudge_space  <-  0.25
  
 v1 <- df_linkage %>% 
    mutate(linkage_in_percent = linkage_in_percent/100) %>% 
    rename(period = quarter) %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = number_positive_identified), fill = "#002A6C",
             width = 0.6) +
    geom_col(aes(y = number_positive_linked), fill = "#BA0C2F", width = 0.5, 
             position = position_nudge(x = nudge_space)) + 
    facet_wrap(~fct_reorder(prime_li_ps, number_positive_identified, sum, na.rm = TRUE,.desc = TRUE),
               nrow = 1) +
    si_style_ygrid() +
    geom_text(aes(y = number_positive_identified,
                  label = number_positive_identified), size = 12/.pt, hjust = 0.5,
              vjust = -1,
              family = "Source Sans Pro Light") +
    geom_text(aes(y = number_positive_linked,
                  label = number_positive_linked), size = 12/.pt, hjust = 0, vjust= -0.5,
              position = position_nudge(x = nudge_space),
              family = "Source Sans Pro Light") + 
   labs(x = NULL, y = NULL)

  
 # si_save(glue("Graphics/01_bar_{metadata$curr_pd}.svg"))
  
 v2 <- df_linkage %>% 
    mutate(linkage_in_percent = linkage_in_percent/100) %>% 
    rename(period = quarter) %>% 
    ggplot(aes(period, linkage_in_percent, group = prime_li_ps,
               color = "#004964", fill = "#004964")) +
    geom_blank(aes(y = 1.1 * linkage_in_percent)) +
    geom_line(size = 1.5) +
    geom_point(shape = 21, size = 10, stroke = 2) +
    facet_wrap(~fct_reorder(prime_li_ps, number_positive_identified, sum, na.rm = TRUE,.desc = TRUE),
               nrow = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent, limits = c(.9, 1)) +
    geom_text(aes(label = percent(linkage_in_percent)), color = "white",
              family = "Source Sans Pro",
              size = 12/.pt) +
    expand_limits(y = .2) +
    si_style_ygrid() +
   labs(x = NULL, y = NULL,
        title = "Contact tracing and ICT has resulted in a 98.2% linkage rate across LIPs" %>% toupper(),
        # subtitle = "subtitle",
        caption = "Source: ")
 
 v2 / v1
  
  si_save(glue("Graphics/01_linkage_pct_{metadata$curr_pd}.svg"))
  
  # TRACING RTT PLOT -------------------------------------------------------------------
  
  nudge_space  <-  0.25
  
  v3 <- df_trace %>% 
    filter(!str_detect(quarter, "FY21")) %>% 
    mutate(percent_reengagment_from_traced_located = percent_reengagment_from_traced_located/100) %>% 
    rename(period = quarter,
           pct_rtt = percent_reengagment_from_traced_located) %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = line_list_received), fill = trolley_grey_light,
             width = 0.6) +
    geom_col(aes(y = traced_located), fill = "#BA0C2F", width = 0.5, 
             position = position_nudge(x = nudge_space)) + 
    geom_col(aes(y = re_engaged), fill = "#002A6C", width = 0.5, 
             position = position_nudge(x = nudge_space*2)) + 
    si_style_ygrid() +
    geom_text(aes(y = line_list_received,
                  label = line_list_received), size = 12/.pt, hjust = 0.4,
              vjust = -0.5, 
              family = "Source Sans Pro Light") +
    geom_text(aes(y = traced_located,
                  label = traced_located), size = 12/.pt, hjust = 0.4,
              vjust = -0.5,
              position = position_nudge(x = nudge_space),
              family = "Source Sans Pro Light") +
    geom_text(aes(y = re_engaged,
                  label = re_engaged), size = 12/.pt, hjust = 0,
              vjust = -0.5,
              position = position_nudge(x = nudge_space*2),
              family = "Source Sans Pro Light") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    labs(x = NULL, y = NULL)
  
  #si_save(glue("Graphics/02_bar_{metadata$curr_pd}.svg"))

  v4 <- df_trace %>% 
    filter(!str_detect(quarter, "FY21")) %>% 
    mutate(percent_reengagment_from_traced_located = percent_reengagment_from_traced_located/100,
           pct_rtt_line_list = re_engaged / line_list_received) %>% 
    rename(period = quarter,
           pct_rtt = percent_reengagment_from_traced_located) %>% 
    ggplot(aes(period)) +
    geom_blank(aes(y = 1.1 * pct_rtt)) +
    geom_line(aes(y = pct_rtt, group = NA,
                  color = "#002A6C"),size = 1.5) +
    geom_point(aes(y = pct_rtt,
                   color = "#002A6C", fill = "#002A6C"), shape = 21, size = 10, stroke = 2) +
    geom_text(aes(y = pct_rtt,
                  label = percent(pct_rtt)), color = "white",
              family = "Source Sans Pro",
              size = 12/.pt) +
    geom_line(aes(y = pct_rtt_line_list, group = NA,
                  color = "#A7C6ED"),size = 1.5) +
    geom_point(aes(y = pct_rtt_line_list,
                   color = "#A7C6ED", fill = "#A7C6ED"), shape = 21, size = 10, stroke = 2) +
    geom_text(aes(y = pct_rtt_line_list,
                  label = percent(pct_rtt_line_list,1)),
              family = "Source Sans Pro",
              size = 12/.pt) +
    scale_y_continuous(labels = percent, limits = c(.5, 1)) +
    scale_fill_identity() +
    scale_color_identity() +
    expand_limits(y = .2) +
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = "Community tracing of clients has improved from FY22 to FY23Q2, with more clients re-engaging into care" %>% toupper(),
         # subtitle = "subtitle",
         caption = "Source: ")

    
  v4 / v3
  
  si_save(glue("Graphics/02_traced_rtt_{metadata$curr_pd}.svg"))
  
    
