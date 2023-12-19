# PURPOSE: Munge and Analysis of conflict SNU's 
# AUTHOR: Lemlem Baraki | SI
# REF ID:   ace3e9c9
# LICENSE: MIT
# DATE: 2023-12-06
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(tidyverse)
library(scales)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)
library(readxl)
library(googlesheets4)
library(glue)
library(gt)
library(gtExtras)


# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY21-24.*Ethiopia")

#site_path <- return_latest(folderpath = merdata, pattern = "Site_IM_FY15-20.*Ethiopia")
site_path <- return_latest(folderpath = merdata, pattern = "Site_IM_FY21-24.*Ethiopia")



# Grab metadata
  get_metadata(file_path)
  #get_metadata(site_path)

# REF ID for plots
ref_id <- "ace3e9c9"

# Functions  
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}  


clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


plot_tx_trends <- function(.data, facet_var = mech_name, nrows = 1, scale_type = "fixed"){
  .data %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = -0.15), width = 0.75) +
    geom_col(aes(y = results_cumulative), fill = scooter, width = 0.75) +
    facet_wrap(enquo(facet_var), nrow = nrows, scales = scale_type) +
    si_style_ygrid(facet_space = 0.25) +
    scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                "FY22Q1", "", "", "",
                                "FY23Q1", "","","")) + #update quarters
    scale_y_continuous(labels = comma, expand = c(0, 0)) +
    geom_text(data = . %>% filter(qtr_flag == 1 | period == "FY22Q4"), 
              aes(y = results_cumulative, label = percent(achv, 1)),
              family = "Source Sans Pro",
              size = 11/.pt, 
              vjust = -.5)
}

# Shrink size of rows in GT tables  
shrink_rows <- function(gt_obj){
  gt_obj %>% 
    tab_options(
      data_row.padding = px(1),
      row_group.padding = px(2),
      heading.padding = px(1)
    ) 
}  

# LOAD DATA ============================================================================  

df <-  read_psd(file_path) %>% 
  clean_agency()

df_site <- read_psd(site_path)
#df_site <- read_psd(df_site_atrb)

data_folder <- "Data/"

usaid_woreda <-  data_folder %>% 
  return_latest("C&T_Woreda_List_COP21") %>% 
  read_xlsx(sheet = "C&T PSNUs List")

# MUNGE ============================================================================

#Woreda list
clinical_cdc_psnu <- df %>% 
  clean_agency() %>% 
  filter(
    funding_agency == "CDC",
    indicator %in% c("TX_CURR"),
    fiscal_year == metadata$curr_fy) %>% 
  distinct(psnuuid) %>% #grab psnuuid's from MSD
  pull()

# VIZ ============================================================================


# Site Distribution  ------------------------------------------------------


df_sites <- df %>% 
  clean_agency() %>% 
  filter(
    funding_agency == "CDC", 
    fiscal_year %in% c(2021,2022,2023),
    psnuuid %in% clinical_cdc_psnu
  ) %>% 
  select(fiscal_year, funding_agency, snu1, #psnu, psnuuid,
         cop22_psnu, cop22_psnuuid) %>% 
  left_join(usaid_woreda %>%
              select(datim_uid, usaid_supported),
            by = c("cop22_psnuuid" = "datim_uid")) %>% #sub in datim_uid's for CDC
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported)) %>% 
  distinct() 

df_site_viz <- df_sites %>% 
  count(snu1, fiscal_year, usaid_supported) %>% 
  pivot_wider(names_from = "usaid_supported", values_from = "n") %>% 
  mutate(`USAID Supported` = ifelse(is.na(`USAID Supported`), 0, `USAID Supported`)) %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`, #calculate total 
         usaid_share = `USAID Supported` / total) %>%  #calculate USAID share 
  mutate(usaid_lab = clean_number(`USAID Supported`),
         total_lab = clean_number(total))


df_site_viz %>% 
  ggplot(aes(x = fct_reorder(snu1, total))) +
  geom_col(aes(y = total),alpha = 0.5, fill = scooter_light, width = 0.5,
           position = position_nudge(x = 0)) +
  geom_col(aes(y = `USAID Supported`), fill = denim, width = 0.5) +
  geom_text(aes(y = `USAID Supported`, label = glue("{usaid_lab}/{total_lab}"),
                # size = 11/.pt, 
                family = "Source Sans Pro", 
                color = grey90k,
                hjust = -0.2,
                vjust = 0.5)) +
  coord_flip() +
  si_style_xgrid() +
  scale_color_identity()+
  scale_fill_identity() +
  facet_wrap(~fiscal_year) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  theme(plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, 
       title = glue("DISTRIBUTION OF <span style = 'color:{denim}'>USAID-SUPPORTED</span> & 
                        <span style = 'color:{scooter_light}'>NON-USAID SUPPORTED</span> WOREDAS"), #add color to the title 
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))

si_save("Images/tigray_psnu_dist.png")



# TX_CURR (3 SNU's) -----------------------------------------------------------------

#Trends by SNU

tx_curr_snu <- df %>% 
  filter(#funding_agency == "CDC", #specify agency
    indicator == "TX_CURR", 
    standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
  group_by(snu1, fiscal_year, indicator) %>% 
  summarise(across(matches("targets|qtr"), sum, na.rm = T)) %>% 
  ungroup() %>% 
  reshape_msd(direction = "quarters") %>% 
  mutate(achv = results_cumulative / targets,
         qtr_flag = ifelse(period %in% c(metadata$curr_pd), 1, 0),
         snu1_order = fct_reorder2(snu1, results, period, .desc = T)
  )

tx_curr_snu %>% #arrange(desc(period))
  filter(snu1 %in% c("Tigray", "Amhara", "Oromia")) %>%
  mutate(snu1 = fct_reorder2(snu1, targets, results, .desc = T)) %>%
  group_by(snu1) %>%
  mutate(
    tx_curr_trend = case_when(
      results < lag(results) ~ golden_sand,
      TRUE ~ NA_character_
    ),
    tx_curr_diff = results - lag(results)
  ) %>%
  ungroup() %>%
  complete(snu1, period) %>% 
  plot_tx_trends(., facet_var = snu1,
                 nrows = 3,
                 scale_type = "free") +
  geom_col(
    data = . %>% filter(period == max(period)),
    aes(y = results, fill = tx_curr_trend), width = 0.75
  ) +
  scale_fill_identity() +
  geom_label(
    data = . %>% filter(period == max(period)),
    aes(y = results, label = comma(tx_curr_diff)),
    vjust = 1.2,
    family = "Source Sans Pro",
    size = 10 / .pt,
    color = grey90k
  ) +
  labs(
    x = NULL, y = NULL, title = glue("TX_CURR TRENDS BY PROVINCE AS OF {metadata$curr_pd}"),
    subtitle = "Gray bars are TX_CURR targets",
    caption = metadata$caption
  ) +
  coord_cartesian(clip = "off")

si_save(glue("Images/{metadata$curr_pd}_TX_CURR_SNU1_trends.png"),
        scale = 1.25)




# IIT (3 SNU's) -----------------------------------------------------------

df_iit_snu1 <- df_site %>% 
  filter(#funding_agency == "USAID", #specify agency 
    indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
    standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                    "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
  group_by(fiscal_year, snu1, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = "indicator",
              names_glue = "{tolower(indicator)}") %>% 
  group_by(snu1) %>% 
  mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
  ungroup()%>% 
  rowwise() %>% 
  mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(period != min(period), 
         str_detect(snu1, "Tigray|Amhara|Oromia", negate = F))

df_iit_spark <- 
  df_iit_snu1 %>% 
  select(period, snu1, iit) %>% 
  arrange(snu1, period) %>% 
  group_by(snu1) %>% 
  summarize(spark_iit = list(iit), .groups = "drop") 

df_iit_snu1 %>% 
  select(period, snu1, iit) %>%
  spread(period, iit) %>% 
  left_join(., df_iit_spark) %>% 
  gt() %>%
  gt_plt_sparkline(spark_iit, 
                   same_limit = , type = "shaded", 
                   fig_dim = c(10, 30),
                   palette = c(grey70k, grey90k, old_rose_light, scooter_med, grey10k),
                   label = F) %>% 
  fmt_percent(columns = where(is.numeric)) %>% 
  cols_label(snu1 = "",
             spark_iit = "",FY21Q2 = "Q2",FY21Q4 = "Q4",
             FY22Q2 = "Q2",FY22Q3 = "Q3",FY22Q4 = "Q4",
             FY23Q1 = "Q1",FY23Q2 = "Q2",FY23Q3 = "Q3",FY23Q4 = "Q4"
  ) %>% 
  tab_header(title = glue("INTERRUPTION IN TREATMENT SUMMARY BY PROVINCE"),) %>% 
  tab_spanner(label = "FY21",columns = 2:3) %>% 
  tab_spanner(label = "FY22",columns = 6:7) %>% 
  tab_spanner(label = "FY23",columns = 10:11) %>% 
  tab_source_note(source_note = gt::md(glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW\n
                        Source: {metadata$source}"))) %>% 
  tab_options(source_notes.font.size = px(10),data_row.padding = px(1),) %>% 
  shrink_rows() %>% 
  gt_theme_nytimes() %>% 
  gtsave_extra("Images/USAID_iit_province.png")




# TX_NEW (3 SNU's) --------------------------------------------------------


df_txnew <-  df %>% 
  filter(#funding_agency == "CDC",
    indicator %in% c("TX_NEW", "TX_NET_NEW"),
    standardizeddisaggregate == "Age/Sex/HIVStatus"
  ) %>% 
  group_by(fiscal_year, indicator, snu1) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
            .groups = "drop") %>%
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(
    names_from = indicator,
    names_glue = "{tolower(indicator)}") %>% 
  filter(period != min(period), 
         str_detect(snu1, "Tigray|Amhara|Oromia", negate = F))

df_txnew %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = tx_new), fill = "#1e87a5", width = 0.75,
           position = position_nudge(x = 0.1), alpha = 0.75) +
  geom_col(aes(y = tx_net_new), fill = "#83dbfb", width = 0.75, alpha = 0.75) +
  #geom_text(aes(y = tx_new,
  #             label = comma(tx_new)),
  #        size = 11/.pt, 
  #       family = "Source Sans Pro", 
  #      color = grey90k,
  #     vjust = -0.5) + 
  geom_text(aes(y = tx_net_new,
                label = comma(tx_net_new)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  facet_wrap(~snu1,
             nrow = 3,
             scales = "free_y")+
  si_style_ygrid()+
  scale_x_discrete(
    breaks = every_nth(n = 2) #adds space btwn breaks 
    #labels = pd_brks
  ) +
  scale_y_continuous(labels = label_number_si())+
  coord_cartesian(expand = T)+
  theme(plot.title = ggtext::element_markdown()) +
  labs(x = NULL, y = NULL, 
       title = glue("<span style = 'color:#1e87a5'>TX_NEW</span> 
                        & <span style = 'color:#83dbfb'>TX_NET_NEW</span> 
                        TRENDS BY PROVINCE AS OF {metadata$curr_pd}"),
       caption = metadata$caption) 

si_save(glue("Images/{metadata$curr_pd}_TX_NEW_SNU1_trends.png"),
        scale = 1.25)

# HTS (3 SNU's) -----------------------------------------------------------


df_hts <-  df %>% 
  filter(#funding_agency == "CDC",
    indicator %in% c("HTS_TST", "HTS_TST_POS"),
    standardizeddisaggregate %in% c("Total Numerator")) %>% 
  gophr::clean_indicator() %>% 
  group_by(fiscal_year, indicator, snu1) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
            .groups = "drop") %>%
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(
    names_from = indicator,
    names_glue = "{tolower(indicator)}") %>% 
  filter(#period != min(period), 
    str_detect(snu1, "Tigray|Amhara|Oromia", negate = F))

df_hts %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = hts_tst), fill = "#e0d4db", width = 0.75,
           position = position_nudge(x = 0.1)) +
  geom_col(aes(y = hts_tst_pos), fill = "#855C75", width = 0.75) +
  #geom_text(aes(y = hts_tst,
  #             label = comma(hts_tst)),
  #        size = 11/.pt, 
  #       family = "Source Sans Pro", 
  #      color = grey90k,
  #     vjust = -0.25) +
  geom_text(aes(y = hts_tst_pos,
                label = percent(hts_tst_pos/hts_tst,1)), #positivity
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.25) +
  si_style_ygrid() +
  facet_wrap(~snu1, 
             nrow = 3,
             scales = "free_y")+
  scale_y_continuous(labels = label_number_si(),
                     expand = c(.15,.15))+
  scale_x_discrete(
    breaks = every_nth(n = 2) #adds space btwn breaks 
    #labels = pd_brks
  ) +
  coord_cartesian(expand = T)+
  theme(plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, 
       title = glue("<span style = 'color:#e0d4db'>HTS_TST_POS</span> &
           <span style = 'color:#855C75'>HTS_TST_POS</span> TRENDS BY PROVINCE
                        AS OF {metadata$curr_pd} ", 
                    caption = metadata$caption))

si_save(glue("Images/{metadata$curr_pd}_HTS_SNU1_trends.png"),
        scale = 1.25)

si_save(glue("Images/{metadata$curr_pd}_HTSPOS_SNU1_trends.png"),
        scale = 1.25)

