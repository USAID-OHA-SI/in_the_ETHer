# AUTHOR:   K. Srikanth, L. Baraki| USAID
# PURPOSE:  USAID / CDC Woreda analysis
# REF ID:   f263ddae 
# LICENSE:  MIT
# DATE:     2023-12-06
# UPDATED: 
# NOTES: https://github.com/karishmas26/ethiopia-cop22/blob/main/Scripts/20221201_Q4Poart_SNU_analysis.R

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(systemfonts)
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
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY21-24.*Ethiopia")

# Grab metadata
get_metadata(file_path)

ref_id <- "f263ddae"

data_folder <- "Data/"

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   #x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# IMPORT ------------------------------------------------------------------

df <-  read_psd(file_path)

usaid_woreda <-  data_folder %>% 
  return_latest("C&T_Woreda_List_COP21") %>% 
  read_xlsx(sheet = "C&T PSNUs List") #%>% 
#rename(psnuuid = datim_uid)

# MUNGE -------------------------------------------------------------------

clinical_cdc_psnu <- df %>% 
  clean_agency() %>% 
  filter(
    funding_agency == "CDC",
    indicator %in% c("TX_CURR"),
    fiscal_year == metadata$curr_fy) %>% 
  distinct(psnuuid) %>% #grab psnuuid's from MSD
  pull()


# Distribution of Sites ---------------------------------------------------

#DISTRIBUTION OF SITES

df_sites <- df %>% 
  clean_agency() %>% 
  filter(
    funding_agency == "CDC", 
    fiscal_year == metadata$curr_fy,
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
  count(snu1, usaid_supported) %>% 
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
  #  facet_wrap(~usaid_supported) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  theme(plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, 
       title = glue("DISTRIBUTION OF <span style = 'color:{denim}'>USAID-SUPPORTED WOREDAS</span> & <span style = 'color:{scooter_light}'>NON-USAID SUPPORTED</span> WOREDAS"), #add color to the title 
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))

#si_save("Graphics/00_psnu_dist.svg")
si_save("Images/00_psnu_dist.png")

# TX CURR -----------------------------------------------------------    

df_viz <-  df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         indicator %in% c("TX_CURR"), #can sub in any indicators 
         fiscal_year == metadata$curr_fy, #pulls FY23
         psnuuid %in% clinical_cdc_psnu,
         standardizeddisaggregate == "Total Numerator"
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, #psnu, psnuuid,
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("cop22_psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))

df_viz %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total)



df_viz %>% 
  #filter(period == "FY22Q4") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  #facet_wrap(~snu1) +
  scale_y_continuous(limits = c(0, 10000)) +
  scale_color_manual(values = c(scooter_light, denim)) +
  theme(legend.position = "none", 
        plot.title = ggtext::element_markdown())+
  labs(x = NULL,
       y = "results", 
       title = glue("<span style = 'color:{denim}'>USAID-SUPPORTED WOREDAS</span> GENERALLY HAVE MORE PATIENTS ON TREATMENT THAN <span style = 'color:{scooter_light}'>NON-USAID SUPPORTED WOREDAS</span>
                    IN {metadata$curr_fy_lab}"),
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))

si_save("Images/01_txcurr_jitter.png", scale = 1.25)
#si_save("Graphics/01_txcurr_jitter.svg")



# CERVICAL CANCER -------------------------------------

df_cxca <-  df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "CXCA"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2022, 2023), #update years
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, #psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported),
            by = c("cop22_psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported),
                                 "Not USAID supported", usaid_supported))


df_cxca %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total)

df_cxca %>% 
  filter(indicator == "CXCA_SCRN",
         period %in% c("FY22Q2", "FY22Q4")) %>% 
  #filter(period == "FY22Q4") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  #facet_wrap(~snu1) +
  scale_y_continuous(limits = c(0, 1500)) +
  scale_color_manual(values = c(scooter_light, denim))


df_cxca_ag <- df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "CXCA"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2022, 2023),
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1,# psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(matches("qtr2|qtr4"), sum, na.rm = TRUE)) %>% 
  pivot_longer(cols = matches("qtr"),
               names_to = "period") %>% 
  spread(indicator, value) %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported),
            by = c("cop22_psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported),
                                 "Not USAID supported", usaid_supported)) %>% 
  group_by(fiscal_year, funding_agency, period, usaid_supported) %>% 
  summarise(across(starts_with("CXCA"), sum, na.rm = TRUE),.groups = "drop") %>%
  mutate(tx_rate = CXCA_TX / CXCA_SCRN_POS, #calculate percent on treatment from those testing positive 
         period = paste0("FY", str_sub(fiscal_year, 3, 4), "Q", str_sub(period, 4))) %>% 
  group_by(fiscal_year, funding_agency, usaid_supported) %>% 
  mutate(order_var = sum(CXCA_SCRN, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ou_order = fct_reorder(usaid_supported, order_var, .desc = T)) 


si_blue <- "#4974a5"
nudge_space  <-  0.15

a <- df_cxca_ag %>% 
  mutate(positivity = CXCA_SCRN_POS/CXCA_SCRN) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = CXCA_SCRN), fill = golden_sand_light,
           width = 0.6) + #number of HIV-positive women on ART screened for cervical cancer 
  geom_col(aes(y = CXCA_SCRN_POS), fill = golden_sand, width = 0.5, 
           position = position_nudge(x = nudge_space)) + #number of those screened that test positive 
  geom_col(aes(y = CXCA_TX), fill = si_blue, width = 0.5, 
           position = position_nudge(x = -nudge_space)) + #number of those positive on treatment 
  geom_text(aes(y = CXCA_SCRN_POS, label = percent(positivity, 1)),
            size = 12/.pt,
            family = "Source Sans Pro Semibold",
            vjust = -0.5, hjust = -0.5) +
  # geom_text(aes(y = CXCA_SCRN, label = percent(positivity, 1)),
  #           size = 12/.pt, family = "Source Sans Pro SemiBold", vjust = -0.5, hjust = -0.5) +
  si_style_xline() +
  facet_wrap(~usaid_supported) +
  geom_hline(yintercept = seq(1e4, 3.5e4, 1e4), color = "white", size = 0.5) +
  #scale_y_continuous(position = "right", labels = label_number()) +
  scale_y_continuous(position = "right", label = label_number(scale_cut = cut_short_scale())) +
  #theme(strip.text = element_blank()) +
  coord_cartesian(expand = F) +
  theme(plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL,
       title = glue("USAID-SUPPORTED WOREDAS HAD SLIGHTLY HIGHER NUMBERS OF
                    <span style = 'color:{golden_sand_light}'>CERVICAL CANCER SCREENINGS</span> AND 
                    RATES OF <span style = 'color:{golden_sand}'>POSITIVITY</span>"))

b <- df_cxca_ag %>% 
  ggplot(aes(x = period, y = tx_rate, group = usaid_supported)) +
  geom_area(aes(y = 1), fill = "#bdcee2", alpha = 0.5)+
  geom_area(fill = si_blue, alpha = 0.5)+
  geom_line(color = si_blue, size = 2) +
  # geom_textpath(aes(label = "Treatment rate"), hjust = 0.95, vjust = -1, include_line = F)+
  #geom_text(aes(label = "Treatment rate", y = 1, x = "FY21Q2"), vjust = -1,
  #         family = "Source Sans Pro",
  #        size = 12/.pt) +
  geom_hline(yintercept = 1, size = 0.25, linetype = "dotted") +
  geom_label(aes(label = percent(tx_rate, 1)), size = 12/.pt,
             family = "Source Sans Pro Semibold") +
  si_style_xline() +
  facet_wrap(~usaid_supported) +
  # coord_cartesian(expand = F) +
  theme(strip.text = element_blank()) +
  scale_x_discrete(expand = expansion(add = 0.25))+
  scale_y_continuous(expand = expansion(mult = 0), lim = c(0, 1.5)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  labs(caption = glue("{metadata$caption} | Ref ID: {ref_id}"))

cxca_set <- a / b + plot_layout(heights = c(8, 2)) +
  plot_annotation(
    #subtitle = "USAID-supported woredas have slightly lower rates of linkage to treatment for cervical cancer than non-USAID supported woredas",
    theme = si_style()
  )

#set + 
# plot_annotation(
#  title = "USAID-SUPPORTED WOREDAS HAD SLIGHTLY HIGHER NUMBERS OF <span style = 'color:{golden_sand_light}'>CERVICAL CANCER SCREENINGS</span> 
# AND RATES OF <span style = 'color:{golden_sand}'>POSITIVITY</span> THAN NON-USAID SUPPORTED WOREDAS",
#theme = theme(plot.title = ggtext::element_markdown())
#)

#si_save("Graphics/02_cxca.svg")
si_save("Images/02_cxca.png", scale = 1.25)

# TESTING --------------------------------------------------------------

df_hts <- df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "HTS"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2022, 2023), #update FY
         psnuuid %in% clinical_cdc_psnu) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, #psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported),
            by = c("cop22_psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))



df_hts %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total) 


df_hts_ag <-  df_hts %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
  pivot_wider(names_from = "indicator")

a <- df_hts_ag %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = HTS_TST), fill = "#e0d4db", width = 0.5,
           position = position_nudge(x = 0.1)) +
  geom_col(aes(y = HTS_TST_POS), fill = "#855C75", width = 0.5) +
  geom_text(aes(y = HTS_TST_POS, label = percent(HTS_TST_POS/HTS_TST, 1)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  si_style_ygrid() +
  facet_wrap(~usaid_supported) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL, y = NULL, title = "TESTING POSITIVITY TRENDS",
       caption = metadata$caption)+
  coord_cartesian(expand = F)

b <- df_hts %>% 
  filter(indicator == "HTS_TST_POS") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  # geom_smooth(aes(x = period, y = value,
  #                 weight = value, group = usaid_supported, color = usaid_supported),
  #             method = "loess",
  #             formula = "y ~ x", se = FALSE, na.rm = TRUE,
  #             size = 1.5) +
  #facet_wrap(~snu1) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_color_manual(values = c(scooter_light, denim)) +
  theme(legend.position = "none",
        plot.title = ggtext::element_markdown()) +
  labs(x = NULL,y = NULL,
       title = glue("<span style = 'color:{denim}'>USAID SUPPORTED</span> WOREDAS IDENTIFY MORE HIV POSITIVES
                    THAN <span style ='color:{scooter_light}'>NON-USAID SUPPORTED</span> WOREDAS"),
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))

#b/a
#si_save("Graphics/03_hts.svg")

b 


si_save("Images/03_hts_pos_jitter.png", scale = 1.25)


# RTT ------------------------------------------------------------------------

df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
         #count(indicator, standardizeddisaggregate) %>% view()
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1,# psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = "indicator",
              names_glue = "{tolower(indicator)}") %>% 
  group_by(snu1, #psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  mutate(tx_curr_lag1 = lag(tx_curr, 1, order_by = period)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  ungroup()


df_rtt <-  df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         str_detect(indicator, "RTT"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c(2022, 2023), #update years of interest
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, # psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("cop22_psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))

df_rtt %>% 
  filter(indicator == "TX_RTT") %>% 
  ggplot() +
  geom_point(mapping = aes(x = period, y = value, 
                           color = usaid_supported),
             alpha = .6,
             position = "jitter",
             size = 2) +
  si_style_ygrid() +
  scale_y_continuous(limits = c(0, 200)) +
  scale_color_manual(values = c(scooter_light, denim)) +
  theme(legend.position = "none",
        plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, title = glue("<span style = 'color:{denim}'>USAID-SUPPORTED WOREDAS</span> HAVE MORE ART PATIENTS RETURN TO TREATMENT THAN NON-USAID SUPPORTED
  WOREDAS IN {metadata$curr_fy_lab}"),
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))

si_save("Images/04_rtt_jitter.png", scale = 1.25)

#si_save("Graphics/04_rtt_scatter.svg")


df_rtt_agg <- df_rtt %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total) 


#rtt_viz <- function(df, snu) {

#df %>% 
#filter(snu1 == snu) %>% 
df_rtt_agg %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = total ), fill = "#e0d4db", width = 0.5,
           position = position_nudge(x = -0.1)) +
  geom_col(aes(y = `USAID Supported`), fill = "#855C75", width = 0.5) +
  geom_text(aes(y = `USAID Supported`, label = percent(usaid_share, 1)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  si_style_ygrid() +
  #  facet_wrap(~usaid_supported) +
  scale_y_continuous(limits = c(0,6000),
                     label = label_number(scale_cut = cut_short_scale())) +
  theme(plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, title = glue("<span style = 'color:#855C75'>USAID-SUPPORTED</span> WOREDAS ACCOUNT FOR
                                          OVER HALF OF THE <span style = 'color:#e0d4db'>TOTAL PATIENTS RETURNING TO TREATMENT</span>"),
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))+ 
  coord_cartesian(expand = F)

#}

#rtt_viz(df_rtt_agg, "Addis Ababa")
#rtt_viz(df_rtt_agg, "Oromia")
#rtt_viz(df_rtt_agg, "Amhara")


si_save("Images/04a_rtt_bar.png")

df_rtt_agg_2 <- df_rtt %>% 
  group_by(period, funding_agency, snu1, #facet by SNU1
           indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = "usaid_supported") %>% 
  mutate(total = `Not USAID supported` + `USAID Supported`,
         usaid_share = `USAID Supported` / total) 

df_rtt_agg_2 %>% 
  filter(snu1 %in% c("Addis Ababa", "Oromia", "Amhara"),
         period %in% c("FY23Q1", "FY23Q2"," FY23Q3", "FY23Q4")) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = `USAID Supported`), fill = denim, width = 0.5) + #USAID supported column
  geom_col(aes(y = `Not USAID supported`), fill = denim_light, width = 0.5,
           position = position_nudge(x = -0.2)) + #non-USAID supported column
  geom_text(aes(y = `USAID Supported`,
                label = clean_number(`USAID Supported`)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  geom_text(aes(y = `Not USAID supported`,
                label = clean_number(`Not USAID supported`)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = "white",
            #position = position_nudge(x = -0.2),
            vjust = -0.5) +
  facet_wrap(~snu1#,scales = "free_y"
  ) +
  scale_y_continuous(limits = c(0, 1500),
                     label = label_number(scale_cut = cut_short_scale()))+
  si_style_ygrid() +
  theme(legend.position = "none",
        plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, title = glue("<span style = 'color:{denim}'>USAID-SUPPORTED</span> WOREDAS ACCOUNT FOR OVER HALF OF PATIENTS RETURNING TO TREATMENT"),
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))+
  coord_cartesian(expand = F) 

si_save("Images/04b_rtt_snu_bar.png")


#TX_NEW ------------------------------------------------------------------------

df_txnew <-  df %>% 
  clean_agency() %>% 
  filter(funding_agency == "CDC",
         indicator %in% c("TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year %in% c(2023), #update years of interest
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  group_by(fiscal_year, funding_agency, indicator, snu1, # psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("cop22_psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported)) 


df_txnew_agg <- df_txnew %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = indicator)

df_txnew_agg %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = TX_NEW ), fill = "#1e87a5", width = 0.5, #TX_NEW
           position = position_nudge(x = -0.1), alpha = 0.75) +
  geom_col(aes(y = TX_NET_NEW), fill = "#83dbfb", width = 0.5, alpha = 0.75) + #TX_NET_NEW
  geom_text(aes(y = TX_NEW,
                label = clean_number(TX_NEW)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) + 
  geom_text(aes(y = TX_NET_NEW,
                label = clean_number(TX_NET_NEW)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  si_style_ygrid() +
  facet_wrap(~usaid_supported
             #, scale = "free_y"
  ) +
  scale_y_continuous(limits = c(0,5000),
                     label = label_number(scale_cut = cut_short_scale())) +
  theme(plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, title = glue("USAID-SUPPORTED WOREDAS HAVE ALMOST DOUBLE THE <span style = 'color:#1e87a5'>TX_NEW</span> &
                                          <span style = 'color:#83dbfb'>TX_NET_NEW</span> COMPARED TO NON-USAID SUPPORTED WOREDAS"),
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))+ #IN {snu %>% toupper()}
  coord_cartesian(expand = F)

si_save("Images/05_txnew_bar.png", scale = 1.25)


# TB ----------------------------------------------------------------------


df_tb <-  df %>% 
  gophr::clean_agency() %>% #cleans agency columns naming  
  filter(funding_agency == "CDC",
         indicator %in% c("TB_PREV"), #need to separate D from N 
         #standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year %in% c(2022,2023), #update years of interest
         psnuuid %in% clinical_cdc_psnu
  ) %>% 
  gophr::clean_indicator() %>% #applies denominator suffic
  group_by(fiscal_year, funding_agency, indicator, snu1, # psnu, psnuuid
           cop22_psnu, cop22_psnuuid) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>%
  reshape_msd() %>% 
  left_join(usaid_woreda %>% select(datim_uid, usaid_supported), by = c("cop22_psnuuid" = "datim_uid")) %>% 
  mutate(usaid_supported= ifelse(is.na(usaid_supported), "Not USAID supported", usaid_supported))

df_tb_agg <- df_tb %>% 
  group_by(period, funding_agency, indicator, usaid_supported) %>% 
  summarise(across(starts_with("value"), sum, na.rm = TRUE),.groups = "drop") %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(
    share = TB_PREV / TB_PREV_D)

df_tb_agg %>%
  filter(period %in% c("FY22Q2", "FY22Q4","FY23Q2", "FY23Q4")) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = TB_PREV_D ), fill = "#8980cb", width = 0.5, #initiation 
           position = position_nudge(x = 0.1)) +
  geom_col(aes(y = TB_PREV), fill = "#f2bc40", width = 0.5) +  #completion
  geom_text(aes(y = TB_PREV,
                label = percent(TB_PREV/TB_PREV_D,1)
                #label = clean_number(TB_PREV_D)
  ),
  size = 11/.pt, 
  family = "Source Sans Pro", 
  color = grey90k,
  vjust = -0.5) + 
  #geom_text(aes(y = TB_PREV,
  #             label = clean_number(TB_PREV)),
  #        size = 11/.pt, 
  #       family = "Source Sans Pro", 
  #      color = grey90k,
  #     vjust = -0.5) +
  si_style_ygrid() +
  facet_wrap(~usaid_supported) +
  scale_y_continuous(limits = c(0,30000),
                     label = label_number(scale_cut = cut_short_scale())
  ) +
  theme(plot.title = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, title = glue("USAID-SUPPORTED WOREDAS HAVE HIGHER <span style = 'color:#8980cb'>TPT INITIATION </span> &
                                          <span style = 'color:#f2bc40'>COMPLETION</span> RATES THAN NON-USAID SUPPORTED WOREDAS"),
       caption = glue("{metadata$caption} | Ref ID: {ref_id}"))+ 
  coord_cartesian(expand = F)

si_save("Images/06_tb_bar.png", scale = 1.25)
