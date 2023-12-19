library(tidyverse)
library(gagglr)
library(janitor)
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

files <- list.files(path="Data") |> print()

# LINKAGE PLOT -------------------------------------------------------------------


contact_tracing <- read_csv(file = str_c("Data/",files[2])) |> clean_names() |> 
  mutate(prime_li_ps2 = str_extract( prime_li_ps, ".+(?=\\s\\()"),
         prime_li_ps1 = str_extract( prime_li_ps, "(?<=\\s\\().+(?=\\))|(?<=\\s\\().+"),
         period = "FY23") |>
  select(-linkage_in_percent) |> 
  mutate(linkage_in_percent = number_positive_linked/number_positive_identified) |> print()

nudge_space  <-  0.25


contact_tracing |> group_by() |> 
  summarise(number_positive_linked = sum(number_positive_linked),
            number_positive_identified = sum(number_positive_identified), .groups = "drop") |> 
  mutate(linkage_in_percent=number_positive_linked/number_positive_identified)

v2 <- contact_tracing |>
  # mutate(linkage_in_percent = linkage_in_percent/100) %>% 
  # rename(period = quarter) %>% 
  ggplot(aes("", linkage_in_percent,
             color = "#004964", fill = "#004964")) +
  geom_blank(aes(y = 1.1 * linkage_in_percent)) +
  geom_point(shape = 21, size = 20, stroke = 2) +
  facet_wrap(~fct_reorder(prime_li_ps2, number_positive_identified, sum, na.rm = TRUE,.desc = TRUE),
             nrow = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(label = scales::percent(linkage_in_percent, accuracy = 1)), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  scale_y_continuous(labels = percent, limits = c(.967, 1.008)) +
  expand_limits(y = .2) +
  si_style_void() +
  theme(strip.text = element_text(hjust=0.5, size=20)) +
  labs(x = NULL, y = NULL,
       # title = "Contact tracing and ICT has resulted in a 98.7% linkage rate across LIPs" %>% toupper(),
       # subtitle = "subtitle",
       # caption = "Source: "
       )

v2
ggsave(plot=v2, filename = paste0("Graphics/01_linkage_pct_top.png"), width = 11.5, height = 2)


v1 <- contact_tracing %>% 
  # mutate(linkage_in_percent = linkage_in_percent/100) %>% 
  # rename(period = quarter) %>% 
  ggplot(aes(x = "")) +
  geom_col(aes(y = number_positive_identified), fill = "#83dbfb",
           width = 0.6) +
  geom_col(aes(y = number_positive_linked), fill = "#1e87a5", width = 0.5, 
           position = position_nudge(x = nudge_space)) + 
  facet_wrap(~fct_reorder(prime_li_ps1, number_positive_identified, sum, na.rm = TRUE,.desc = TRUE),
             nrow = 1) +
  si_style_ygrid() +
  scale_y_continuous(limits = c(0, 1000), breaks = c(0, 250, 500, 750)) +
  geom_text(aes(y = number_positive_identified,
                label = number_positive_identified), size = 12/.pt, hjust = 0.5,
            vjust = -1,
            family = "Source Sans Pro Light"
            ) +
  geom_text(aes(y = number_positive_linked,
                label = number_positive_linked), size = 12/.pt, hjust = 0, vjust= -0.5,
            position = position_nudge(x = nudge_space),
            family = "Source Sans Pro Light"
            ) + 
  labs(x = NULL, y = NULL) + 
  # theme(strip.text = element_blank())
  theme(strip.text = element_text(hjust=0.5, size=14)) 
  

v1

ggsave(plot=v1, filename = paste0("Graphics/01_linkage_pct_bottom.png"), width = 11.5, height = 3)



# TRACING RTT PLOT -------------------------------------------------------------------
reasons <- read_csv(file = str_c("Data/",files[5]), ) |> clean_names() 

iit_by_q <- read_csv(file = str_c("Data/",files[3]), ) |> clean_names() |> 
  rename(period = fyq,
         ) |> 
    mutate(pct_rtt = re_engaged / traced_located,
          pct_rtt_line_list = re_engaged / line_list_received) |> 
  glimpse()
  
  # mutate(prime_li_ps2 = str_extract( prime_li_ps, ".+(?=\\s\\()"),
  #        prime_li_ps1 = str_extract( prime_li_ps, "(?<=\\s\\().+(?=\\))|(?<=\\s\\().+"),
  #        period = "FY23") |>
  # select(-linkage_in_percent) |> 
  # mutate(linkage_in_percent = number_positive_linked/number_positive_identified) |> print()

nudge_space  <-  0.25

v3 <- 
  
iit_by_q |>  
  ggplot(aes(x = period)) +
  geom_col(aes(y = line_list_received), fill = trolley_grey_light,
           width = 0.4) +
  geom_col(aes(y = traced_located), fill = "#7ecfc0", width = 0.4, 
           position = position_nudge(x = nudge_space)) + 
  geom_col(aes(y = re_engaged), fill = "#287c6f", width = 0.4, 
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
            position = position_nudge(x = nudge_space*1.7),
            family = "Source Sans Pro Light") +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale()), limits = c(0, 2600)) +
  labs(x = NULL, y = NULL)

v3
  
ggsave(plot=v3, filename = paste0("Graphics/01_rtt_top.png"), width = 11.5, height = 3)


v4 <- iit_by_q |>  ggplot(aes(period)) +
  geom_blank(aes(y = 1.1 * pct_rtt)) +
  geom_line(aes(y = pct_rtt, group = NA,
                color = "#287c6f"),size = 1.5) +
  geom_point(aes(y = pct_rtt,
                 color = "#287c6f", fill = "#287c6f"), shape = 21, size = 10, stroke = 2) +
  geom_text(aes(y = pct_rtt,
                label = percent_reengagment_from_traced_located), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  geom_line(aes(y = pct_rtt_line_list, group = NA,
                color = "#f2bc40"),size = 1.5) +
  geom_point(aes(y = pct_rtt_line_list,
                 color = "#f2bc40", fill = "#f2bc40"), shape = 21, size = 10, stroke = 2) +
  geom_text(aes(y = pct_rtt_line_list,
                label = percent_reengagment_from_line_list_received),
            family = "Source Sans Pro",
            size = 12/.pt) +
  scale_y_continuous(labels = percent, limits = c(.5, 1)) +
  scale_fill_identity() +
  scale_color_identity() +
  expand_limits(y = .2) +
  si_style_void() +
  labs(x = NULL, y = NULL,
       # title = "Community tracing of clients has improved from FY22 to FY23Q2, with more clients re-engaging into care" %>% toupper(),
       # subtitle = "subtitle",
       # caption = "Source: "
       )


ggsave(plot=v4, filename = paste0("Graphics/01_rtt_bottom.png"), width = 11.5, height = 2.5)




# Cervical Cancer PLOT -------------------------------------------------------------------
cc <- read_csv(file = str_c("Data/",files[1]), ) |> clean_names() |> 
  rename(period = fyq,
  ) |> 
  mutate(pct_rx = the_number_of_women_who_are_on_treatment / number_of_wlhiv_screened_positive_for_cervical_cancer ,
         pct_via = number_of_wlhiv_screened_positive_for_cervical_cancer/ number_of_wlhiv_screened_for_cervical_cancer) |> glimpse()


cc_results <- cc |> ggplot(aes(x = period)) +
  # geom_col(aes(y = number_of_wlhiv_screened_for_cervical_cancer), fill = trolley_grey_light,
  #          width = 0.4) +
  geom_col(aes(y = number_of_wlhiv_screened_positive_for_cervical_cancer ), fill = "#bfddff", width = 0.4, 
           position = position_nudge(x = -nudge_space*0.5)) + 
  geom_col(aes(y = the_number_of_women_who_are_on_treatment), fill = "#2057a7", width = 0.4, 
           position = position_nudge(x = nudge_space*0.5)) + 
  si_style_ygrid() +
  # geom_text(aes(y = number_of_wlhiv_screened_positive_for_cervical_cancer,
  #               label = number_of_wlhiv_screened_positive_for_cervical_cancer), size = 12/.pt, hjust = 0.4,
  #           vjust = -0.5, 
  #           family = "Source Sans Pro Light") +
  geom_text(aes(y = number_of_wlhiv_screened_positive_for_cervical_cancer,
                label = number_of_wlhiv_screened_positive_for_cervical_cancer), size = 12/.pt, 
            hjust = 0.7,
            vjust = -0.5,
            position = position_nudge(x = -nudge_space*0.5),
            family = "Source Sans Pro Light") +
  geom_text(aes(y = the_number_of_women_who_are_on_treatment,
                label = the_number_of_women_who_are_on_treatment), size = 12/.pt, 
            hjust = -0.05,
            vjust = -0.5,
            position = position_nudge(x = nudge_space*0.5),
            family = "Source Sans Pro Light") +
  scale_y_continuous(limits = c(0, 140)) +
  labs(x = NULL, y = NULL)

ggsave(plot=cc_results, filename = paste0("Graphics/01_cc_right.png"), width = 9, height = 3)



screened <- cc |> ggplot(aes(x = period)) +
  geom_col(aes(y = number_of_wlhiv_screened_for_cervical_cancer), fill = trolley_grey,
           width = 0.4) +
  si_style_ygrid() +
  geom_text(aes(y = number_of_wlhiv_screened_for_cervical_cancer,
                label = number_of_wlhiv_screened_for_cervical_cancer), size = 12/.pt, 
            # hjust = 0.4,
            vjust = -0.5,
            family = "Source Sans Pro Light") +
  labs(x = NULL, y = NULL) + 
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(limits = c(0, 3500))
  


screened

ggsave(plot=screened, filename = paste0("Graphics/01_cc_left.png"), width = 5, height = 3)

glimpse(cc)
lines <- cc |>  ggplot(aes(period)) +
  geom_blank(aes(y = 1.1 * pct_rx)) +
  geom_line(aes(y = pct_rx, group = NA,
                color = "#2057a7"),size = 1.5) +
  geom_point(aes(y = pct_rx,
                 color = "#2057a7", fill = "#2057a7"), shape = 21, size = 15, stroke = 2) +
  geom_text(aes(y = pct_rx,
                label = percent_on_rx), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  geom_line(aes(y = pct_via, group = NA,
                color = "#bfddff"),size = 1.5) +
  geom_point(aes(y = pct_via,
                 color = "#bfddff", fill = "#bfddff"), shape = 21, size = 12, stroke = 2) +
  geom_text(aes(y = pct_via,
                label = percent_via_positive_from_screened ),
            family = "Source Sans Pro",
            size = 12/.pt) +
  scale_y_continuous(labels = percent, limits = c(-0.03, 1.05)) +
  scale_fill_identity() +
  scale_color_identity() +
  expand_limits(y = .2) +
  si_style_void() +
  labs(x = NULL, y = NULL)

lines

ggsave(plot=lines, filename = paste0("Graphics/01_cc_right_bottom.png"), width = 9, height = 3)






# ICT PLOT -------------------------------------------------------------------
ict <- read_csv(file = str_c("Data/",files[6]), ) |> clean_names() |> 
  rename(period = fyq,
         percent_yield = yield
  ) |> 
  mutate(pct_yield = positives / ict_case_traced_in_the_com_and_tested_at_com  ,
         pct_linkage = number_linked/ positives,
         period=str_remove(period, "\\s")) |> print()


ict_results <- ict |> ggplot(aes(x = period)) +
  geom_blank(aes(y = 1.1 * positives)) +
  # geom_col(aes(y = number_of_wlhiv_screened_for_cervical_cancer), fill = trolley_grey_light,
  #          width = 0.4) +
  geom_col(aes(y = positives ), fill = moody_blue_light, width = 0.4, 
           position = position_nudge(x = -nudge_space*0.6)) + 
  geom_col(aes(y = number_linked), fill = moody_blue, width = 0.4, 
           position = position_nudge(x = nudge_space*0.6)) + 
  si_style_ygrid() +
  # geom_text(aes(y = number_of_wlhiv_screened_positive_for_cervical_cancer,
  #               label = number_of_wlhiv_screened_positive_for_cervical_cancer), size = 12/.pt, hjust = 0.4,
  #           vjust = -0.5, 
  #           family = "Source Sans Pro Light") +
  geom_text(aes(y = positives,
                label = scales::comma(positives)), size = 12/.pt, 
            hjust = 0.8,
            vjust = -0.5,
            position = position_nudge(x = -nudge_space*0.5),
            family = "Source Sans Pro Light") +
  geom_text(aes(y = number_linked,
                label = scales::comma(number_linked)), size = 12/.pt, 
            hjust = .2,
            vjust = -0.5,
            position = position_nudge(x = nudge_space*0.5),
            family = "Source Sans Pro Light") +
  # scale_y_continuous(limits = c(0, 140)) +
  labs(x = NULL, y = NULL)

ict_results

ggsave(plot=ict_results, filename = paste0("Graphics/01_ict_right.png"), width = 9, height = 3)



tested <- ict |> ggplot(aes(x = period)) +
  geom_blank(aes(y = 1.1 * ict_case_traced_in_the_com_and_tested_at_com)) +
  geom_line(aes(y = ict_case_traced_in_the_com_and_tested_at_com, group=1), color = grey60k, size=1) +
  si_style_nolines() +
  geom_text(aes(y = ict_case_traced_in_the_com_and_tested_at_com,
                label = scales::comma(ict_case_traced_in_the_com_and_tested_at_com)), size = 12/.pt, 
            hjust = -0.3,
            vjust = -0.5,
            family = "Source Sans Pro Light") +
  labs(x = NULL, y = NULL) + 
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


tested

ggsave(plot=tested, filename = paste0("Graphics/01_ict_left.png"), width = 5, height = 3)


ict_lines_top <- ict |>  ggplot(aes(period)) +
  geom_blank(aes(y = 1.1 * pct_linkage)) +
  geom_line(aes(y = pct_linkage, group = NA,
                color = moody_blue),size = 1.5) +
  geom_point(aes(y = pct_linkage,
                 color = moody_blue, fill = moody_blue), shape = 21, size = 12, stroke = 2) +
  geom_text(aes(y = pct_linkage,
                label = percent_linkage ), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  scale_y_continuous(limits = c(0.97, 1.01)) +
  scale_fill_identity() +
  scale_color_identity() +
  expand_limits(y = .2) +
  si_style_void() +
  labs(x = NULL, y = NULL)

ict_lines_top

ggsave(plot=ict_lines_top, filename = paste0("Graphics/01_ict_right_bottom1.png"), width = 9, height = 1)


ict_lines_bottom <- ict |>  ggplot(aes(period)) +
  geom_line(aes(y = pct_yield , group = NA,
                color = moody_blue_light),size = 1.5) +
  geom_point(aes(y = pct_yield ,
                 color = moody_blue_light, fill = moody_blue_light), shape = 21, size = 12, stroke = 2) +
  geom_text(aes(y = pct_yield ,
                label = percent_yield  ),
            family = "Source Sans Pro",
            size = 12/.pt) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(limits = c(0.05, .14)) +
  si_style_void() +
  labs(x = NULL, y = NULL)


ict_lines_bottom

ggsave(plot=ict_lines_bottom, filename = paste0("Graphics/01_ict_right_bottom2.png"), width = 9, height = 2)

