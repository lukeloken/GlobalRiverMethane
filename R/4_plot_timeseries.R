

library(tidyverse)
library(ggplot2)
library(lubridate)

load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))


conc_ts <- conc_df %>%
  filter(!is.na(CH4mean)) %>%
  group_by(Site_Nid) %>%
  mutate(n_means = n(),
         Month = month(SampleDatestart)) %>%
  filter(n_means >= 20) %>%
  mutate(Month = factor(Month, 1:12), 
         Season_date = case_when(Month %in% c(1,2,12) ~ "Winter", 
                               Month %in% c(3:5) ~ "Spring", 
                               Month %in% c(6:8) ~ "Summer", 
                               Month %in% c(9:11) ~ "Autumn", 
         ), 
         Season_date = factor(Season_date, c("Winter", "Spring", 
                                             "Summer", "Autumn"))) %>%
  left_join(select(sites_df, Site_Nid, Latitude)) %>%
  mutate(Latitude = round(Latitude, 1), 
         Date2020 = as.Date(paste0("2020-", month(SampleDatestart), "-", day(SampleDatestart))))
  
site_arranged <- conc_ts %>%
  select(Site_Nid, Latitude)  %>%
  distinct() %>%
  arrange(desc(Latitude))

site_labels <- as.character(site_arranged$Latitude)
names(site_labels) <- as.character(site_arranged$Site_Nid)

conc_ts <- conc_ts %>%
  mutate(Site_Nid = factor(Site_Nid, site_arranged$Site_Nid))

season_colors <- c("blue4", "springgreen3", "Red", "goldenrod1")

CH4conc_ts_bysite <- ggplot(conc_ts, aes(x = SampleDatestart, y = CH4mean, col = Season_date)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~Site_Nid, scales = "free", 
             labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_manual(values = season_colors) + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 6), 
        axis.text = element_text(size = 6), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank()) +
  labs(y = expression(paste(CH[4], " (", mu, "M)"))) +
  ggtitle(expression(paste(CH[4], " concentrations at 20+ timepoints (title = latitude)"))) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = c(0.95, -0.03),
        legend.justification = c(1, 0), 
        legend.title.align = 0.5, 
        legend.box = "horizontal")

print(CH4conc_ts_bysite)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_20plus_sites_allyears.png"), 
       CH4conc_ts_bysite, 
       height = 10, width = 18)

CH4conc_ts_bysite_v2 <- ggplot(conc_ts, aes(x = Date2020, y = CH4mean, col = Season_date)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~Site_Nid, scales = "free_y", 
             labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_manual(values = season_colors) + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 6), 
        axis.text = element_text(size = 6), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank()) +
  labs(y = expression(paste(CH[4], " (", mu, "M)"))) +
  ggtitle(expression(paste(CH[4], " concentrations at 20+ timepoints (title = latitude)"))) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = c(0.95, -0.03),
        legend.justification = c(1, 0), 
        legend.title.align = 0.5, 
        legend.box = "horizontal") +
  scale_x_date(date_labels = "%b-%d")

print(CH4conc_ts_bysite_v2)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_20plus_sites_annual.png"), 
       CH4conc_ts_bysite_v2, 
       height = 10, width = 18)


my_breaks = 10^seq(-6, 9)
CH4conc_ts_v3 <- ggplot(conc_ts, aes(x = Date2020, y = as.factor(Latitude), col = CH4mean)) +
  geom_point() +
  # geom_vpline() + 
  # scale_y_log10() + 
  # facet_wrap(~Site_Nid, scales = "free_y", 
             # labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_distiller(palette = "YlOrBr", breaks = my_breaks, trans = "log", direction = 1) + 
  theme(axis.title.x = element_blank()) +
  labs(y = "Latitude")  + 
  # guides(color = guide_legend(nrow = 1)) +
  # theme(legend.position = c(0.95, -0.03),
  #       legend.justification = c(1, 0), 
  #       legend.title.align = 0.5, 
  #       legend.box = "horizontal") +
  # scale_x_date(date_labels = "%Y", date_breaks = "1 years") 
  scale_x_date(date_labels = "%b-%d")

print(CH4conc_ts_v3)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_20plus_sites_v3.png"), 
       CH4conc_ts_v3, 
       height = 8, width = 6)


##### By Q
CH4conc_Q_bysite <- conc_ts %>% 
  filter(Q> 0) %>% 
  ggplot( aes(x = Q, y = CH4mean, col = Season_date)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~Site_Nid, scales = "free", 
             labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_manual(values = season_colors) + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 6), 
        axis.text = element_text(size = 6), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank()) +
  labs(y = expression(paste(CH[4], " (", mu, "M)"))) +
  ggtitle(expression(paste(CH[4], " concentrations at 20+ timepoints (title = latitude)"))) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = c(0.95, -0.03),
        legend.justification = c(1, 0), 
        legend.title.align = 0.5, 
        legend.box = "horizontal")

print(CH4conc_Q_bysite)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_Q.png"), 
       CH4conc_Q_bysite, 
       height = 10, width = 18)
